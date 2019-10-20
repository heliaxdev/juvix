module Juvix.Core.IR.Typechecker where

import Control.Lens ((^?), ix)
import Control.Monad.Except (throwError)
import Juvix.Core.IR.Types
import Juvix.Core.Usage
import Juvix.Library hiding (show)
import Prelude (Show (..), String, error, lookup)

-- initial environment
initEnv ∷ Env primTy primVal
initEnv = []

cEval ∷ (Show primTy, Show primVal) ⇒ CTerm primTy primVal → Env primTy primVal → Value primTy primVal
cEval (Star i) _d = VStar i
cEval (PrimTy p) _d = VPrimTy p
cEval (Pi pi ty ty') d = VPi pi (cEval ty d) (\x → cEval ty' (x : d))
cEval (Lam e) d = VLam (\x → cEval e (x : d))
cEval (Conv ii) d = iEval ii d

toInt ∷ Natural → Int
toInt = fromInteger . toInteger

-- TODO ∷ Promote iEval and cEval into the maybe monad and all call sites
iEval ∷ (Show primTy, Show primVal) ⇒ ITerm primTy primVal → Env primTy primVal → Value primTy primVal
iEval (Free x) _d = vfree x
iEval (Prim p) _d = VPrim p
iEval (App iterm cterm) d = vapp (iEval iterm d) (cEval cterm d)
iEval (Ann _pi term _type) d = cEval term d
iEval (Bound ii) d =
  case d ^? ix (toInt ii) of
    Just x → x
    Nothing → error ("unbound index " <> show ii)

vapp ∷ (Show primTy, Show primVal) ⇒ Value primTy primVal → Value primTy primVal → Value primTy primVal
vapp (VLam f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)
vapp x y =
  error
    ( "Application (vapp) error. Cannot apply \n"
        <> show y
        <> "\n to \n"
        <> show x
    )

-- substitution function for checkable terms
cSubst ∷ (Show primTy, Show primVal) ⇒ Natural → ITerm primTy primVal → CTerm primTy primVal → CTerm primTy primVal
cSubst _ii _r (Star i) = Star i
cSubst _ii _r (PrimTy p) = PrimTy p
cSubst ii r (Pi pi ty ty') = Pi pi (cSubst ii r ty) (cSubst (ii + 1) r ty')
cSubst ii r (Lam f) = Lam (cSubst (ii + 1) r f)
cSubst ii r (Conv e) = Conv (iSubst ii r e)

-- substitution function for inferable terms
iSubst ∷ (Show primTy, Show primVal) ⇒ Natural → ITerm primTy primVal → ITerm primTy primVal → ITerm primTy primVal
iSubst ii r (Bound j)
  | ii == j = r
  | otherwise = Bound j
iSubst _ii _r (Free y) = Free y
iSubst _ii _r (Prim p) = Prim p
iSubst ii r (App it ct) = App (iSubst ii r it) (cSubst ii r ct)
iSubst ii r (Ann pi term t) = Ann pi (cSubst ii r term) (cSubst ii r t)

-- error message for inferring/checking types
errorMsg ∷ (Show primTy, Show primVal) ⇒ Natural → CTerm primTy primVal → Annotation primTy primVal → Annotation primTy primVal → String
errorMsg binder cterm expectedT gotT =
  "Type mismatched. \n"
    <> show cterm
    <> " \n (binder number "
    <> show binder
    <> ") is of type \n"
    <> show (show (snd gotT))
    <> " , with "
    <> show (fst gotT)
    <> " usage.\n But the expected type is "
    <> show (show (snd expectedT))
    <> " , with "
    <> show (fst expectedT)
    <> " usage."

-- Type (and usage) checking
type Result a = Either String a --when type checking fails, it throws an error.

-- | 'checker' for checkable terms checks the term against an annotation and returns ().
cType ∷ (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒ Natural → Context primTy primVal → CTerm primTy primVal → Annotation primTy primVal → Result ()
cType _ii _g t@(Star n) ann = do
  unless (SNat 0 == fst ann) (throwError "Sigma has to be 0.") -- checks sigma = 0.
  let ty = snd ann
  case ty of
    VStar j →
      unless
        (n < j)
        ( throwError $
            show t
              <> " is of type * of a higher universe. But the expected type "
              <> show (snd ann)
              <> " is * of a equal or lower universe."
        )
    _ → throwError $ "* n is of type * but " <> show (snd ann) <> " is not *."
{-
cType ii _g Nats ann =
  unless
    (SNat 0 == fst ann && quote0 (snd ann) == Star 0)
    (throwError (errorMsg ii Nats (zero, VStar 0) ann))
-}
cType ii g (Pi pi varType resultType) ann = do
  unless (SNat 0 == fst ann) (throwError "Sigma has to be 0.") -- checks sigma = 0.
  let ty = snd ann
  case ty of
    VStar _ → do
      cType ii g varType ann -- checks varType is of type Star i
      let ty = cEval varType []
      cType -- checks resultType is of type Star i
        (ii + 1)
        ((Local ii, (pi, ty)) : g)
        (cSubst 0 (Free (Local ii)) resultType)
        ann
    _ →
      throwError
        "The variable type and the result type must be of type * at the same level."
-- (Lam) introduction rule of dependent function type
cType ii g (Lam s) ann =
  case ann of
    (sig, VPi pi ty ty') →
      -- Lam s should be of dependent function type (Pi pi ty ty').
      cType
        (ii + 1)
        ((Local ii, (sig <.> pi, ty)) : g) -- put s in the context with usage sig*pi
        (cSubst 0 (Free (Local ii)) s) -- x (varType) in context S with sigma*pi usage.
        (sig, ty' (vfree (Local ii))) -- is of type M (usage sigma) in context T
    _ → throwError $ show (snd ann) <> " is not a function type but should be."
cType ii g (Conv e) ann = do
  ann' ← iType ii g e
  unless
    (fst ann == fst ann' && quote0 (snd ann) == quote0 (snd ann'))
    (throwError (errorMsg ii (Conv e) ann ann'))

-- inferable terms have type as output.
iType0 ∷ (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒ Context primTy primVal → ITerm primTy primVal → Result (Annotation primTy primVal)
iType0 = iType 0

iTypeErrorMsg ∷ Natural → Name → String
iTypeErrorMsg ii x =
  "Cannot find the type of \n"
    <> show x
    <> "\n (binder number "
    <> show ii
    <> ") in the environment."

iType ∷ (Show primTy, Show primVal, Eq primTy, Eq primVal) ⇒ Natural → Context primTy primVal → ITerm primTy primVal → Result (Annotation primTy primVal)
-- the type checker should never encounter a bound variable (as in LambdaPi)? To be confirmed.
iType _ii _g (Bound _) = error "Bound variable cannot be inferred"
iType ii g (Free x) =
  case lookup x g of
    Just ann → return ann
    Nothing → throwError (iTypeErrorMsg ii x)
-- Prim-Const typing rule
{-
iType _ii _g (Nat _) = return (Omega, VNats)
-}
-- App rule, function M applies to N
iType ii g (App m n) = do
  mTy ← iType ii g m -- annotation of M is usage sig and Pi with pi usage.
  case mTy of
    (sig, VPi pi varTy resultTy) → do
      cType ii g n (pi, varTy) -- N has to be of type varTy with usage pi
      return (sig, resultTy (cEval n []))
    _ →
      throwError
        ( show m
            <> "\n (binder number "
            <> show ii
            <> ") is not a function type and thus \n"
            <> show n
            <> "\n cannot be applied to it."
        )
iType ii g (Ann pi theTerm theType) =
  -- TODO check theType is of type Star first? But we have stakable universes now.
  -- cType ii g theType (0, VStar 0)
  do
    let ty = cEval theType []
    cType ii g theTerm (pi, ty)
    return (pi, ty)
