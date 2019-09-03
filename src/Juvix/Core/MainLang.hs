-- Quantitative type implementation inspired by
-- Atkey 2017 and McBride 2016.
module Juvix.Core.MainLang where

import           Control.Monad.Except
import           Numeric.Natural
import           Prelude

data NatAndw -- semiring of (Nat,w) for usage annotation
  = SNat Natural -- 0, 1, or n usage
  | Omega -- unspecified usage

instance Show NatAndw where
  show (SNat n) = show n
  show Omega    = "w"

instance Eq NatAndw where
  SNat x == SNat y = x == y
  SNat _ == Omega = True
  Omega == _ = True

instance Num NatAndw where
  SNat x + SNat y = SNat (x + y)
  Omega + _ = Omega
  _ + Omega = Omega
  SNat j * SNat k = SNat (j * k)
  Omega * _ = Omega
  _ * Omega = Omega
  fromInteger x = SNat (fromInteger x)

-- checkable terms
data CTerm
  = Star Natural -- (sort i) i th ordering of (closed) universe.
  | Nats -- (Prim) primitive type (naturals)
  | Pi NatAndw CTerm CTerm -- formation rule of the dependent function type (PI).
                              -- the NatAndw(π) tracks how many times x is used.
  | Pm NatAndw CTerm CTerm -- dependent multiplicative conjunction (tensor product)
  | Pa NatAndw CTerm CTerm -- dependent additive conjunction type
  | NPm CTerm CTerm -- non-dependent multiplicative disjunction type
  | Lam NatAndw CTerm --(LAM) Introduction rule of PI.
                        -- The abstracted variable's usage is tracked with the NatAndw(π).
  | Conv ITerm --(CONV) conversion rule. TODO make sure 0Γ ⊢ S≡T
              -- Conv is the constructor that embeds ITerm to CTerm
  deriving (Show, Eq)

-- inferable terms
data ITerm
  = Bound Natural -- Bound variables, in de Bruijn indices
  | Free Name -- Free variables of type name (see below)
  | Nat Natural -- primitive constant (naturals)
  | App ITerm CTerm -- elimination rule of PI (APP).
                              -- the NatAndw(π) tracks how x is use.
  | Ann NatAndw CTerm CTerm --Annotation with usage.
  deriving (Show, Eq)

data Name
  = Global String -- Global variables are represented by name thus type string
  | Local Natural -- to convert a bound variable into a free one
  | Quote Natural
  deriving (Show, Eq)

--Values/types
data Value
  = VStar Natural
  | VNats
  | VPi NatAndw Value (Value -> Value)
  | VPm NatAndw Value (Value -> Value)
  | VPa NatAndw Value (Value -> Value)
  | VNPm Value Value
  | VLam NatAndw (Value -> Value)
  | VNeutral Neutral
  | VNat Natural

showVal :: Value -> String
showVal (VLam _ f) = showFun f
showVal (VStar i) = "*" ++ show i
showVal VNats = "Nats"
showVal (VPi n v f) = "[" ++ show n ++ "]" ++ showVal v ++ " -> " ++ showFun f
showVal (VPm n v f) =
  "([" ++ show n ++ "]" ++ showVal v ++ ", " ++ showFun f ++ ")"
showVal (VPa _ _ _) = "/\\"
showVal (VNPm _ _) = "\\/"
showVal (VNeutral _n) = "neutral "
showVal (VNat i) = show i

showFun :: (Value -> Value) -> String
showFun _f = "\\x.t"

--A neutral term is either a variable or an application of a neutral term to a value
data Neutral
  = NFree Name
  | NApp Neutral Value

--vfree creates the value corresponding to a free variable
vfree :: Name -> Value
vfree n = VNeutral (NFree n)

--Annotations include usage and type.
type Annotation = (NatAndw, Value)

--Contexts map variables to their types.
type Context = [(Name, Annotation)]

--Evaluation
type Env = [Value]

cEval :: CTerm -> Env -> Value
cEval (Star i) _d      = VStar i
cEval Nats _d          = VNats
cEval (Pi pi ty ty') d = VPi pi (cEval ty d) (\x -> cEval ty' (x : d))
cEval (Pm pi ty ty') d = VPm pi (cEval ty d) (\x -> cEval ty' (x : d))
cEval (Pa pi ty ty') d = VPa pi (cEval ty d) (\x -> cEval ty' (x : d))
cEval (NPm ty ty') d   = VNPm (cEval ty d) (cEval ty' d)
cEval (Lam pi e) d     = VLam pi (\x -> cEval e (x : d))
cEval (Conv ii) d      = iEval ii d

toInt :: Natural -> Int
toInt = fromInteger . toInteger

iEval :: ITerm -> Env -> Value
iEval (Free x) _d            = vfree x
iEval (Bound ii) d           = d !! toInt ii --(!!) :: [a] -> Int -> a, the list lookup operator.
iEval (Nat n) _d             = VNat n
iEval (App iterm cterm) d    = vapp (iEval iterm d) (cEval cterm d)
iEval (Ann _pi term _type) d = cEval term d

vapp :: Value -> Value -> Value
vapp (VLam _pi f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)
vapp x y =
  error
    ("Application (vapp) error. Cannot apply \n" ++
     showVal y ++ "\n to \n" ++ showVal x)

--substitution function for checkable terms
cSubst :: Natural -> ITerm -> CTerm -> CTerm
cSubst _ii _r (Star i)     = Star i
cSubst _ii _r Nats         = Nats
cSubst ii r (Pi pi ty ty') = Pi pi (cSubst ii r ty) (cSubst (ii + 1) r ty')
cSubst ii r (Pm pi ty ty') = Pm pi (cSubst ii r ty) (cSubst (ii + 1) r ty')
cSubst ii r (Pa pi ty ty') = Pa pi (cSubst ii r ty) (cSubst (ii + 1) r ty')
cSubst ii r (NPm fst snd)  = NPm (cSubst ii r fst) (cSubst ii r snd)
cSubst ii r (Lam pi f)     = Lam pi (cSubst (ii + 1) r f)
cSubst ii r (Conv e)       = Conv (iSubst ii r e)

--substitution function for inferable terms
iSubst :: Natural -> ITerm -> ITerm -> ITerm
iSubst ii r (Bound j)
  | ii == j = r
  | otherwise = Bound j
iSubst _ii _r (Free y) = Free y
iSubst _ii _r (Nat n) = Nat n
iSubst ii r (App it ct) = App (iSubst ii r it) (cSubst ii r ct)
iSubst ii r (Ann pi term t) = Ann pi (cSubst ii r term) (cSubst ii r t)

--iSubst ii r (App iterm cterm) =
--Quotation: takes a value back to a term
quote0 :: Value -> CTerm
quote0 = quote 0

quote :: Natural -> Value -> CTerm
quote _ii (VStar n) = Star n
quote _ii VNats = Nats
quote ii (VPi pi v f) =
  Pi pi (quote ii v) (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VPm pi fst snd) =
  Pm pi (quote ii fst) (quote (ii + 1) (snd (vfree (Quote ii))))
quote ii (VPa pi fst snd) =
  Pa pi (quote ii fst) (quote (ii + 1) (snd (vfree (Quote ii))))
quote ii (VNPm fst snd) = NPm (quote ii fst) (quote ii snd)
quote ii (VLam pi f) = Lam pi (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VNeutral n) = Conv (neutralQuote ii n)
quote _ii (VNat n) = Conv (Nat n)

neutralQuote :: Natural -> Neutral -> ITerm
neutralQuote ii (NFree x)  = boundfree ii x
neutralQuote ii (NApp n v) = App (neutralQuote ii n) (quote ii v)

--checks if the variable occurring at the head of the application is a bound variable or a free name
boundfree :: Natural -> Name -> ITerm
boundfree ii (Quote k) = Bound (ii - k - 1)
boundfree _ii x        = Free x

--error message for inferring/checking types
errorMsg :: Natural -> CTerm -> Annotation -> Annotation -> String
errorMsg binder iterm expectedT gotT =
  "Type mismatched. \n" ++
  show iterm ++
  " \n (binder number " ++
  show binder ++
  ") is of type \n" ++
  show (showVal (snd gotT)) ++
  " , with " ++
  show (fst gotT) ++
  " usage.\n But the expected type is " ++
  show (showVal (snd expectedT)) ++
  " , with " ++ show (fst expectedT) ++ " usage."

--Type (and usage) checking
type Result a = Either String a --when type checking fails, it throws an error.

--checkable terms take a type as input and returns ().
cType :: Natural -> Context -> CTerm -> Annotation -> Result ()
cType ii _g (Star n) v =
  unless
    (fst v == 0 && quote0 (snd v) == Star (n + 1))
    (throwError (errorMsg ii (Star n) (0, VStar (n + 1)) v))
cType ii _g Nats v =
  unless
    (fst v == 0 && quote0 (snd v) == Star 0)
    (throwError (errorMsg ii Nats (0, VStar 0) v))
cType ii g (Conv e) v = do
  v' <- iType ii g e
  unless
    (fst v == fst v' && quote0 (snd v) == quote0 (snd v'))
    (throwError (errorMsg ii (Conv e) v v'))
cType ii g (Lam pi f) (pi', VPi _pi ty ty') =
  cType
    (ii + 1)
    ((Local ii, (pi, ty)) : g)
    (cSubst 0 (Free (Local ii)) f)
    (pi', ty' (vfree (Local ii)))
cType ii _g cterm theType =
  throwError
    ("Type mismatch: \n" ++
     show cterm ++
     "\n (binder number " ++
     show ii ++
     ") is not a checkable term. Cannot check that it is of type " ++
     showVal (snd theType) ++ " with " ++ show (fst theType) ++ "usage.")

--inferable terms have type as output.
iType0 :: Context -> ITerm -> Result Annotation
iType0 = iType 0

iType :: Natural -> Context -> ITerm -> Result Annotation
iType ii g (Free x) =
  case lookup x g of
    Just ty -> return ty
    Nothing ->
      throwError
        ("Cannot find the type of \n" ++
         show x ++ "\n (binder number " ++ show ii ++ ") in the environment.")
