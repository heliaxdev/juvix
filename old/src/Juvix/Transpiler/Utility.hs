module Juvix.Transpiler.Utility where

import           Control.Monad.RWS.Strict
import           Data.List                    (elemIndex, foldl, replicate)
import qualified Data.Text                    as T
import           Foundation                   hiding (replicate)

import qualified Juvix.Backends.Michelson     as M
import           Juvix.Core
import           Juvix.Core.CompilerTypes
import qualified Juvix.Core.GHC               as GHC
import           Juvix.Transpiler.PrettyPrint ()

tyApp ∷ GHC.CoreExpr → GHC.Type → GHC.CoreExpr
tyApp (GHC.Var v) ty =
  let varType = GHC.varType v
  in GHC.Var (GHC.setVarType v (tyAppSubst varType ty))
tyApp expr _ = expr

tyAppSubst ∷ GHC.Type → GHC.Type → GHC.Type
tyAppSubst ty var =
  case ty of
    GHC.ForAllTy (GHC.TvBndr x _) y → substTy x var y
    other                           → other

substTy ∷ GHC.TyVar → GHC.Type → GHC.Type → GHC.Type
substTy source target inType =
  GHC.substTyWith [source] [target] inType

{-  Whether an expression uses the value of a variable. Assumes uniqueness of names.   -}

uses ∷ T.Text → Expr M.Type → Bool
uses var expr =
  case expr of
    BuiltIn _     → False
    Lit _         → False
    Var v         → v == var
    Let _ x y     → uses var x || uses var y
    App x y       → uses var x || uses var y
    Lam _ x       → uses var x
    Case s _ _ cs → uses var s || any (uses var . caseExpr) cs
    BindIO x y    → uses var x || uses var y
    SeqIO x y     → uses var x || uses var y
    ReturnIO x    → uses var x
    Ann e _       → uses var e

caseExpr ∷ CaseOption M.Type → Expr M.Type
caseExpr (DefaultCase e)    = e
caseExpr (CaseOption _ _ e) = e

rearrange ∷ Int → M.ExprUT
rearrange 0 = M.NopUT
rearrange 1 = M.SwapUT
rearrange n = M.SeqUT (M.DipUT (rearrange (n - 1))) M.SwapUT

unrearrange ∷ Int → M.ExprUT
unrearrange 0 = M.NopUT
unrearrange 1 = M.SwapUT
unrearrange n = M.SeqUT M.SwapUT (M.DipUT (unrearrange (n - 1)))

position ∷ T.Text → StackRep → Maybe Int
position n = elemIndex (BoundVariable n)

dropFirst ∷ T.Text → StackRep → StackRep
dropFirst n (x:xs) = if x == BoundVariable n then xs else x : dropFirst n xs
dropFirst _ []     = []

foldDrop ∷ Int → M.ExprUT
foldDrop 0 = M.NopUT
foldDrop n = M.DipUT (foldl M.SeqUT M.NopUT (replicate n M.DropUT))

unForAll ∷ GHC.Type → GHC.Type
unForAll (GHC.ForAllTy _ t) = unForAll t
unForAll ty                 = ty

typeApply ∷ GHC.TyVar → GHC.Type → GHC.Type → GHC.Type
typeApply v t =
  \case
    GHC.TyVarTy v' | v == v'                              → t
    GHC.AppTy x y                                         → GHC.AppTy (typeApply v t x) (typeApply v t y)
    GHC.TyConApp c ts                                     → GHC.TyConApp c (fmap (typeApply v t) ts)
    GHC.ForAllTy (GHC.TvBndr v' _) e | v == v'            → typeApply v t e
    GHC.CastTy ty c                                       → GHC.CastTy (typeApply v t ty) c
    other                                                 → other

genReturn ∷ M.ExprUT → CompilerM M.ExprUT M.Type
genReturn expr = do
  modify =<< genFunc expr
  return expr

genFunc ∷ M.ExprUT → CompilerM (StackRep → StackRep) M.Type
genFunc expr = if

  | expr `elem` [M.NopUT] → return id

  | expr `elem` [M.DropUT] → return (drop 1)
  | expr `elem` [M.DupUT] → return (\(x:xs) → x:x:xs)
  | expr `elem` [M.SwapUT] → return (\(x:y:xs) → y:x:xs)

  | expr `elem` [M.AmountUT] → return ((:) FuncResult)

  | expr `elem` [M.FailUT, M.LeftUT, M.RightUT, M.DefaultAccountUT, M.CarUT, M.CdrUT, M.EqUT, M.LeUT, M.NotUT] → return ((:) FuncResult . drop 1)

  | expr `elem` [M.MapGetUT, M.AddIntNatUT, M.AddNatIntUT, M.AddNatNatUT,  M.AddIntIntUT, M.AddTezUT, M.SubIntUT, M.SubTezUT, M.MulIntIntUT, M.ConsPairUT] → return ((:) FuncResult . drop 2)

  | expr `elem` [M.MapUpdateUT] → return ((:) FuncResult . drop 3)

  | otherwise →
      case expr of

        M.DipUT x → do
          f ← genFunc x
          return (\(x:xs) → x : f xs)

        M.SeqUT x y → do
          x ← genFunc x
          y ← genFunc y
          return (y . x)

        M.ConstUT _ → return ((:) (FuncResult))

        _           → throw (NotYetImplemented (T.concat ["genFunc: ", prettyPrintValue expr]))
