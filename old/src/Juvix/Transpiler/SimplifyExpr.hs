module Juvix.Transpiler.SimplifyExpr (
  simplifyExpr
) where

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Foundation

import qualified Juvix.Backends.Michelson as M
import           Juvix.Core
import           Juvix.Core.CompilerTypes
import           Juvix.Transpiler.Utility

{-  Stage 2 : Expression Simplification

    Rewrite the intermediary expression form in ways more conducive to Michelson transformation.

    - Eliminate unnecessary/equivalent function calls, e.g. rewrite (\x → x) y ⇒ y.
    - Minimize requisite stack manipulation to reorder parameters, e.g. rewrite (\x y → f y x) to f (BuiltIn SwapUT)    -}

simplifyExpr ∷ Expr M.Type → CompilerM (Expr M.Type) M.Type
simplifyExpr expr = do
  let inner e = do
        one ← simplifyExpr' e
        two ← simplifyExpr' one
        if one == two then return two else inner two
  inner expr

-- TODO : Rewrite this so it's guaranteed to be correct under defined evaluation rules.

simplifyExpr' ∷ Expr M.Type → CompilerM (Expr M.Type) M.Type
simplifyExpr' expr = do
  let tellReturn ∷ Expr M.Type → CompilerM (Expr M.Type) M.Type
      tellReturn ret = tell [SimplifiedExpr expr ret] >> return ret
  case expr of

    Ann (App (BuiltIn "SeqIO") x) _ → do
      simplifyExpr' (App (BuiltIn "SeqIO") x)

    App (Ann (App (BuiltIn "BindIO") x) (M.LamT (M.LamT xTy _) _)) y → do
      -- Hmm. TODO
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (BindIO (Ann x xTy) y)

    {- Annotations (should be unchanged). -}
    Ann expr ty → do
      expr ← simplifyExpr' expr
      return (Ann expr ty)

    {- Literal casts. -}
    App (BuiltIn "tezFromInteger") (Lit (LInt i)) → do
      tellReturn (Lit (LTez i))
    App (BuiltIn "intFromInteger") (Lit (LInt i)) → do
      tellReturn (Lit (LInt i))
    App (BuiltIn "natFromInteger") (Lit (LInt i)) → do
      -- check > 0?
      tellReturn (Lit (LNat i))

    {- IO monad (specialized to prevent accidental optimization) -}
    App (BuiltIn "ReturnIO") x        → do
      x ← simplifyExpr' x
      return (ReturnIO x)
    App (App (BuiltIn "SeqIO") x) y   → do
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (SeqIO x y)
    App (App (BuiltIn "BindIO") x) y  → do
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (BindIO x y)

    Let var bind expr → do
      bind ← simplifyExpr' bind
      expr ← simplifyExpr' expr
      return (Let var bind expr)

    {- Unused variables. -}
    App (Lam a e) _ | not (uses a e) → do
      inner ← simplifyExpr' e
      tellReturn inner
    App (App (Lam a (Lam b e)) v) _ | not (uses b e) → do
      inner ← simplifyExpr (App (Lam a e) v)
      tellReturn inner

    App x y → do
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (App x y)

    Lam a (App e (Var b)) | a == b && not (uses a e) → do
      inner ← simplifyExpr' e
      tellReturn inner
    Lam a (Lam b (App (App e (Var c)) (Var d))) | a == d && b == c && not (uses a e) && not (uses b e) → do
      inner ← simplifyExpr' e
      return (App inner (BuiltIn "SwapUT"))

    Lam v e → do
      e ← simplifyExpr' e
      return (Lam v e)

    Case (Var v) _ (M.PairT _ _) [CaseOption _ [Just x, Just y] (App (App (BuiltIn "ConsPairUT") (Var y')) (Var x'))] | x' == x && y' == y → do
      tellReturn (App (App (BuiltIn "ConsPairUT") (App (BuiltIn "CdrUT") (App (BuiltIn "SwapUT") (App (BuiltIn "CarUT") (BuiltIn "DupUT"))))) (Var v))

    Case e b t os → do
      e ← simplifyExpr e
      os ← flip mapM os $ \case
        DefaultCase e → DefaultCase |<< simplifyExpr' e
        CaseOption d b e → CaseOption d b |<< simplifyExpr' e
      let bnw = case b of
                  Just b  → if any (uses b . caseExpr) os then Just b else Nothing
                  Nothing → Nothing
      return (Case e bnw t os)

    BindIO x y → do
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (BindIO x y)
    SeqIO x y → do
      x ← simplifyExpr' x
      y ← simplifyExpr' y
      return (SeqIO x y)
    ReturnIO x → do
      x ← simplifyExpr' x
      return (ReturnIO x)

    other                      → return other
