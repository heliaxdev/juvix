module Juvix.Backends.Michelson.Transpilation.Expr where

import           Control.Monad.Writer
import qualified Data.Text                                    as T
import           Protolude

import           Juvix.Backends.Michelson.Transpilation.Types
import qualified Juvix.Backends.Michelson.Untyped             as M
import           Juvix.Lang
import           Juvix.Utility

import qualified Idris.Core.TT                                as I
import qualified IRTS.Lang                                    as I

exprToMichelson ∷ ∀ m . (MonadWriter [TranspilationLog] m, MonadError TranspilationError m) ⇒ Expr → m (M.Expr, M.Type)
exprToMichelson expr = (,) <$> exprToExpr expr <*> exprToType expr

{-
 - Transform core expression to Michelson instruction sequence.
 - This requires tracking the stack state.
 - At the moment, this functions maintains a forward mapping from the Haskell expression type and the Michelson stack type.
 - ∷ { Haskell Type } ~ { Stack Pre-Evaluation } ⇒ { Stack Post-Evaluation }
 -}

exprToExpr ∷ ∀ m . (MonadWriter [TranspilationLog] m, MonadError TranspilationError m) ⇒ Expr → m M.Expr
exprToExpr expr = do

  let tellReturn ∷ M.Expr → m M.Expr
      tellReturn ret = tell [ExprToExpr expr ret] >> return ret

      notYetImplemented ∷ m M.Expr
      notYetImplemented = throw (NotYetImplemented $ prettyPrintValue expr)

      failWith ∷ Text → m M.Expr
      failWith = throw . NotYetImplemented

  case expr of

    -- :: a ~ s => (a, s)
    I.LV name            -> notYetImplemented

    -- ∷ (\a → b) a ~ s ⇒ (b, s)
    I.LApp _ func args   -> notYetImplemented

    -- ∷ (\a → b) a ~ s ⇒ (b, s)
    I.LLazyApp func args -> notYetImplemented

    -- :: a ~ s => (a, s)
    I.LLazyExp expr      -> notYetImplemented

    -- :: a ~ s => (a, s)
    I.LForce expr        -> notYetImplemented

    -- :: a ~ s => (a, s)
    I.LLet name val expr -> notYetImplemented

    -- ∷ \a... → b ~ (a..., s) ⇒ (b, s)
    I.LLam args body     -> notYetImplemented

    -- ??
    I.LProj expr num     -> notYetImplemented

    -- need to typeconvert
    I.LCon _ _ name args -> notYetImplemented

    -- ∷ a ~ s ⇒ (a, s)
    I.LCase ct expr alts -> notYetImplemented

    -- ∷ a ~ s ⇒ (a, s)
    I.LConst const       -> notYetImplemented

    -- ??
    I.LForeign _ _ _     -> notYetImplemented

    -- (various)
    I.LOp prim exprs     -> notYetImplemented

    I.LNothing           -> notYetImplemented

    I.LError msg         -> failWith (T.pack msg)

exprToType ∷ ∀ m . (MonadWriter [TranspilationLog] m, MonadError TranspilationError m) ⇒ Expr → m M.Type
exprToType expr = do
  return (M.LamT (M.PairT M.StringT M.StringT) (M.PairT M.StringT M.StringT))

primToExpr ∷ ∀ m . (MonadWriter [TranspilationLog] m, MonadError TranspilationError m) ⇒ Prim → m M.Expr
primToExpr prim = do
  let notYetImplemented ∷ m M.Expr
      notYetImplemented = throw (NotYetImplemented $ prettyPrintValue prim)

  case prim of
    I.LPlus (I.ATInt I.ITBig)  -> return M.AddIntInt
    I.LMinus (I.ATInt I.ITBig) -> return M.SubInt

    _                          -> notYetImplemented
