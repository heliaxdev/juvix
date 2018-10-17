module Juvix.Backends.Michelson.Transpilation.Expr where

import           Control.Monad.Writer
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

  case expr of

    -- ∷ (\a → b) a ~ s ⇒ (b, s)
    I.LApp _ func args   -> notYetImplemented

    -- ∷ \a... → b ~ (a..., s) ⇒ (b, s)
    I.LLam args body     -> notYetImplemented

    -- what is this
    I.LProj expr num     -> notYetImplemented

    -- need to typeconvert
    I.LCon _ _ name args -> notYetImplemented

    -- ∷ a ~ s ⇒ (a, s)
    I.LCase ct expr alts -> notYetImplemented

    -- ∷ a ~ s ⇒ (a, s)
    I.LConst const       -> notYetImplemented

    -- (various)
    I.LOp prim exprs     -> notYetImplemented

    _                    -> notYetImplemented

exprToType ∷ ∀ m . (MonadWriter [TranspilationLog] m, MonadError TranspilationError m) ⇒ Expr → m M.Type
exprToType expr = do
  return (M.LamT (M.PairT M.StringT M.StringT) (M.PairT M.StringT M.StringT))

primToExpr ∷ ∀ m . (MonadWriter [TranspilationLog] m, MonadError TranspilationError m) ⇒ Prim → m M.Expr
primToExpr prim = throw (NotYetImplemented $ prettyPrintValue prim)
