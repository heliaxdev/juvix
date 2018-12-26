module Juvix.Backends.Michelson.Transpilation.Types where

import           Control.Monad.Writer
import           Protolude

import           Juvix.Backends.Michelson.Lift
import           Juvix.Backends.Michelson.Typed
import qualified Juvix.Backends.Michelson.Untyped as U
import qualified Juvix.Lang                       as J
import           Juvix.Utility

data TranspilationError
  = NotYetImplemented Text
  | InvalidInput Text
  | InternalFault Text
  | DidNotTypecheck TypecheckError

data TranspilationLog
  = ExprToExpr J.Expr U.Expr
  | Optimized SomeExpr SomeExpr

instance PrettyPrint TranspilationError where
  prettyPrintValue = \case
    NotYetImplemented m -> "not yet implemented: " <> m
    InvalidInput m      -> "invalid input: " <> m
    InternalFault m     -> "internal transpilation fault: " <> m <> " - this is a bug in Juvix and should be reported"
    DidNotTypecheck e   -> "did not typecheck: " <> prettyPrintValue e

instance PrettyPrint TranspilationLog where
  prettyPrintValue = \case
    ExprToExpr a b    -> "expr to expr: " <> prettyPrintValue a <> " to " <> prettyPrintValue b
    Optimized a b     -> "optimized: " <> prettyPrintValue a <> " to " <> prettyPrintValue b
