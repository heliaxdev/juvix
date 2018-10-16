module Juvix.Backends.Michelson.Transpilation.Types where

import           Protolude

import           Juvix.Backends.Michelson.Lift
import           Juvix.Backends.Michelson.Typed
import           Juvix.Utility

data TranspilationError
  = NotYetImplemented
  | InvalidInput Text
  | InternalFault Text
  | DidNotTypecheck TypecheckError

data TranspilationLog
  = Optimized SomeExpr SomeExpr

instance PrettyPrint TranspilationError where
  prettyPrintValue = \case
    NotYetImplemented -> "not yet implemented"
    InvalidInput m    -> "invalid input: " <> m
    InternalFault m   -> "internal transpilation fault: " <> m <> " - this is a *bug* in Juvix and should be reported"
    DidNotTypecheck _ -> "did not typecheck"

instance PrettyPrint TranspilationLog where
  prettyPrintValue = \case
    Optimized a b     -> "optimized " <> prettyPrintValue a <> " to " <> prettyPrintValue b
