module Juvix.Backends.Michelson.Transpilation.Types where

import           Protolude

import           Juvix.Backends.Michelson.Lift
import           Juvix.Utility

data TranspilationError
  = NotYetImplemented
  | InvalidInput Text
  | InternalFault Text
  | DidNotTypecheck TypecheckError

instance PrettyPrint TranspilationError where
  prettyPrintValue = \case
    NotYetImplemented -> "not yet implemented"
    InvalidInput m    -> "invalid input: " <> m
    InternalFault m   -> "internal transpilation fault: " <> m <> " - this is a *bug* in Juvix and should be reported"
    DidNotTypecheck _ -> "did not typecheck"
