module Juvix.Backends.Michelson.Transpilation.Types where

import           Protolude

import           Juvix.Backends.Michelson.Lift
import           Juvix.Utility

data TranspilationError
  = NotYetImplemented
  | DidNotTypecheck TypecheckError

instance PrettyPrint TranspilationError where
  prettyPrintValue = \case
    NotYetImplemented -> "not yet implemented"
    DidNotTypecheck _ -> "did not typecheck"
