module Types where

import           Foundation
import qualified Prelude                  as P

import qualified Juvix.Backends.Michelson as M
import           Juvix.Core

data TranspilationTestCase
  = TranspilationTestCase {
    testName    ∷ P.String,
    testHaskell ∷ P.String,
    testInputs  ∷ [(DynamicValue, M.Tez, M.Timestamp, DynamicValue)]
  }
