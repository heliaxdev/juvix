module Types where

import           Protolude

import qualified Juvix.Backends as J
import qualified Juvix.Utility  as J

data CompilationTestCase
  = CompilationTestCase {
    testName   ∷ Text,
    testIdris  ∷ Text,
    testInputs ∷ [(J.DynamicValue, J.Tez, J.Timestamp, J.DynamicValue)]
  }
