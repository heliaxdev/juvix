module Code where

import           Protolude         hiding (identity)
import qualified Test.Tasty        as T
import           Text.RawString.QQ

import qualified Juvix.Backends    as J
import qualified Juvix.Utility     as J

import           Types

compilationTestCases ∷ [CompilationTestCase]
compilationTestCases = [
  identity
  ]

identity ∷ CompilationTestCase
identity = CompilationTestCase {
  testName    = "identity",
  testIdris   = [r|
module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : (String, String) -> (List Operation, String)
main (storage, _) = (Nil, storage)
|],
  testInputs = [identityInput]
}

someString ∷ Text
someString = "a"

identityInput ∷ (J.DynamicValue, J.Tez, J.Timestamp, J.DynamicValue)
identityInput = (J.toDynamicValue (J.Pair someString ()), J.Tez 0, J.Timestamp 0, J.toDynamicValue (J.Pair someString ()))
