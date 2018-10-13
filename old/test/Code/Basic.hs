module Code.Basic (
  basicTranspilationTestCases
) where

import qualified Data.Text                as T
import           Foundation               hiding (swap)
import           Text.RawString.QQ

import qualified Juvix.Backends.Michelson as M
import           Juvix.Core
import           Types

basicTranspilationTestCases ∷ [TranspilationTestCase]
basicTranspilationTestCases = [identityOne, identityTwo, identityThree, identityFour, swap, add, arithmetic]

identityOne ∷ TranspilationTestCase
identityOne = TranspilationTestCase {
  testName    = "identityOne",
  testHaskell = [r|
module Identity where

import           Juvix.Lib

main ∷ (String, ()) → (String, ())
main s = s
|],
  testInputs = [identityInput]
  }

identityTwo ∷ TranspilationTestCase
identityTwo = TranspilationTestCase {
  testName    = "identityTwo",
  testHaskell = [r|
module Identity where

import           Juvix.Lib

-- This identity example is *not* fully optimized by Juvix (yet).

main ∷ (String, ()) → (String, ())
main (str, ()) = (str, ())
|],
  testInputs = [identityInput]
  }

identityThree ∷ TranspilationTestCase
identityThree = TranspilationTestCase {
  testName    = "identityThree",
  testHaskell = [r|
module Identity where

import           Juvix.Lib

main ∷ (String, ()) → (String, ())
main = id

id ∷ a → a
id x = x
|],
  testInputs = [identityInput]
  }

identityFour ∷ TranspilationTestCase
identityFour = TranspilationTestCase {
  testName    = "identityFour",
  testHaskell = [r|
module Identity where

import           Juvix.Lib

main ∷ (String, ()) → (String, ())
main = func

func =
  let x ∷ Int
      x = 3
      y ∷ Int
      y = 4
      z = \x → \y → y x
  in \a → a
|],
  testInputs = [identityInput]
  }

swap ∷ TranspilationTestCase
swap = TranspilationTestCase {
  testName    = "swap",
  testHaskell = [r|
module Swap where

import           Juvix.Lib

main ∷ (String, String) → (String, String)
main (param, storage) = (storage, param)
|],
  testInputs = [(toDynamicValue (M.Pair someString anotherString), M.Tez 0, M.Timestamp 0, toDynamicValue (M.Pair anotherString someString))]
  }

add ∷ TranspilationTestCase
add = TranspilationTestCase {
  testName    = "add",
  testHaskell = [r|
module Add where

import           Juvix.Lib

main ∷ (Int, ()) → (Int, ())
main (x, ()) = (x + x, ())
|],
  testInputs = [(toDynamicValue (M.Pair (2 ∷ Integer) ()), M.Tez 0, M.Timestamp 0, toDynamicValue (M.Pair (4 ∷ Integer) ()))]
  }

arithmetic ∷ TranspilationTestCase
arithmetic = TranspilationTestCase {
  testName    = "arithmetic",
  testHaskell = [r|
module Arithmetic where

import           Juvix.Lib

main ∷ ((Int, Tez), ()) → ((Int, Tez), ())
main ((x, y), ()) = ((x * x, y + (2 ∷ Tez)), ())
|],
  testInputs = [(toDynamicValue (M.Pair (M.Pair (2 ∷ Integer) (M.Tez 3)) ()), M.Tez 0, M.Timestamp 0, toDynamicValue (M.Pair (M.Pair (4 ∷ Integer) (M.Tez 5)) ()))]
  }

someString ∷ T.Text
someString = "a"

anotherString ∷ T.Text
anotherString = "b"

identityInput ∷ (DynamicValue, M.Tez, M.Timestamp, DynamicValue)
identityInput = (toDynamicValue (M.Pair someString ()), M.Tez 0, M.Timestamp 0, toDynamicValue (M.Pair someString ()))
