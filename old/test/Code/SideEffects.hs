module Code.SideEffects (
  sideEffectsTranspilationTestCases
) where

import qualified Data.Text                as T
import           Text.RawString.QQ

import qualified Juvix.Backends.Michelson as M
import           Types

sideEffectsTranspilationTestCases ∷ [TranspilationTestCase]
sideEffectsTranspilationTestCases = [send]

send ∷ TranspilationTestCase
send = TranspilationTestCase {
  testName    = "send",
  testHaskell = [r|
module Send where

import           Juvix.Lib

main ∷ (Hash, ()) → IO ((), ())
main (keyHash, ()) = do
  transferTokens () 1 (defaultAccount keyHash) ()
  return ((), ())
|],
  testInputs = []
  }
