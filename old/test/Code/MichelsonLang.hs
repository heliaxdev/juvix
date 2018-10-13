module Code.MichelsonLang (
  michelsonLangTranspilationTestCases
) where

import qualified Data.Text                as T
import           Text.RawString.QQ

import qualified Juvix.Backends.Michelson as M
import           Types

michelsonLangTranspilationTestCases ∷ [TranspilationTestCase]
michelsonLangTranspilationTestCases = [accounts, atleast]

accounts ∷ TranspilationTestCase
accounts = TranspilationTestCase {
  testName    = "accounts",
  testHaskell = [r|
module Accounts where

import           Juvix.Lib

data Action
  = Deposit   Key
  | Withdraw  Key Tez Signature

main ∷ (Action, Map Key Tez) → IO ((), Map Key Tez)
main (action, state) = do
  case action of
    Deposit key → do
      let previousBalance = fromMaybe 0 (mapGet key state)
          newBalance      = previousBalance + amount
          newState        = mapUpdate key (Just newBalance) state
      return ((), newState)
    Withdraw key tez signature → do
      let previousBalance = fromMaybe 0 (mapGet key state)
          newBalance = previousBalance - tez
      if newBalance >= 0 && checkSignature key (signature, tez) then do
        let newState = mapUpdate key (Just newBalance) state
        transferTokens () tez (defaultAccount (hashKey key)) ()
        return ((), newState)
      else fail
|],
  testInputs = []
  }

atleast ∷ TranspilationTestCase
atleast = TranspilationTestCase {
  testName    = "atleast",
  testHaskell = [r|
module AtLeast where

import           Juvix.Lib

main ∷ ((), Tez) → IO ((), Tez)
main ((), minimum) = if amount > minimum then return ((), minimum) else fail
|],
  testInputs = []
  }
