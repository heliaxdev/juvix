module Code.Examples (
  examplesTranspilationTestCases
) where

import qualified Data.Text                as T
import           Text.RawString.QQ

import qualified Juvix.Backends.Michelson as M
import           Types

examplesTranspilationTestCases ∷ [TranspilationTestCase]
examplesTranspilationTestCases = [datapublisher, token]

datapublisher ∷ TranspilationTestCase
datapublisher = TranspilationTestCase {
  testName    = "datapublisher",
  testHaskell = [r|
{-# LANGUAGE ExplicitForAll #-}

module DataPublisher where

import           Juvix.Lib

data Action a
  = Retrieve
  | Update a Signature Nat

data Result a
  = Retrieved a
  | Updated

type DataPublisher a = (Action a, (a, Nat)) → IO (Result a, (a, Nat))

dataPublisher ∷ ∀ a . DataPublisher a
dataPublisher (action, storage@(value, nonce)) =
  case action of
    Retrieve → do
      if amount < 1 then fail else return (Retrieved value, storage)
    Update newValue signature newNonce → do
      if newNonce == nonce + (1 ∷ Nat) && checkSignature (manager source) (signature, (newValue, newNonce)) then
        return (Updated, (newValue, newNonce))
      else fail

{-# SPECIALISE dataPublisher :: DataPublisher String #-}

main ∷ DataPublisher String
main = dataPublisher
|],
  testInputs  = []
  }

token ∷ TranspilationTestCase
token = TranspilationTestCase {
  testName    = "token",
  testHaskell = [r|
module Token where

import           Juvix.Lib

type State = Map Key Nat

data Action
  = GetTotalSupply
  | GetBalanceOf Key
  | Transfer Key Nat

data Result
  = TotalSupply Nat
  | Balance Nat
  | Transferred

totalSupply ∷ Nat
totalSupply = 1000

main ∷ (Action, State) → IO (Result, State)
main (action, state) =
  case action of
    GetTotalSupply    → return (TotalSupply totalSupply, state)
    GetBalanceOf key  → do
      let balance = fromMaybe 0 (mapGet key state)
      return (Balance balance, state)
    Transfer to amount → do
      let oldSenderBalance = fromMaybe 0 (mapGet (manager source) state)
          newSenderBalance = oldSenderBalance - amount
      if newSenderBalance < 0 then fail else do
        let oldRecipientBalance = fromMaybe 0 (mapGet to state)
            newRecipientBalance = oldRecipientBalance + amount
        return (Transferred, mapUpdate to (Just newRecipientBalance) state)
|],
  testInputs = []
  }
