module Main

import Data.SortedMap
import Data.Vect

||| Account contains the balance of the token.
Account : Type
Account = Nat

||| Address is the key hash, a string of length 36.
Address : Type
Address = String
{- Attempt at making address a dependent type
data Address : Type where
  MkAddress : (s : String) -> (length s = 36) -> Address

stringToAddress : (s : String) -> Maybe Address
stringToAddress s = case length s == 36 of
                         False => Nothing
                         True => Just MkAddress
-}

-- Prove address is a string of length 36.
--address36Char : (address : String) -> length address = 36
--address36Char address = ?address36Char_rhs

||| The storage has type Storage which is a record with fields accounts,
||| version number of the token standard, total supply, name, symbol, and owner of tokens.
record Storage where
    constructor MkStorage
    accounts : SortedMap Address Account
    version : Nat --version of the token standard
    totalSupply : Nat
    name : String
    symbol : String
    owner : Address

data Error = NotEnoughBalance
           | FailedToAuthenticate

||| getAccount returns the balance of an associated key hash.
||| @address the key hash of the owner of the balance
total getAccount : (address : Address) -> SortedMap Address Account -> Nat
getAccount address accounts = case lookup address accounts of
                      Nothing => 0
                      (Just balance) => balance

||| performTransfer transfers tokens from the from address to the dest address.
||| @from the address the tokens to be transferred from
||| @dest the address the tokens to be transferred to
||| @tokens the amount of tokens to be transferred
total performTransfer : (from : Address) -> (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error (SortedMap Address Account)
performTransfer from dest tokens storage =
  let fromBalance = getAccount from (accounts storage)
      destBalance = getAccount dest (accounts storage) in
        case lte tokens fromBalance of
             False => Left NotEnoughBalance
             True => let accountsStored = insert from (minus fromBalance tokens) (accounts storage) in
                       Right (insert dest (destBalance + tokens) accountsStored)

||| createAccount transfers tokens from the owner to an address
||| @dest the address of the account to be created
||| @tokens the amount of tokens in the new created account
total createAccount : (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error (Either Error (SortedMap Address Account))
createAccount dest tokens storage =
  let owner = owner storage in
      case owner == owner of --when sender can be detected, check sender == owner.
           False => Left FailedToAuthenticate
           True => Right (performTransfer owner dest tokens storage)

--Prove the total supply is the sum of all account balances. TODO define owner account & balance and its relation to totalSupply.
totalSupplyToken : (totalSupply : Nat) -> (allAccount : SortedMap Address Account) -> (sumAccountBalance = sum (values allAccount)) -> totalSupply = sumAccountBalance
totalSupplyToken totalSupply allAccount prf = ?totalSupplyToken_rhs
