import Data.List
import Data.SortedMap
import Effect.Exception
import Effects

||| Account contains the balance of the token.
Account : Type
Account = Nat

||| Address is the key hash of the owner of the associated account.
Address : Type
Address = String

record Storage where
    constructor MkStorage
    accounts : SortedMap Address Account
    version : Nat --version of the token standard
    totalSupply : Nat
    name : String
    symbol : String
    owner : Address

{-record Person where
    constructor MkPerson
    firstName, middleName, lastName : String
    age : Int

fred : Person
fred = MkPerson "Fred" "Joe" "Bloggs" 30
-}

total getAccount : Address -> SortedMap Address Account -> Nat
getAccount address accounts = case lookup address accounts of
                      Nothing => 0
                      (Just balance) => balance

total performTransfer : Address -> Address -> Nat -> Storage -> Either String (SortedMap Address Account)
performTransfer from dest tokens storage =
  let fromBalance = getAccount from (accounts storage)
      destBalance = getAccount dest (accounts storage) in
          case lte tokens fromBalance of
               False => Left "Not enough balance."
               True => let accountsStored = insert from (minus fromBalance tokens) (accounts storage) in
                       Right (insert dest (destBalance + tokens) accountsStored)
