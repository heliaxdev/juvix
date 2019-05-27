import Data.SortedMap

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

data Error = NotEnoughBalance
           | FailedToAuthenticate

total getAccount : Address -> SortedMap Address Account -> Nat
getAccount address accounts = case lookup address accounts of
                      Nothing => 0
                      (Just balance) => balance

total performTransfer : Address -> Address -> Nat -> Storage -> Either Error (SortedMap Address Account)
performTransfer from dest tokens storage =
  let fromBalance = getAccount from (accounts storage)
      destBalance = getAccount dest (accounts storage) in
        case lte tokens fromBalance of
             False => Left NotEnoughBalance
             True => let accountsStored = insert from (minus fromBalance tokens) (accounts storage) in
                       Right (insert dest (destBalance + tokens) accountsStored)

total createAccount : Address -> Nat -> Storage -> Either Error (Either Error (SortedMap Address Account))
createAccount dest tokens storage =
  let owner = owner storage in
      case owner == owner of --when sender can be detected, check sender == owner.
           False => Left FailedToAuthenticate
           True => Right (performTransfer owner dest tokens storage)
