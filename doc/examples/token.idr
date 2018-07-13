module Swap

import Prelude
import Data.SortedMap

%default total

{- Utility -}

lookupWithDefault : (SortedMap k v) -> k -> v -> v
lookupWithDefault map key default =
  case lookup key map of
    Just val  => val
    Nothing   => default

record Token where
  constructor MkToken
  supply: Nat
  balances: SortedMap String Nat
  allowances: SortedMap (String, String) Nat
  
{- Total Supply -}

totalSupply : Token -> Nat
totalSupply = supply

{- Balance -}

balanceOf : Token -> String -> Nat
balanceOf token key = lookupWithDefault (balances token) key 0

{- Allowance -}

allowance : Token -> String -> String -> Nat
allowance token key spender = lookupWithDefault (allowances token) (key, spender) 0

{- Transfer -}

transfer : Token -> String -> String -> Nat -> (Token, Bool)
transfer token key dest amount =
  let sourceBalance = balanceOf token key
  in case isLTE sourceBalance amount of
    Yes prf =>
      let destBalance = balanceOf token dest
      in (record { balances $= (insert dest (destBalance + amount) . insert key (sourceBalance - amount)) } token, True)
    No _ => (token, False)

-- Prove: transfer reduces balance of source
-- Prove: transfer adds balance to dest
-- Prove: transfer preserves total supply

{- Approve -}

approve : Token -> String -> Nat -> Bool
approve = ?approve

-- Prove: approve preserves total supply
-- Prove: approve determines allowance

{- transferFrom -}

transferFrom : Token -> String -> String -> Nat -> Bool
transferFrom = ?transferFrom

-- Prove: approve allows transferFrom
-- Prove: transferFrom fails without approve
-- Prove: transferFrom reduces balance of source
-- Prove: transferFrom adds balance to dest
-- Prove: transferFrom preserves total supply

