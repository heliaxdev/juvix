module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

{- Definitions -}

lookupWithDefault : (Map k v) -> k -> v -> v
lookupWithDefault map key default =
  case Map.get key map of
    Just val  => val
    Nothing   => default

record Token where
  constructor MkToken
  balances: Map String Nat

newToken : (s : String, a : Nat) -> Token
newToken s a = MkToken (Map.update s (Just a) Map.empty)

totalSupply : Token -> Nat
totalSupply t = Map.foldl (+) 0 (balances t)

balanceOf : Token -> String -> Nat
balanceOf token key = lookupWithDefault (balances token) key 0

transfer : Token -> String -> String -> Nat -> Token
transfer token key dest amount =
  let sourceBalance = balanceOf token key in
  if amount < sourceBalance then
    let destBalance = balanceOf token dest in
    record { balances $= (Map.update dest (Just (destBalance + amount)) . Map.update key (Just (sourceBalance - amount))) } token
  else token

data Action =
  Transfer String String Nat

main : (Token, Action) -> (List Operation, Token)
main (token, Transfer from to amount) = (Nil, transfer token from to amount)

{-

lookupInsert : (map : SortedMap k v, key : k, def : v, val : v) -> lookupWithDefault (insert key val map) key def = val
lookupInsert = ?lookupInsert

lookupUnaffected : (map : SortedMap k v, key : k, otherKey : k, val : v, def : v) -> Not (key = otherKey) -> lookupWithDefault (insert otherKey val map) key def = lookupWithDefault map key def
lookupUnaffected = ?lookupUnaffected

lookupEmpty : (key : k) -> lookup key empty = Nothing
lookupEmpty k = ?loookupEmpty

lookupDefaultEmpty : (key : k, def : v) -> lookupWithDefault empty key def = def
lookupDefaultEmpty k d = ?lookupDefaultEmpty

lookupEmptyUnaffected : (map : SortedMap k v, key : k, otherKey: k, def: v, val: v) -> Not (key = otherKey) -> lookupWithDefault map key def = def -> lookupWithDefault (insert otherKey val map) key def = def
lookupEmptyUnaffected map key otherKey def val ne eq = rewrite lookupUnaffected map key otherKey val def ne in rewrite eq in Refl

-- Prove: new token has correct total supply
newTotalSupply : (s : String, a : Nat) -> totalSupply (newToken s a) = a
newTotalSupply = ?newTotalSupply

-- Prove: new token has correct balance
newBalanceOf : (s : String, a : Nat) -> balanceOf (newToken s a) s = a
newBalanceOf s a =
  let prf = lookupInsert empty s 0 a in rewrite prf in Refl

-- Prove: new token has no balance for any other
newBalanceOfOther : (s : String, o : String, a : Nat) -> Not (o = s) -> balanceOf (newToken s a) o = 0
newBalanceOfOther s o a ne =
  let prf = lookupEmptyUnaffected empty o s 0 a ne (lookupDefaultEmpty o 0) in rewrite prf in Refl

-- Prove: transfer reduces balance of source by amount
transferBalanceReducesSource : (t : Token, s : String, d : String, a : Nat) => LTE a (balanceOf t s) -> (balanceOf (fst (transfer t s d a)) s + a) = (balanceOf t s)
transferBalanceReducesSource prf = ?transferBalanceReducesSource

-- Prove: transfer adds balance of amount to dest
transferBalanceAddsDest : (t : Token, s : String, d : String, a : Nat) => LTE a (balanceOf t s) -> (balanceOf (fst (transfer t s d a)) d) = (balanceOf t d + a)
transferBalanceAddsDest prf = ?transferBalanceAddsDest

-- Prove: transfer preserves total supply
transferTotalSupplyPreserved : (t : Token, s : String, d : String, a : Nat) => totalSupply t = totalSupply (fst (transfer t s d a))
transferTotalSupplyPreserved = ?transferTotalSupplyPreserved

-}
