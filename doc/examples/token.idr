module Swap

import Prelude
import Data.SortedMap

%default total

{- Definitions -}

lookupWithDefault : (SortedMap k v) -> k -> v -> v
lookupWithDefault map key default =
  case lookup key map of
    Just val  => val
    Nothing   => default

record Token where
  constructor MkToken
  balances: SortedMap String Nat

newToken : (s : String, a : Nat) -> Token
newToken s a = MkToken (insert s a empty)

totalSupply : Token -> Nat
totalSupply = foldl (+) 0 . map snd . toList . balances

balanceOf : Token -> String -> Nat
balanceOf token key = lookupWithDefault (balances token) key 0

transfer : Token -> String -> String -> Nat -> (Token, Bool)
transfer token key dest amount =
  let sourceBalance = balanceOf token key
  in case isLTE amount sourceBalance of
    Yes prf =>
      let destBalance = balanceOf token dest
      in (record { balances $= (insert dest (destBalance + amount) . insert key ((-) sourceBalance amount {smaller = prf})) } token, True)
    No _ => (token, False)

{- Proofs -}

{- The idea is that you don't care about the implementation. A "token" is defined as any implementation which satisfies these proofs.
   Then we can use them as *rewrite rules* (whoah) under search-based compilation optimization. -}

-- Prove: new token has correct total supply
newTotalSupply : (s : String, a : Nat) -> totalSupply (newToken s a) = a
newTotalSupply = ?newTotalSupply

-- Prove: new token has correct balance
newBalanceOf : (s : String, a : Nat) -> balanceOf (newToken s a) s = a
newBalanceOf = ?newBalanceOf

-- Prove: new token has no balance for any other
newBalanceOfOther : (s : String, o : String, a : Nat) -> Not (s = o) -> balanceOf (newToken s a) o = 0
newBalanceOfOther = ?newBalanceOfOther

-- Prove: transfer reduces balance of source by amount
transferBalanceReducesSource : (t : Token, s : String, d : String, a : Nat) => GTE (balanceOf t s) a -> (balanceOf (fst (transfer t s d a)) s + a) = (balanceOf t s)
transferBalanceReducesSource = ?transferBalanceReducesSource

-- Prove: transfer adds balance of amount to dest
transferBalanceAddsDest : (t : Token, s : String, d : String, a : Nat) => GTE (balanceOf t s) a -> (balanceOf (fst (transfer t s d a)) d) = (balanceOf t d + a)
transferBalanceAddsDest = ?transferBalanceAddsDest

-- Prove: transfer preserves total supply
transferTotalSupplyPreserved : (t : Token, s : String, d : String, a : Nat) => totalSupply t = totalSupply (fst (transfer t s d a))
transferTotalSupplyPreserved = ?transferTotalSupplyPreserved
