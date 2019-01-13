module Main

import Prelude
import Data.SortedMap

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

record Token where
  constructor MkToken
  minter : String
  tokens : SortedMap Nat String

newToken : (s : String) -> Token
newToken s = MkToken s empty

totalSupply : Token -> Nat
totalSupply = Prelude.List.length . toList . tokens

exists : Token -> Nat -> Bool
exists t n = isJust $ Data.SortedMap.lookup n (tokens t)

ownerOf : Token -> Nat -> Maybe String
ownerOf t n = Data.SortedMap.lookup n (tokens t)

mint : Token -> String -> Nat -> (Token, Bool)
mint token to which =
  if exists token which then
    (token, False)
  else
    (record { tokens $= (insert which to) } token, True)

transfer : Token -> String -> String -> Nat -> (Token, Bool)
transfer token from to which =
  if ownerOf token which == Just from then
    (record { tokens $= (insert which to) } token, True)
  else (token, False)

data Action
  = Mint String Nat
  | Transfer String String Nat

-- Main contract function.
main : (Token, Action) -> (Token, Bool)
main (token, action) =
  case action of
    Mint to uid => mint token to uid
    Transfer from to uid => transfer token from to uid

-- Prove: iff transfer returns true, dest now owns token.
-- Prove: token can never be transferred by anyone other than current owner.
-- Prove: only minter can mint tokens.
