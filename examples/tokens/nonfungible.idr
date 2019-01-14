module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

record Token where
  constructor MkToken
  tokens : Map Integer String

newToken : Token
newToken = MkToken Map.empty

totalSupply : Token -> Integer
totalSupply t = Map.size (tokens t)

exists : Token -> Integer -> Bool
exists t n = Map.member n (tokens t)

ownerOf : Token -> Integer -> Maybe String
ownerOf t n = Map.get n (tokens t)

mint : Token -> String -> Integer -> (Token, Bool)
mint token to which =
  if exists token which then
    (token, False)
  else
    (record { tokens $= (Map.update which (Just to)) } token, True)

transfer : Token -> String -> String -> Integer -> (Token, Bool)
transfer token from to which =
  if ownerOf token which == Just from then
    (record { tokens $= (Map.update which (Just to)) } token, True)
  else (token, False)

data Action
  = Mint String Integer
  | Transfer String String Integer

-- Main contract function.
main : (Token, Action) -> (Token, Bool)
main (token, action) =
  case action of
    Mint to uid => mint token to uid
    Transfer from to uid => transfer token from to uid

-- Prove: iff transfer returns true, dest now owns token.
-- Prove: token can never be transferred by anyone other than current owner.
-- Prove: only minter can mint tokens.
