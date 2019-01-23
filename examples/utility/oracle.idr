module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Hash function (placeholder).
hash : String -> String
hash s = s

-- Assumed.
hash_no_preimage : (hash x = hash y) -> x = y
hash_no_preimage p = really_believe_me p

-- Main contract function.
main : (String, String) -> (List Operation, String)
main _ = (Nil, "")

-- TODO: Basic oracle: periodic hash-commitment to value id, later Merkle tree proof on request of sub-value.
