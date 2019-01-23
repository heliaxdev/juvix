module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Filler for now.

{-
Key : Type
Key = String

Signature : Type
Signature = String

verify : Key -> String -> Signature -> Bool
verify _ _ _ = True
-}

-- Main contract function.
main : (String, String) -> (List Operation, String)
main _ = (Nil, "")

-- TODO: Basic multisig: execute a transaction on N-of-M signatures. Requires ECDSA primitives.
