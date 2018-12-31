module Main

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : String -> String
main x = x

-- TODO: Basic multisig: execute a transaction on N-of-M signatures. Requires ECDSA primitives.
