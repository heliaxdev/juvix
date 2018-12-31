module Main

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : String -> String
main x = x

-- TODO: Basic oracle: periodic hash-commitment to value id, later Merkle tree proof on request of sub-value.
