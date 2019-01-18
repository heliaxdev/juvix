module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : (List String, List String) -> (List Operation, List String)
main (param, storage) = (nil, storage)

-- Reverse a list of strings.
-- Ref https://www.michelson-lang.com/contract-a-day.html#sec-1-31
