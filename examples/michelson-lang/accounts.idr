module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : (String, String) -> (List Operation, String)
main (_, storage) = (Nil, storage)

-- Simple account system
-- Ref https://www.michelson-lang.com/contract-a-day.html#sec-1-28
