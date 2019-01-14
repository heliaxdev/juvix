module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : (String, String) -> (List Operation, Integer)
main (_, _) = (nil, amount)
