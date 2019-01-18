module Main

import Tezos

%default total
%access public export

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : (Tez, String) -> (List Operation, Tez)
main (_, _) = (nil, amount)
