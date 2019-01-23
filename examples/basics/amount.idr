module Main

import Tezos

%default total
%access public export

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : (String, Tez) -> (List Operation, Tez)
main (_, _) = (Nil, amount)
