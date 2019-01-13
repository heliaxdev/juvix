module Main

import Tezos

%default total

%access public export

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : (String, String) -> (List Operation, String)
main (_, _) = (nil, "hello world")
