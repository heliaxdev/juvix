module Main

import Tezos

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Swap storage and parameter.
main : (String, String) -> (List Operation, String)
main (param, storage) = (nil, param)

{-

helloWorld : main ("Hello", "World") = ("World", "Hello")
helloWorld = Refl

-}
