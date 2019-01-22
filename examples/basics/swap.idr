module Main

import Tezos

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Swap storage and parameter.
main : (String, String) -> (List Operation, String)
main (param, storage) = (Nil, param)

snd : (a, b) -> b
snd (_, b) = b

main_swaps_ex : snd (main ("param", "storage")) = "param"
main_swaps_ex = Refl

main_swaps : (param, storage : String) -> snd (main (param, storage)) = param
main_swaps param storage = Refl
