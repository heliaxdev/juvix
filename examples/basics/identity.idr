module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : (String, String) -> (List Operation, String)
main (_, storage) = (nil, storage)

{-

main_test1 : main "xy" = "xy"
main_test1 = Refl

main_test2 : (x : String) -> main x = x
main_test2 x = Refl

-}
