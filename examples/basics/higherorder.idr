module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

mulBy2 : Integer -> Integer
mulBy2 = (*) 2

main : ((), Integer) -> (List Operation, Integer)
main ((), storage) = (Nil, mulBy2 storage)

main_test1 : main ((), 2) = (Nil, 2 * 2)
main_test1 = Refl
