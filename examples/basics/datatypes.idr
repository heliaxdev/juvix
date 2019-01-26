module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

data Storage
  = MkStorage Integer Integer

data Param
  = MkParam Integer Integer

main : (Param, Storage) -> (List Operation, Storage)
main (MkParam a b, MkStorage c d) = (Nil, MkStorage (a + c) (b * d))

main_test1 : main (MkParam 1 2, MkStorage 3 4) = (Nil, MkStorage (1 + 3) (2 * 4))
main_test1 = Refl
