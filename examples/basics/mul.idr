module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- The main function.
main : (Integer, Integer) -> (List Operation, Integer)
main (param, storage) = (nil, param * storage)

{-

mulByZero : (n : Nat) -> n * 0 = 0
mulByZero Z     = Refl
mulByZero (S m) = rewrite mulByZero m in Refl

proofThreeTimesFive : main (3, 5) = (3, 15)
proofThreeTimesFive = Refl

proofMulZero : (n : Nat) -> main (0, n) = (0, 0)
proofMulZero n = rewrite mulByZero n in Refl

proofConstantStorage : (s : Nat, p : Nat) -> fst (main (s, p)) = s
proofConstantStorage s p = Refl

-}
