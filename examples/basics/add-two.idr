module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

main : (Integer, Integer) -> (List Operation, Integer)
main (param, storage) = (nil, storage + param)

{-

addTwo : Nat -> Nat
addTwo n = S (S n)

greater : Nat -> Nat -> Bool
greater Z     _      = False
greater (S _) Z      = True
greater (S m) (S n)  = greater m n

greater_succ_2 : (a : Nat) -> greater (S (S a)) a = True
greater_succ_2 Z          = Refl
greater_succ_2 (S (S n))  = rewrite (greater_succ_2 n) in Refl

mainAlwaysAdds : (a : Nat, b : Nat) -> greater (snd (main (a, b))) a = True
mainAlwaysAdds a b = rewrite (greater_succ_2 a) in Refl

-}
