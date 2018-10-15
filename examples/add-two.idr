module Main

-- A helper function to add two to any number!
addTwo : Nat -> Nat
addTwo n = S (S n)

-- The Michelson contract function.
main : (Nat, Nat) -> (Nat, Nat)
main (x, y) = (y, addTwo Z)

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f
