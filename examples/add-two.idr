module Main

-- A helper function to add two to any number!
addTwo : Nat -> Nat
addTwo n = S (S n)

-- The Michelson contract function.
main : (Nat, Nat) -> (Nat, Nat)
main (x, y) = (y, addTwo x)

-- An interesting proof!
--mainAlwaysAdds : (a : Nat, b : Nat) -> GT (snd (main (a, b))) a
--mainAlwaysAdds a b = rewrite (gt a) in Refl

-- Proof helper.
--gt : (a : Nat) -> GT (addTwo a) a
--gt a = Refl

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f
