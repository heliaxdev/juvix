module Main

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- The main function.
main : (Nat, Nat) -> (Nat, Nat)
main (storage, param) = (storage, param * storage)

{- Proof helpers. -}

mulByZero : (n : Nat) -> n * 0 = 0
mulByZero Z     = Refl
mulByZero (S m) = rewrite mulByZero m in Refl

{- Some very trivial proofs. -}

proofThreeTimesFive : main (3, 5) = (3, 15)
proofThreeTimesFive = Refl

proofMulZero : (n : Nat) -> main (0, n) = (0, 0)
proofMulZero n = rewrite mulByZero n in Refl

proofConstantStorage : (s : Nat, p : Nat) -> fst (main (s, p)) = s
proofConstantStorage s p = Refl
