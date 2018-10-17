module Main

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- The main function.
main : (Bool, Bool) -> (Bool, Bool)
main (storage, param) =
  case (storage, param) of
    (True, False) => (True, True)
    (False, True) => (True, True)
    _ => (False, False)

-- Some very trivial proofs.
proofOne : main (True, False) = (True, True)
proofOne = Refl

proofTwo : main (False, True) = (True, True)
proofTwo = Refl

proofThree : main (False, False) = (False, False)
proofThree = Refl

proofFour : main (True, True) = (False, False)
proofFour = Refl
