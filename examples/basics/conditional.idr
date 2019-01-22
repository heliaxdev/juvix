module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- The main function.
main : (Bool, Bool) -> (List Operation, Bool)
main (param, storage) =
  case storage of
    True  =>
      case param of
        True  => (Nil, False)
        False => (Nil, True)
    False =>
      case param of
        True  => (Nil, True)
        False => (Nil, False)

{-

test XOR

proofOne : main (True, False) = (True, True)
proofOne = Refl

proofTwo : main (False, True) = (True, True)
proofTwo = Refl

proofThree : main (False, False) = (False, False)
proofThree = Refl

proofFour : main (True, True) = (False, False)
proofFour = Refl

-}
