module Juvix.Core.Parameterisations.Naturals where

import Juvix.Core.Types hiding (typeOf)
import Juvix.Library

data NatTy
  = Nat
  deriving (Show, Eq)

data NatVal
  = Natural Natural
  deriving (Show, Eq)

typeOf ∷ NatVal → NatTy
typeOf (Natural _) = Nat

naturals ∷ Parameterisation NatTy NatVal
naturals = Parameterisation typeOf
