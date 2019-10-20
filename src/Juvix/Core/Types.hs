module Juvix.Core.Types where

import Juvix.Library

data Parameterisation primTy primVal
  = Parameterisation
      { typeOf ∷ primVal → primTy
      }
