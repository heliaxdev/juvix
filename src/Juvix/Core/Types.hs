module Juvix.Core.Types where

import Juvix.Library

data CoreParams primTy primVal
  = CoreParams
      { typeOf ∷ primVal → primTy
      }
