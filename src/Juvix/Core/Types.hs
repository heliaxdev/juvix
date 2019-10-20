module Juvix.Core.Types where

data Parameterisation primTy primVal
  = Parameterisation
      { typeOf ∷ primVal → primTy
      }
