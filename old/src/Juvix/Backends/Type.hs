module Juvix.Backends.Type where

data Backend a where
  MkBackend {
    backendTODO ∷ a
  } ∷ Backend a
