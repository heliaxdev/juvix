module Juvix.Core.HR.Types where

import Juvix.Core.Usage
import Juvix.Library

data Term primTy primVal
  = Star Natural
  | PrimTy primTy
  | Pi Usage (Term primTy primVal) (Term primTy primVal)
  | Lam Text (Term primTy primVal)
  | Elim (Elim primTy primVal)
  deriving (Show, Eq, Generic)

data Elim primTy primVal
  = Var Text
  | Prim primVal
  | App (Term primTy primVal) (Elim primTy primVal)
  | Ann Usage (Term primTy primVal) (Term primTy primVal)
  deriving (Show, Eq, Generic)
