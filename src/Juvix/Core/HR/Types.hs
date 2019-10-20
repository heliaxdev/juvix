module Juvix.Core.HR.Types where

import Juvix.Core.Usage
import Juvix.Library

data Term primTy primVal
  = Star Natural
  | PrimTy primTy
  | Pi Usage (Term primTy primVal) (Term primTy primVal)
  | Lam Symbol (Term primTy primVal)
  | Elim (Elim primTy primVal)
  deriving (Show, Eq, Generic)

data Elim primTy primVal
  = Var Symbol
  | Prim primVal
  | App (Elim primTy primVal) (Term primTy primVal)
  | Ann Usage (Term primTy primVal) (Term primTy primVal)
  deriving (Show, Eq, Generic)
