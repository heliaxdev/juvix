module Juvix.Core.HR.Types where

import Juvix.Library
import Juvix.Core.Usage

data Term primTy primVal binderTy
  = Star Natural
  | PrimTy primTy
  | Pi Usage (Term primTy primVal binderTy) (Term primTy primVal binderTy)
  | Lam binderTy (Term primTy primVal binderTy)
  | Elim (Elim primTy primVal binderTy)

  deriving (Show, Eq, Generic)

data Elim primTy primVal binderTy
  = Var binderTy
  | Prim primVal
  | App (Term primTy primVal binderTy) (Elim primTy primVal binderTy)
  | Ann Usage (Term primTy primVal binderTy) (Term primTy primVal binderTy)

  deriving (Show, Eq, Generic)
