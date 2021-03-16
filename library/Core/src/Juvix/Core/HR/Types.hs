module Juvix.Core.HR.Types
  ( module Juvix.Core.HR.Types,
    Universe,
    GlobalName,
    PatternVar,
    PatternMap,
    BoundVar,
    Name (..),
    GlobalUsage (..),
  )
where

import Juvix.Core.HR.Extend
import Juvix.Core.IR.Types.Base

data T

extendTerm "Term" [] [t|T|] extTerm

-- TODO allow extendTerm to reorder fields?
pattern Lam x t = Lam0 t x

pattern Pi π x s t = Pi0 π s t x

pattern Sig π x s t = Sig0 π s t x

pattern Let π x l b = Let0 π l b x

{-# COMPLETE Star, PrimTy, Prim, Pi, Lam, Sig, Pair, Let, UnitTy, Unit, Elim #-}

extendElim "Elim" [] [t|T|] extElim
