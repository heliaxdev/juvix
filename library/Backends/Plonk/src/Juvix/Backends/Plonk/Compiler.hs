module Juvix.Backends.Plonk.Compiler where

import Juvix.Backends.Plonk.IR
import Juvix.Backends.Plonk.Types
import qualified Juvix.Core.ErasedAnn.Types as Ann
import Juvix.Library

-- translate case statements to conditionals (nested)
-- inline lambdas
-- convert datatypes to some field element representation
-- etc

termToIR ::
  FFTerm f ->
  IR i f ty
termToIR term = notImplemented

instOuter :: FFTerm f -> IR i f ty
instOuter a@(Ann.Ann _ ty _) = do
  inst <- inst a
  ty <- typeToPrimType ty
  expandedToInst ty inst

inst :: FFTerm f -> IR i f ty
inst (Ann.Ann _usage ty t) =
  case t of
    Ann.Var symbol -> var symbol
    Ann.AppM fun a -> appM fun a
    Ann.UnitM -> notImplemented
    Ann.PairM p1 p2 -> notImplemented
    Ann.LamM c a b -> do
      v <- lambda c a b ty
      consVal v ty
      pure v
    Ann.Prim prim' -> notImplemented
