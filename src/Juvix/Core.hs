module Juvix.Core
  ( module HR,
    module IR,
    hrToIR,
    irToHR,
    module Juvix.Core.Erasure,
    module Juvix.Core.Usage,
  )
where

import Juvix.Core.Erasure
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import Juvix.Core.Usage
import Juvix.Core.Utility
import Juvix.Library

hrToIR ∷ HR.Term primTy primVal → IR.CTerm primTy primVal
hrToIR = fst . exec . hrToIR'

hrToIR' ∷
  (HasState "symbolStack" [Symbol] m) ⇒
  HR.Term primTy primVal →
  m (IR.CTerm primTy primVal)
hrToIR' = undefined

irToHR ∷ IR.CTerm primTy primVal → HR.Term primTy primVal
irToHR = fst . exec . irToHR'

irToHR' ∷
  ( HasState "nextName" Int m,
    HasState "nameStack" [Int] m
  ) ⇒
  IR.CTerm primTy primVal →
  m (HR.Term primTy primVal)
irToHR' term =
  case term of
    IR.Star n → pure (HR.Star n)
    IR.PrimTy p → pure (HR.PrimTy p)
    IR.Pi u a b → do
      a ← irToHR' a
      b ← irToHR' b
      pure (HR.Pi u a b)
    IR.Lam t → do
      n ← newName
      t ← irToHR' t
      pure (HR.Lam n t)
    IR.Conv e → HR.Elim |<< irElimToHR' e

irElimToHR' ∷
  (HasState "nextName" Int m, HasState "nameStack" [Int] m) ⇒
  IR.ITerm primTy primVal →
  m (HR.Elim primTy primVal)
irElimToHR' e =
  case e of
    IR.Bound i → do
      v ← unDeBruijin (fromIntegral i)
      pure (HR.Var v)
    IR.Prim p → pure (HR.Prim p)
    IR.App f x → do
      f ← irElimToHR' f
      x ← irToHR' x
      pure (HR.App f x)
    IR.Ann u t x → do
      t ← irToHR' t
      x ← irToHR' x
      pure (HR.Ann u t x)

exec ∷ EnvConv a → (a, Env)
exec (EnvCon env) = runState env (Env 0 [])

data Env = Env {nextName ∷ Int, nameStack ∷ [Int]} deriving (Show, Eq, Generic)

newtype EnvConv a = EnvCon (State Env a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "nextName" Int)
    via Field "nextName" () (MonadState (State Env))
  deriving
    (HasState "nameStack" [Int])
    via Field "nameStack" () (MonadState (State Env))
