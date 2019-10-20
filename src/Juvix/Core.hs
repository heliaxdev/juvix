module Juvix.Core
  ( module HR,
    module IR,
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
hrToIR = undefined

irToHR ∷ IR.CTerm primTy primVal → HR.Term primTy primVal
irToHR = fst . exec . irToHR'

irToHR' ∷
  ( HasState "nextName" Int m,
    HasState "nameStack" [Int] m
  ) ⇒
  IR.CTerm primTy primVal →
  m (HR.Term primTy primVal)
irToHR' = undefined

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
