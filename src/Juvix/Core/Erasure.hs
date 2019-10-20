module Juvix.Core.Erasure
  ( erase',
  )
where

import qualified Juvix.Core.IR.Types as Core
import Juvix.Core.Utility
import qualified Juvix.EAC.Types as EAC
import Juvix.Library hiding (empty)
import Juvix.Utility
import Prelude ((!!))

erase' ∷ Core.CTerm primTy primVal → Either ErasureError (EAC.Term, EAC.TypeAssignment)
erase' cterm =
  let (term, env) = exec (erase cterm)
   in term >>| \term →
        (term, typeAssignment env)

exec ∷ EnvErasure a → (Either ErasureError a, Env)
exec (EnvEra env) = runState (runExceptT env) (Env empty 0 [])

erase ∷
  ( HasState "typeAssignment" EAC.TypeAssignment m,
    HasState "nextName" Int m,
    HasState "nameStack" [Int] m,
    HasThrow "erasureError" ErasureError m
  ) ⇒
  Core.CTerm primTy primVal →
  m EAC.Term
erase term =
  case term of
    Core.Lam body → do
      name ← newName
      -- TODO: Instead calculate type of this lambda-bound variable.
      let ty = EAC.SymT name
      -- TODO ∷ replace map here with unordered map
      -- then remove the Ord deriving from the Symbol type.
      _stk ← get @"nameStack"
      modify @"typeAssignment" (insert name ty)
      body ← erase body
      pure (EAC.Lam name body)
    Core.Conv iterm → do
      case iterm of
        Core.Bound n → do
          name ← unDeBruijin (fromIntegral n)
          pure (EAC.Var name)
        Core.Free n →
          case n of
            Core.Global s → pure (EAC.Var (intern s))
            Core.Local _s → throw @"erasureError" Unsupported
            Core.Quote _s → throw @"erasureError" Unsupported
        Core.App a b → do
          a ← erase (Core.Conv a)
          b ← erase b
          pure (EAC.App a b)
        Core.Ann _ _ a → do
          erase a
    --Core.Nat n → pure (EAC.Prim (EAC.Nat n))
    _ → throw @"erasureError" Unsupported

data Env
  = Env
      { typeAssignment ∷ EAC.TypeAssignment,
        nextName ∷ Int,
        nameStack ∷ [Int]
      }
  deriving (Show, Eq, Generic)

newtype EnvErasure a = EnvEra (ExceptT ErasureError (State Env) a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "typeAssignment" EAC.TypeAssignment)
    via Field "typeAssignment" () (MonadState (ExceptT ErasureError (State Env)))
  deriving
    (HasState "nextName" Int)
    via Field "nextName" () (MonadState (ExceptT ErasureError (State Env)))
  deriving
    (HasState "nameStack" [Int])
    via Field "nameStack" () (MonadState (ExceptT ErasureError (State Env)))
  deriving
    (HasThrow "erasureError" ErasureError)
    via MonadError (ExceptT ErasureError (MonadState (State Env)))

data ErasureError
  = Unsupported
  deriving (Show, Eq, Generic)
