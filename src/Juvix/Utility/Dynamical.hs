module Juvix.Utility.Dynamical where

import           Data.Typeable
import           Protolude
import qualified Type.Reflection           as R

import           Juvix.Utility.PrettyPrint
import           Juvix.Utility.Sugar

{-
 - This is a modified version of Data.Dynamic and friends.
 - We want more constraints on the typeclass.
 - I think generics might be a better approach...
 -}

class (Eq a, PrettyPrint a, R.Typeable a) ⇒ Dynamical a where

data DynamicError where
  CannotCast ∷ ∀ a . Dynamical a ⇒ DynamicValue → R.TypeRep a → DynamicError

data DynamicValue where
  DynamicValue ∷ ∀ a . Dynamical a ⇒ a → DynamicValue

data DynamicType where
  DynamicType ∷ ∀ a . (Dynamical a) ⇒ Proxy a → DynamicType

toDynamicType ∷ ∀ a . (Dynamical a) ⇒ Proxy a → DynamicType
toDynamicType = DynamicType

toDynamicValue ∷ ∀ a. (Dynamical a) ⇒ a → DynamicValue
toDynamicValue = DynamicValue

fromDynamicValue ∷ ∀ a m . (MonadError DynamicError m, Dynamical a) ⇒ DynamicValue → m a
fromDynamicValue dyn@(DynamicValue (b ∷ bT)) = do
  let castRep = R.typeRep ∷ R.TypeRep a
  case R.eqTypeRep castRep (R.typeRep ∷ R.TypeRep bT) of
    Just R.HRefl → return b
    Nothing      → throw (CannotCast dyn castRep)

liftDyn1 ∷ ∀ b . (∀ a . Dynamical a ⇒ a → b) → DynamicValue → b
liftDyn1 func (DynamicValue a) = func a

appDyn1 ∷ ∀ a b m . (Dynamical a, MonadError DynamicError m) ⇒ (a → b) → DynamicValue → m b
appDyn1 func = (|<<) func . fromDynamicValue

instance Dynamical ()
instance Dynamical Text
instance Dynamical Bool
instance Dynamical Integer
instance (Dynamical a) ⇒ Dynamical (Maybe a)
instance (Dynamical a, Dynamical b) ⇒ Dynamical (a, b)

instance (Eq (a → b)) where
  _ == _ = False

instance (Dynamical a, Dynamical b) ⇒ Dynamical (a → b)
