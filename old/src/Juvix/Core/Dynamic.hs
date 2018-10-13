module Juvix.Core.Dynamic where

import qualified Data.Text          as T
import           Data.Typeable      (Proxy)
import           Foundation
import qualified Type.Reflection    as T

import           Juvix.Core.Types
import           Juvix.Core.Utility

toDynamicType ∷ ∀ a . (Dynamical a) ⇒ Proxy a → DynamicType
toDynamicType = DynamicType

toDynamicValue ∷ ∀ a. (Dynamical a) ⇒ a → DynamicValue
toDynamicValue = DynamicValue

fromDynamicValue ∷ ∀ a m . (MonadError DynamicError m, Dynamical a) ⇒ DynamicValue → m a
fromDynamicValue dyn@(DynamicValue (b ∷ bT)) = do
  let castRep = T.typeRep ∷ T.TypeRep a
  case T.eqTypeRep castRep (T.typeRep ∷ T.TypeRep bT) of
    Just T.HRefl → return b
    Nothing      → throw (CannotCast dyn castRep)

liftDyn1 ∷ ∀ b . (∀ a . Dynamical a ⇒ a → b) → DynamicValue → b
liftDyn1 func (DynamicValue a) = func a

appDyn1 ∷ ∀ a b m . (Dynamical a, MonadError DynamicError m) ⇒ (a → b) → DynamicValue → m b
appDyn1 func = (|<<) func . fromDynamicValue

instance Eq DynamicValue where
  (==) (DynamicValue x) y = (==) (Right x) (fromDynamicValue y)

instance PrettyPrint DynamicValue where
  prettyPrintValue  = liftDyn1 prettyPrintValue
  prettyPrintType   = liftDyn1 prettyPrintType

instance PrettyPrint DynamicError where
  prettyPrintValue (CannotUnify a b) = T.concat ["Type mismatch: ", prettyPrintProxy a, " <=> ", prettyPrintProxy b]
  prettyPrintValue _ = "TODO"
  prettyPrintType  _ = "TODO"
  prettyPrintProxy _ = "TODO"

instance Dynamical ()
instance Dynamical Bool
instance Dynamical Integer
instance Dynamical T.Text
