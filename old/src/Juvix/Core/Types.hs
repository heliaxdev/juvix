{-
 - Eagerly awaiting Dependent Haskell (https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell).
 - Until then, this is about the closest one can get.
-}

module Juvix.Core.Types where

import qualified Control.Monad.Except as E
import qualified Data.Text            as T
import           Data.Typeable        (Proxy (..), typeOf)
import           Foundation
import qualified Prelude              as P
import qualified Type.Reflection      as T

class PrettyPrint a where
  prettyPrintValue ∷ a → T.Text

  default prettyPrintValue ∷ (P.Show a) ⇒ a → T.Text
  prettyPrintValue = T.pack . P.show

  prettyPrintType ∷ a → T.Text

  default prettyPrintType ∷ (T.Typeable a) ⇒ a → T.Text
  prettyPrintType = T.pack . P.show . T.typeOf

  prettyPrintProxy ∷ Proxy a → T.Text

  default prettyPrintProxy ∷ (T.Typeable a) ⇒ Proxy a → T.Text
  prettyPrintProxy = T.pack . P.show . typeOf

instance (T.Typeable a, PrettyPrint a) ⇒ PrettyPrint (Maybe a) where
  prettyPrintValue = const "OptionType"

instance (T.Typeable a, T.Typeable b, PrettyPrint a, PrettyPrint b) ⇒ PrettyPrint (a, b) where
  prettyPrintValue = const "ProductType"

instance (T.Typeable a, T.Typeable b, PrettyPrint a, PrettyPrint b) ⇒ PrettyPrint (Either a b) where
  prettyPrintValue = const "SumType"

instance (T.Typeable a, T.Typeable b, PrettyPrint a, PrettyPrint b) ⇒ PrettyPrint (a → b) where
  prettyPrintValue = const "λ"

instance (T.Typeable a, PrettyPrint a) ⇒ PrettyPrint [a] where
  prettyPrintValue l = T.concat ["[", T.intercalate ", " (fmap prettyPrintValue l), "]"]

type MonadError e m = E.MonadError e m

throw ∷ ∀ a e m . (MonadError e m) ⇒ e → m a
throw = E.throwError

catch ∷ ∀ a e m . (MonadError e m) ⇒ m a → (e → m a) → m a
catch = E.catchError

data DynamicError where
  CannotCast      ∷ ∀ a . Dynamical a ⇒ DynamicValue → T.TypeRep a → DynamicError
  CannotUnify     ∷ ∀ a b . (Dynamical a, Dynamical b) ⇒ Proxy a → Proxy b → DynamicError
  NotAnOptionType ∷ ∀ a . Dynamical a ⇒ Proxy a → DynamicError
  NotAProductType ∷ ∀ a . Dynamical a ⇒ Proxy a → DynamicError
  NotASumType     ∷ ∀ a . Dynamical a ⇒ Proxy a → DynamicError
  NotAnArrowType  ∷ ∀ a . Dynamical a ⇒ Proxy a → DynamicError

instance Eq DynamicError where
  (==) _ _ = False -- TODO

{- I *think* we can actually implement this with generics. Useful? -}

class (Eq a, PrettyPrint a, T.Typeable a) ⇒ Dynamical a where
  unSum     ∷ ∀ m . (MonadError DynamicError m) ⇒ Proxy a → m (DynamicType, DynamicType)
  unSum     = throw . NotASumType

  unProduct ∷ ∀ m . (MonadError DynamicError m) ⇒ Proxy a → m (DynamicType, DynamicType)
  unProduct = throw . NotAProductType

  unOption  ∷ ∀ m . (MonadError DynamicError m) ⇒ Proxy a → m DynamicType
  unOption  = throw . NotAnOptionType

  unArrow ∷ ∀ m . (MonadError DynamicError m) ⇒ Proxy a → m (DynamicType, DynamicType)
  unArrow   = throw . NotAnArrowType

instance (Dynamical a, Dynamical b) ⇒ Dynamical (Either a b) where
  unSum (Proxy ∷ Proxy (Either a b)) = return (DynamicType (Proxy ∷ Proxy a), DynamicType (Proxy ∷ Proxy b))

instance (Dynamical a, Dynamical b) ⇒ Dynamical (a, b) where
  unProduct (Proxy ∷ Proxy (x, y)) = return (DynamicType (Proxy ∷ Proxy a), DynamicType (Proxy ∷ Proxy b))

instance (Dynamical a) ⇒ Dynamical (Maybe a) where
  unOption (Proxy ∷ Proxy (Maybe a)) = return (DynamicType (Proxy ∷ Proxy a))

instance (Eq a, Eq b) ⇒ Eq (a → b) where
  (==) _ _ = False

instance (Dynamical a, Dynamical b) ⇒ Dynamical (a → b) where
  unArrow (Proxy ∷ Proxy (a → b)) = return (DynamicType (Proxy ∷ Proxy a), DynamicType (Proxy ∷ Proxy b))

{- This is essentially a modified version of Data.Dynamic, as we want more constraints on the typeclass. -}

data DynamicValue where
  DynamicValue ∷ ∀ a . Dynamical a ⇒ a → DynamicValue

{- Existentially quantified type proxy. -}

data DynamicType where
  DynamicType ∷ ∀ a . (Dynamical a) ⇒ Proxy a → DynamicType
