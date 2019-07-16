{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Juvix.Library hiding (link, reduce)
import           Data.Map.Strict as Map
import qualified Data.Typeable as T

data Eal a where
  Term   :: SomeSymbol → Eal SomeSymbol
  Lambda :: SomeSymbol → Types a → Term b → Eal (a → b)
  App    :: Eal (a → b) → Eal a → Eal b

deriving instance Show (Eal a)

data Term a = Bang Integer (Eal a)
          deriving Show

-- remove the Bang'' later... only a hack for now!
newtype Bang a = Bang'' a deriving Show

data UBang a

--deriving instance Show (Bang a)
deriving instance Show (UBang a)

deriving instance Typeable (Bang a)
deriving instance Typeable (Bang)
deriving instance Typeable (UBang a)

data Types a where
  Lolly  :: Types b → Types a
  BangT  :: Types a → Types (Bang a)
  UBangT :: Types a → Types (UBang a)
  TermT  :: Types a

deriving instance Show (Types a)

data TypeErrors = MisMatch
                | MissingOverUse

data WrappedTypes where
  WrapT :: forall a. Dynamical a ⇒ Types a → WrappedTypes

deriving instance Show WrappedTypes

unwrapTWith :: WrappedTypes → (forall p. Types p → r) → r
unwrapTWith (WrapT e) f = f e

unwrapT :: WrappedTypes → TypeRep
unwrapT (WrapT e) = T.typeOf e

typeOf :: (HasState "ctxt" (Map SomeSymbol WrappedTypes) m, HasThrow "typ" TypeErrors m)
       ⇒ Eal a → m WrappedTypes
typeOf (Term s) = do
  ctxt ← get @"ctxt"
  case ctxt Map.!? s of
    Nothing → throw @"typ" MissingOverUse
    Just x@(WrapT (_ :: Types a)) →
      case isBang (Proxy :: Proxy a) of
        False → pure x
        True  → put @"ctxt" (Map.delete s ctxt) >> pure x

typeOf (Lambda sym symType t) = do
--  modify Map.insert undefined
  undefined
typeOf (App t1 t2)  = undefined

typeOfTerm (Bang n t) = undefined

type family (F a) :: Bool where
  F (Bang _)  = 'True
  F a         = 'False

class (Typeable a) ⇒ Dynamical a where
  isBang :: Proxy a → Bool

instance (Typeable a, F a ~ flag, Dynamical' flag a) ⇒ Dynamical a where
  isBang = isBang' (Proxy :: Proxy flag)

class Dynamical' (flag :: Bool) a where
  isBang' :: Proxy flag → Proxy a → Bool

instance Dynamical' 'True (Bang a) where
  isBang' _ _ = True

instance Dynamical' 'False a where
  isBang' _ _ = False
