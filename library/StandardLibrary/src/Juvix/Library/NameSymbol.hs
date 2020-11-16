{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}


module Juvix.Librbary.NameSymbol where

import Juvix.Library
import qualified Juvix.Library.NameSymbol.NonEmpty as NonEmpty
import Control.Lens hiding ((|>), cons)

-- | T is the base class that talks about normal symbol operations
class (Eq t, Show t) => T t where
  toSymbol :: t -> Symbol
  fromSymbol :: Symbol -> t
  prefixOf :: t -> t -> Bool
  takePrefixOf :: t -> t -> Maybe t
  cons :: Symbol -> t -> t
  hd :: t -> Symbol

-- | The default NameSymbol implementation
instance T NonEmpty.T where
  toSymbol = NonEmpty.toSymbol
  fromSymbol = NonEmpty.fromSymbol
  prefixOf = NonEmpty.prefixOf
  takePrefixOf = NonEmpty.takePrefixOf
  cons = NonEmpty.cons
  hd = NonEmpty.hd

type NonEmpty = NonEmpty.T

class T (Ext original new) => Extended original new where
  type Ext original new = res | res -> original new -- ?

  fromNameSymbol :: original -> new -> Ext original new
  originalName :: Ext original new -> original
  newName :: Ext original new -> new
  setNewName :: new -> Ext original new -> Ext original new

class T a => Extra a where
  boundGlobal :: a -> Maybe Symbol

type Qualified original new =
  (T original,
   Extra new,
   Extended original new,
   Ext original new ~ (original, new))


data Ex t a =
  Ex { extendedOriginalName :: t
      , extendedNewName :: a
      } deriving Show

-- data Qualified =
--   Qual { qualifiedOriginalName :: NonEmpty.T
--        , qualifiedNewName :: NonEmpty.T
--        } deriving Show

-- makeLensesWith camelCaseFields ''Qualified
-- makeLensesWith camelCaseFields ''Extended

-- setNewName :: HasNewName b a => b -> a -> b
-- setNewName ext val = ext |> set newName val

-- qualifyName :: (HasNewName b a, T a) => b -> a -> b
-- qualifyName = setNewName

-- instance Eq a => Eq (Extended t a) where
--   n1 == n2 = view newName n1 == view newName n2

-- instance Eq Qualified where
--   n1 == n2 = view newName n1 == view newName n2

-- instance T Qualified where
--   toSymbol x = toSymbol (x^.newName)
--   fromSymbol x = Qual (fromSymbol x) (fromSymbol x)
--   prefixOf x1 x2 = prefixOf (x1^.newName) (x2^.newName)
--   -- x2 is the bigger symbol thus the one you're really looking at
--   takePrefixOf x1 x2 = setNewName x2 <$> takePrefixOf (x1^.newName) (x2^.newName)
--   cons s = over newName (cons s)
--   hd x1 = hd (x1^.newName)

-- -- | Extended behavior to talk about
-- class (Eq t, Show t) => Extended t where
--   -- | @originalName@ for when we've erased the original
--   -- name and now we want it back
--   originalName :: T t1 => t -> t1
--   -- | @newName@ for when we want to set the new name
--   newName :: (Eq t1, Show t1) => t -> t1 -> t
