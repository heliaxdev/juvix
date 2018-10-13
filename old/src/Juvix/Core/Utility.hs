module Juvix.Core.Utility where

import qualified Data.Text        as T
import           Foundation

import           Juvix.Core.Types

(|<<) ∷ Functor f ⇒ (a → b) → f a → f b
(|<<) = fmap

(>>|) ∷ Functor f ⇒ f a → (a → b) → f b
(>>|) = flip fmap

instance PrettyPrint ()
instance PrettyPrint Bool
instance PrettyPrint T.Text
instance PrettyPrint Integer
