module Juvix.Utility.PrettyPrint where

import qualified Data.Text       as T
import           Protolude
import qualified Type.Reflection as R

class PrettyPrint a where
  prettyPrintValue ∷ a → Text

  default prettyPrintValue ∷ (Show a) ⇒ a → Text
  prettyPrintValue = show

  prettyPrintType ∷ a → Text

  default prettyPrintType ∷ (R.Typeable a) ⇒ a → Text
  prettyPrintType = show . R.typeOf

instance (PrettyPrint a, R.Typeable a) ⇒ PrettyPrint [a] where
  prettyPrintValue l = T.concat ["[", T.intercalate ", " (fmap prettyPrintValue l), "]"]

instance PrettyPrint Text
instance PrettyPrint Integer
