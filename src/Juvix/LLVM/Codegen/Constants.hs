-- | Module for predefined constants
module Juvix.LLVM.Codegen.Constants where

import Juvix.Library hiding (Type)
import qualified LLVM.AST.Type as Type

portLength ∷ Num p ⇒ p
portLength = 32

-- | An abbreviation for 'IntegerType' 8
i4 ∷ Type.Type
i4 = Type.IntegerType 4
