module Juvix.LLVM.Shared where

import Juvix.Library hiding (Type)
import Juvix.Utility.HashMap as Map
import LLVM.AST

type SymbolTable = Map.Map Symbol Operand

-- | a mapping between the variant and the sum type it encompasses
type VariantToType = Map.Map Symbol Symbol
