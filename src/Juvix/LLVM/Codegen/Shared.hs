-- | Shared between Types and Sum
module Juvix.LLVM.Codegen.Shared where

import Juvix.Library hiding (Type)
import Juvix.Utility.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST ()
import qualified LLVM.AST.Constant as C ()
import LLVM.AST.Global as Global ()

type SymbolTable = Map.Map Symbol Operand

-- | a mapping between the variant and the sum type it encompasses
type VariantToType = Map.Map Symbol Symbol

type Names = Map.Map Symbol Int

instance Hashable Name
