-- | Shared between Types and Sum
module Juvix.LLVM.Codegen.Shared where

import Juvix.Library hiding (Type)
import Juvix.Utility.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST ()
import qualified LLVM.AST.Constant as C ()
import LLVM.AST.Global as Global ()

type SymbolTable = Map.Map Symbol Operand

type TypeTable = Map.Map Symbol Type

-- | a mapping between the variant and the sum type along with
-- the tag associated with it
type VariantToType = Map.Map Symbol (Symbol, Int)

type Names = Map.Map Symbol Int

instance Hashable Name
