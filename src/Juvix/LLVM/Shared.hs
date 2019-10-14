module Juvix.LLVM.Shared where

import Juvix.Library hiding (Type)
import Juvix.Utility.HashMap as Map
import LLVM.AST

type SymbolTable = Map.Map Symbol Operand

-- | a mapping between the variant and the sum type it encompasses
type VariantToType = Map.Map Symbol Symbol

type Names = Map.Map Symbol Int

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

data CodegenState
  = CodegenState
      { -- | Name of the active block to append to
        currentBlock ∷ Name,
        -- | Blocks for function
        blocks ∷ Map.Map Name BlockState,
        -- | Function scope symbol table
        symtab ∷ SymbolTable,
        -- | a mapping from the variants to the sum type
        vartab ∷ VariantToType,
        -- | Count of basic blocks
        blockCount ∷ Int,
        -- | Count of unnamed instructions
        count ∷ Word,
        -- | Name Supply
        names ∷ Names
      }
  deriving (Show, Generic)

data BlockState
  = BlockState
      { -- | Block index
        idx ∷ Int,
        -- | Stack of instructions
        stack ∷ [Named Instruction],
        -- | Block terminator
        term ∷ Maybe (Named Terminator)
      }
  deriving (Show, Generic)

newtype Codegen a = CodeGen {runCodegen ∷ State CodegenState a}
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "currentBlock" Name)
    via Field "currentBlock" () (MonadState (State CodegenState))
  deriving
    (HasState "blocks" (Map.Map Name BlockState))
    via Field "blocks" () (MonadState (State CodegenState))
  deriving
    (HasState "symtab" SymbolTable)
    via Field "symtab" () (MonadState (State CodegenState))
  deriving
    (HasState "vartab" VariantToType)
    via Field "vartab" () (MonadState (State CodegenState))
  deriving
    (HasState "blockCount" Int)
    via Field "blockCount" () (MonadState (State CodegenState))
  deriving
    (HasState "count" Word)
    via Field "count" () (MonadState (State CodegenState))
  deriving
    (HasState "names" Names)
    via Field "names" () (MonadState (State CodegenState))
