module Juvix.LLVM.Codegen.Types
  ( module Juvix.LLVM.Codegen.Types,
    module Juvix.LLVM.Codegen.Shared,
  )
where

import Juvix.LLVM.Codegen.Shared
import Juvix.LLVM.Codegen.Sum
import Juvix.Library hiding (Type)
import qualified Juvix.Utility.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST ()
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C ()
import LLVM.AST.Global as Global ()

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

data Errors
  = -- | Error when a block does not exist
    NoSuchBlock Text
  deriving (Show)

newtype Codegen a = CodeGen {runCodegen ∷ ExceptT Errors (State CodegenState) a}
  deriving (Functor, Applicative, Monad)
  deriving
    (HasState "currentBlock" Name)
    via Field "currentBlock" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "blocks" (Map.Map Name BlockState))
    via Field "blocks" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "symtab" SymbolTable)
    via Field "symtab" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "vartab" VariantToType)
    via Field "vartab" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "blockCount" Int)
    via Field "blockCount" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "count" Word)
    via Field "count" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasState "names" Names)
    via Field "names" () (MonadState (ExceptT Errors (State CodegenState)))
  deriving
    (HasThrow "err" Errors)
    via MonadError (ExceptT Errors (State CodegenState))

--------------------------------------------------------------------------------
-- LLVM Types
--------------------------------------------------------------------------------

double ∷ Type
double = FloatingPointType DoubleFP

-- TODO ∷ change this based on the machine, C++ pargma?

-- | Use 64 bit ints
int ∷ Type
int = IntegerType 64

-- | 'numPortsSmall' is used for the number of ports that fit within 16 bits
numPortsSmall ∷ VarientInfo
numPortsSmall =
  Variant
    { size = 16,
      name = "small",
      typ' = IntegerType 16
    }

-- | 'numPortsLarge' is used for the number of ports that don't fit within 16 bits
numPortsLarge ∷ VarientInfo
numPortsLarge =
  Variant
    { size = 16,
      name = "large",
      typ' = PointerType
        { pointerReferent = nodeType,
          pointerAddrSpace = AddrSpace 16
        }
    }

-- number of ports on a node or the port offset
numPorts ∷ Type
numPorts =
  typ
    { elementTypes =
        let _ : rest = elementTypes typ
         in IntegerType {typeBits = 1} : rest
    }
  where
    typ = createSum [numPortsLarge, numPortsSmall]

-- | Construct a 16 bit port space so we can put many inside a node cheaply
-- The pointer points to the beginning of a node and an offset
portPointer ∷ Type
portPointer = PointerType
  { pointerReferent = nodeType,
    pointerAddrSpace = AddrSpace 16
  }

portType ∷ Type
portType = StructureType
  { isPacked = True,
    elementTypes =
      [ portPointer, -- the pointer to the other port
        numPorts -- the offset from the base of the node the port is
      ]
  }

-- TODO ∷ Figure out how to have an un-tagged union here for all baked in types
dataType ∷ Type
dataType = int

-- TODO ∷ Figure out how to get varying data in here
nodeType ∷ Type
nodeType = StructureType
  { isPacked = True,
    elementTypes =
      [ numPorts, -- length of this node
        ArrayType 0 portType, -- variable size array of ports
        ArrayType 0 dataType -- variable size array of data the node stores
      ]
  }
