module Juvix.LLVM.Codegen where

import Juvix.LLVM.Shared
import Juvix.Library hiding (Type)
import qualified Juvix.Utility.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import LLVM.AST.Global as Global
import qualified LLVM.AST.Linkage as L

-----------------------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------------------

double ∷ Type
double = FloatingPointType DoubleFP

-- TODO ∷ change this based on the machine, C++ pargma?

-- | Use 64 bit ints
int ∷ Type
int = IntegerType 64

-- number of ports on a node or the port offset
-- TODO ∷ Have this union of a pointer of the same size
-- so we can have fixed size offset of nodes
numPorts ∷ Type
numPorts = IntegerType 16

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

-- TODO ∷ Figure out how to have a union here for all baked in types
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

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

uniqueName ∷ Symbol → Names → (Symbol, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing → (nm, Map.insert nm 1 ns)
    Just ix → (intern (unintern nm <> show ix), Map.insert nm (succ ix) ns)

-----------------------------------------------------------------------------------------
-- INets
-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------
-- Constants
-----------------------------------------------------------------------------------------

portLength ∷ Num p ⇒ p
portLength = 32
