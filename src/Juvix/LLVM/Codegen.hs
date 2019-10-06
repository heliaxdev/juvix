module Juvix.LLVM.Codegen where

import           Juvix.Library                   hiding(Type)

import           LLVM.AST
import           LLVM.AST.AddrSpace
import           LLVM.AST.Global                 as Global
import qualified LLVM.AST                        as AST
import qualified Data.Map.Strict                 as Map

import qualified LLVM.AST.Linkage                as L
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.Attribute              as A
import qualified LLVM.AST.CallingConvention      as CC
import qualified LLVM.AST.FloatingPointPredicate as FP



-----------------------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------------------

double :: Type
double = FloatingPointType DoubleFP

-- TODO :: change this based on the machine, C++ pargma?
-- | Use 64 bit ints
int :: Type
int = IntegerType 64

-- TODO :: increase 16 to whatever the maximum node size can be
portLength :: Type
portLength = IntegerType 16


-- | Construct a 16 bit port space so we can put many inside a node cheaply
-- The pointer points to the beginning of a node and an offset
portPointer :: Type
portPointer = PointerType {
  pointerReferent  = int,
  pointerAddrSpace = AddrSpace 16
}

portType :: Type
portType = StructureType {
  isPacked     = True,
  elementTypes = [ portPointer -- the pointer to the other port
                 , portLength  -- the offset from the base of the node the port is
                 ]
}

-- TODO :: Figure out how to have a union here for all baked in types
dataType :: Type
dataType = int

-- TODO :: Figure out how to get varying data in here
nodeType :: Type
nodeType = StructureType {
  isPacked     = True,
  elementTypes = [ portLength           -- length of this node
                 , ArrayType 0 portType -- variable size array of ports
                 , ArrayType 0 dataType -- variable size array of data the node stores
                 ]
}
-----------------------------------------------------------------------------------------
-- INets
-----------------------------------------------------------------------------------------
