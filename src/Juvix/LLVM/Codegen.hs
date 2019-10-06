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



-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

double :: Type
double = FloatingPointType DoubleFP

-- TODO :: change this based on the machine, C++ pargma?
-- | Use 64 bit ints
int :: Type
int = IntegerType 64

-- TODO :: increase 5 to whatever the maximum size an argument can be
portLength :: Type
portLength = IntegerType 5


-- | Construct a 16 bit port space so we can put many inside a node cheaply
portType :: Type
portType = PointerType {
  pointerReferent  = int,
  pointerAddrSpace = AddrSpace 16
}

-- TODO :: Figure out how to have a union here for all baked in types
dataType :: Type
dataType = int

-- TODO :: Figure out how to get varying data in here
nodeType :: Type
nodeType = StructureType {
  isPacked     = True,
  elementTypes = [ int                  -- length of this node
                 , ArrayType 0 portType -- variable size array of ports
                 , ArrayType 0 dataType -- variable size array of data the node stores
                 ]
}
-------------------------------------------------------------------------------
-- INets
-------------------------------------------------------------------------------
