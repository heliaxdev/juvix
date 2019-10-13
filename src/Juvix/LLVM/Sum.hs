-- | Provides a mechanism for defining Sum types
module Juvix.LLVM.Sum where

import Juvix.Library hiding (Type)
import qualified Juvix.Utility.HashMap ()
import LLVM.AST
import qualified LLVM.AST as AST ()
import qualified LLVM.AST.Constant as C ()
import LLVM.AST.Global as Global ()
import LLVM.AST.Type

-----------------------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------------------

type Size = Int

-- | Data needed to make a Variant
data VarientInfo
  = Variant
      { size ∷ Size,
        name ∷ Symbol,
        typ' ∷ Type
      }

-- TODO ∷ Optimize this using hacker's delight

-- | 'sumSize' takes a list of variants and creates the array type to hold the
-- largest variant
sumSize ∷ [VarientInfo] → Type
sumSize variants
  -- Assume 8 is the smallest allocation type
  | largest `mod` 16 == 0 = ArrayType (divLarg 16) i16
  | otherwise = ArrayType (divLarg 8) i8
  where
    largest = maximumDef 0 (size <$> variants)
    divLarg = div (fromIntegral largest)

-- TODO ∷ optimize this using bit-wise functions
-- Think hacker's delight

-- | 'tagSize' takes a list of variants and figures out what the size of the tag should be
tagSize ∷ [VarientInfo] → Type
tagSize variants
  | len < 256 = i8
  | len < 65536 = i16
  | len < 4294967296 = i32
  | otherwise = i64
  where
    len = length variants

createSum ∷ [VarientInfo] → Type
createSum variants = StructureType
  { isPacked = False,
    elementTypes =
      [ tag,
        arrSize
      ]
  }
  where
    tag = tagSize variants
    arrSize = sumSize variants
