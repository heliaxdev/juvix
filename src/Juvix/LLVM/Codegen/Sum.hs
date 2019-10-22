-- | Provides a mechanism for defining Sum types
module Juvix.LLVM.Codegen.Sum where

import Juvix.LLVM.Codegen.Shared
import Juvix.Library hiding (Type)
import qualified Juvix.Utility.HashMap as Map
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
data VariantInfo
  = Variant
      { size ∷ Size,
        name ∷ Symbol,
        typ' ∷ Type
      }

-----------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------

-- TODO ∷ Optimize this using hacker's delight

-- | 'sumSize' takes a list of variants and creates the array type to hold the
-- largest variant
sumSize ∷ [VariantInfo] → Type
sumSize variants
  -- Assume 8 is the smallest allocation type
  | largest `mod` 16 == 0 = ArrayType (divLarg 16) i16
  | otherwise = ArrayType (divLarg 8) i8
  where
    largest = maximumDef 0 (size <$> variants)
    divLarg = div (fromIntegral largest)

-- TODO ∷ optimize this using bit-wise functions
-- Think hacker's delight
-- Also should we allow smaller than i8?

-- | 'tagSize' takes a list of variants and figures out what the size of the tag should be
tagSize ∷ [VariantInfo] → Type
tagSize variants
  | len < 256 = i8
  | len < 65536 = i16
  | len < 4294967296 = i32
  | otherwise = i64
  where
    len = length variants

createVariantName ∷ Symbol → Symbol → Symbol
createVariantName sumName varName = sumName <> "-" <> varName

-----------------------------------------------------------------------------------------
-- Important functions
-----------------------------------------------------------------------------------------

createSum ∷ [VariantInfo] → Type
createSum variants =
  StructureType
    { isPacked = False,
      elementTypes =
        [tag, arrSize]
    }
  where
    tag = tagSize variants
    arrSize = sumSize variants

-- | 'insertSums' creates a sum type, and inserts the new types into the symbol table
-- and the variant table for all the newly created variants
insertSums ∷
  Symbol →
  [VariantInfo] →
  SymbolTable →
  VariantToType →
  (SymbolTable, VariantToType)
insertSums sumName variants symTbl varTbl = (newSymTbl, newVarTbl)
  where
    sum' = createSum variants
    symTbl' =
      Map.insert sumName (LocalReference sum' (mkName (unintern sumName))) symTbl
    newVarTbl =
      foldr
        ( \(Variant {name = n}) tbl →
            Map.insert (createVariantName sumName n) sumName tbl
        )
        varTbl
        variants
    newSymTbl =
      foldr
        ( \(Variant _s n t) tbl →
            let name = createVariantName sumName n
                operand = LocalReference t (mkName (unintern name))
             in Map.insert name operand tbl
        )
        symTbl'
        variants

-- | creates a variant
createVariant = undefined
