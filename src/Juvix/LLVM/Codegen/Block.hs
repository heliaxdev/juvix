module Juvix.LLVM.Codegen.Block where

import Juvix.LLVM.Codegen.Types
import Juvix.Library hiding (Type, local)
import Juvix.Utility.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST ()
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as Global ()

entry ∷ (HasState "currentBlock" Name m) ⇒ m Name
entry = get @"currentBlock"

modifyBlock ∷
  ( HasState "blocks" (HashMap Name v) m,
    HasState "currentBlock" Name m
  ) ⇒
  v →
  m ()
modifyBlock new = do
  active ← get @"currentBlock"
  modify @"blocks" (Map.insert active new)

current ∷
  ( HasState "blocks" (HashMap Name b) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  m b
current = do
  c ← get @"currentBlock"
  b ← get @"blocks"
  case Map.lookup c b of
    Just x → return x
    Nothing → throw @"err" (NoSuchBlock (show c))

local ∷ Type → Name → Operand
local = LocalReference

instr ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Type →
  Instruction →
  m Operand
instr typ ins = do
  n ← fresh
  let ref = UnName n
  blk ← current
  let i = stack blk
  modifyBlock (blk {stack = (ref := ins) : i})
  pure (local typ ref)

fresh ∷ (HasState "count" b m, Enum b) ⇒ m b
fresh = do
  i ← get @"count"
  put @"count" (succ i)
  pure (succ i)

unnminstr ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Instruction →
  m ()
unnminstr ins = do
  blk ← current
  let i = stack blk
  modifyBlock (blk {stack = (Do ins) : i})

--------------------------------------------------------------------------------
-- Effects
--------------------------------------------------------------------------------
alloca ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Type →
  m Operand
alloca ty = instr ty $ Alloca ty Nothing 0 []

load ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Type →
  Operand →
  m Operand
load typ ptr = instr typ $ Load False ptr Nothing 0 []

store ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Operand →
  Operand →
  m ()
store ptr val = unnminstr $ Store False ptr val Nothing 0 []

bitCast ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Operand →
  Type →
  m Operand
bitCast op typ = instr typ $ BitCast op typ []

--------------------------------------------------------------------------------
-- Sum Type Declarations
--------------------------------------------------------------------------------

-- | creates a variant
createVariant ∷
  ( Eq k,
    Hashable k,
    HasThrow "err" Errors m,
    Show k,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m,
    HasState "typTab" (HashMap k Type) m,
    HasState "varTab" (HashMap k (k, Integer)) m
  ) ⇒
  k →
  m Operand
createVariant variantName = do
  varTable ← get @"varTab"
  typTable ← get @"typTab"
  case Map.lookup variantName varTable of
    Nothing →
      throw @"err" (NoSuchVariant (show variantName))
    Just (sumName, offset) →
      case Map.lookup sumName typTable of
        Nothing →
          throw @"err" (DoesNotHappen ("type " <> show sumName <> "does not exist"))
        Just sumTyp → do
          sum ← alloca sumTyp
          getEle ←
            instr sumTyp $
              GetElementPtr
                { inBounds = True,
                  address = sum,
                  indices =
                    [ ConstantOperand (C.Int 32 0),
                      ConstantOperand (C.Int 32 0)
                    ],
                  metadata = []
                }
          store
            getEle
            -- TODO ∷ store size of variant
            (ConstantOperand (C.Int 8 offset))
          -- TODO ∷ remove the ! call here
          casted ← bitCast sum (typTable Map.! variantName)
          pure casted
