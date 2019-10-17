module Juvix.LLVM.Codegen.Block where

import Juvix.LLVM.Codegen.Types
import Juvix.Library hiding (Type, local)
import Juvix.Utility.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST ()
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C ()
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
