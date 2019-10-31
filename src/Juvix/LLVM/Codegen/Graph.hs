-- | Operations necessary to update nodes
module Juvix.LLVM.Codegen.Graph where

import Juvix.LLVM.Codegen.Block
import qualified Juvix.LLVM.Codegen.Constants as Constants
import Juvix.LLVM.Codegen.Types
import Juvix.Library hiding (Type, local)
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

-- TODO ∷ We need to do a few things
-- 1. getElementPointer the tag
-- Int ⇒
-- \ 1. times offset by this number
-- \ 2. grab the node from here
-- \ 3. do operation
-- Int* ⇒
-- \ 1. deref the Int
-- \ 2. times the offset by this deref
-- \ 3. grab node from  here
-- \ 4. do operation

-- TODO ∷ Figure out what types to send in… not entirely sure yet

--------------------------------------------------------------------------------
-- Main Functions
--------------------------------------------------------------------------------

-- take 4 Operand.Operand
--
link = do
  return $
    define Constants.int "link" args undefined
  where
    args =
      ( [ (nodeType, "node_1"),
          (numPorts, "port_1"),
          (nodeType, "node_2"),
          (numPorts, "port_2")
        ] ∷
          [(Type.Type, Name.Name)]
      )
    -- TODO ∷ Abstract most of the logic in this function
    body =
      makeFunction "link" args $ do
        undefined

-- preform offsets
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

makeFunction name args body = do
  entry ← addBlock name
  _ ← setBlock entry
  -- Maybe not needed?
  traverse_
    ( \(typ, nam) → do
        var ← alloca typ
        store var (local typ nam)
        assign nam var
    )
    args
  body >>= ret
