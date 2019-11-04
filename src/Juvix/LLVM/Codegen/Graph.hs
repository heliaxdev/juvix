-- | Operations necessary to update nodes
module Juvix.LLVM.Codegen.Graph where

import Juvix.LLVM.Codegen.Block as Block
import qualified Juvix.LLVM.Codegen.Constants as Constants
import qualified Juvix.LLVM.Codegen.Shared as Shared
import Juvix.LLVM.Codegen.Types
import Juvix.Library hiding (Type, local)
import qualified Juvix.Utility.HashMap as Map
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Instruction as Instruction
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
  (b ∷ [AST.BasicBlock]) ← body
  return $
    define Constants.int "link" args b
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
    body = do
      makeFunction "link" args
      setPort ("node_1", "port_1") ("node_2", "port_2")
      setPort ("node_2", "port_2") ("node_1", "port_1")
      _ ← retNull
      createBlocks

-- preform offsets
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

makeFunction ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  Symbol →
  [(Type.Type, Name.Name)] →
  m ()
makeFunction name args = do
  entry ← addBlock name
  _ ← setBlock entry
  -- Maybe not needed?
  traverse_
    ( \(typ, nam) → do
        var ← alloca typ
        store var (local typ nam)
        assign (nameToSymbol nam) var
    )
    args

setPort ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Names m,
    HasState "symtab" SymbolTable m
  ) ⇒
  (Name.Name, Name.Name) →
  (Name.Name, Name.Name) →
  m ()
setPort (n1, p1) (n2, p2) = do
  (no1, po1) ← (,) <$> Block.externf n1 <*> Block.externf p1
  (no2, po2) ← (,) <$> Block.externf n2 <*> Block.externf p2
  -- Is a pointer to tagPtr
  tagPtr ← Block.instr Type.i1 $
    Instruction.GetElementPtr
      { Instruction.inBounds = False,
        Instruction.address = po1,
        Instruction.indices =
          [ Operand.ConstantOperand (C.Int 32 0),
            Operand.ConstantOperand (C.Int 32 1)
          ],
        Instruction.metadata = []
      }
  tag ← load Type.i1 tagPtr
  let intBranch = do
        casted ← bitCast po1 (varientToType numPortsSmall)
        valueP ← Block.instr numPortsSmallValue $
          Instruction.GetElementPtr
            { Instruction.inBounds = False,
              Instruction.address = casted,
              Instruction.indices =
                [ Operand.ConstantOperand (C.Int 32 0),
                  Operand.ConstantOperand (C.Int 32 1)
                ],
              Instruction.metadata = []
            }
        value ← load numPortsSmallValue valueP
        -- Look into nod1 to set
        portsPtr ← Block.instr portData $
          Instruction.GetElementPtr
            { Instruction.inBounds = False,
              Instruction.address = no1,
              Instruction.indices =
                [ Operand.ConstantOperand (C.Int 32 0),
                  Operand.ConstantOperand (C.Int 32 2)
                ],
              Instruction.metadata = []
            }
        ports ← load portType portsPtr
        -- allocate the new pointer
        p2Ptr ← newPortType no2 po2
        -- Set the port
        portLocation ← Block.instr portData $
          Instruction.GetElementPtr
            { Instruction.inBounds = False,
              Instruction.address = ports,
              -- TODO ∷ Ι may have to count size here, I don't think so?
              Instruction.indices =
                [ Operand.ConstantOperand (C.Int 32 0),
                  value
                ],
              Instruction.metadata = []
            }
        store portLocation p2Ptr
        pure portLocation
      ptrBranch = do undefined
  generateIf Type.i1 tag intBranch ptrBranch
  pure ()

newPortType node offset = do
  p2Ptr ← alloca portType
  -- This is a ptr to a ptr
  nodePtr ← Block.instr nodePointer $
    Instruction.GetElementPtr
      { Instruction.inBounds = False,
        Instruction.address = p2Ptr,
        Instruction.indices =
          [ Operand.ConstantOperand (C.Int 32 0),
            Operand.ConstantOperand (C.Int 32 1)
          ],
        Instruction.metadata = []
      }
  offsetPtr ← Block.instr nodePointer $
    Instruction.GetElementPtr
      { Instruction.inBounds = False,
        Instruction.address = p2Ptr,
        Instruction.indices =
          [ Operand.ConstantOperand (C.Int 32 0),
            Operand.ConstantOperand (C.Int 32 1)
          ],
        Instruction.metadata = []
      }
  pure p2Ptr
