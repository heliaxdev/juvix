-- | Operations necessary to update nodes
module Juvix.LLVM.Codegen.Graph where

import Juvix.LLVM.Codegen.Block as Block
import Juvix.LLVM.Codegen.Types as Types
import Juvix.Library hiding (Type, local)
import qualified Juvix.Utility.HashMap as Map
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.Type as Type

-- TODO ∷ We need to do a few things
-- 1. getElementPointer the tag
-- Int ⇒
-- \ 1. times offset by this number (* Don't really have to do it! *)
-- \ 2. grab the node from here
-- \ 3. do operation
-- Int* ⇒
-- \ 1. deref the Int
-- \ 2. times the offset by this deref (* Don't really have to do it! *)
-- \ 3. grab node from  here
-- \ 4. do operation

-- TODO ∷ Figure out what types to send in… not entirely sure yet

--------------------------------------------------------------------------------
-- Main Functions
--------------------------------------------------------------------------------

-- take 4 Operand.Operand
--
link ∷
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "moduleDefinitions" [AST.Definition] m,
    HasState "names" Names m,
    HasState "symtab" (Map.HashMap Symbol Operand.Operand) m
  ) ⇒
  m Operand.Operand
link = body >>= define Type.void "link" args
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

isBothPrimary = body >>= define Type.i1 "is_both_primary" args
  where
    args = [(nodeType, "node")]
    body = do
      makeFunction "is_both_primary" args
      undefined -- TODO call to find_edge
      createBlocks

findEdge = body >>= define undefined "find_edge" args
  where
    args = [(nodeType, "node"), (numPorts, "port")]
    body = do
      makeFunction "find_edge" args
      undefined
      createBlocks

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
  tagPtr ← getElementPtr $
    Types.Minimal
      { Types.type' = Type.i1,
        Types.address' = po1,
        Types.indincies' = Block.constant32List [0, 1]
      }
  tag ← load Type.i1 tagPtr
  let branchGen variant variantType extraDeref = do
        casted ← bitCast po1 (varientToType variant)
        valueP ← getElementPtr $
          Types.Minimal
            { Types.type' = variantType,
              Types.address' = casted,
              Types.indincies' = Block.constant32List [0, 1]
            }
        value ← load variantType valueP
        -- Does nothing for the small case
        value ← extraDeref value
        -- Look into nod1 to set
        portsPtr ← getElementPtr $
          Types.Minimal
            { Types.type' = portData,
              Types.address' = no1,
              Types.indincies' = Block.constant32List [0, 2]
            }
        ports ← load portType portsPtr
        -- allocate the new pointer
        p2Ptr ← newPortType no2 po2
        -- Set the port
        portLocation ← getElementPtr $
          Types.Minimal
            { Types.type' = portData,
              Types.address' = ports,
              -- TODO ∷ Ι may have to count size here, I don't think so?
              Types.indincies' =
                [ Operand.ConstantOperand (C.Int 32 0),
                  value
                ]
            }
        store portLocation p2Ptr
        pure portLocation
      intBranch = branchGen numPortsSmall numPortsSmallValue return
      ptrBranch = branchGen numPortsLarge numPortsLargeValuePtr $
        \vPtr → do
          deref2 ← Block.getElementPtr $
            Types.Minimal
              { Types.type' = numPortsLargeValue,
                Types.address' = vPtr,
                Types.indincies' = Block.constant32List [0, 1]
              }
          load numPortsLargeValue deref2
  _ ← generateIf Type.void tag intBranch ptrBranch
  pure ()

newPortType ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (Map.HashMap Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m
  ) ⇒
  Operand.Operand →
  Operand.Operand →
  m Operand.Operand
newPortType node offset = do
  newPort ← alloca portType
  -- This is a ptr to a ptr
  nodePtr ← getElementPtr $
    Types.Minimal
      { Types.type' = nodePointer,
        Types.address' = newPort,
        Types.indincies' = Block.constant32List [0, 1]
      }
  offsetPtr ← getElementPtr $
    Types.Minimal
      { Types.type' = numPorts,
        Types.address' = newPort,
        Types.indincies' = Block.constant32List [0, 2]
      }
  -- allocate pointer to the node
  givenNodePtr ← alloca nodePointer
  placeToStoreNode ← getElementPtr $
    Types.Minimal
      { Types.type' = nodeType,
        Types.address' = givenNodePtr,
        Types.indincies' = Block.constant32List [0, 1]
      }
  -- Store the node to it
  store placeToStoreNode node
  -- now store the pointer to the newPort
  store nodePtr givenNodePtr
  -- store the offset
  store offsetPtr offset
  pure newPort
