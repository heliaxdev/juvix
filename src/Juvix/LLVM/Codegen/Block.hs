module Juvix.LLVM.Codegen.Block where

import Data.ByteString.Short
import Juvix.LLVM.Codegen.Shared
import Juvix.LLVM.Codegen.Types
import Juvix.Library hiding (Type, local)
import Juvix.Utility.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as Global

--------------------------------------------------------------------------------
-- Codegen Operations
--------------------------------------------------------------------------------

fresh ∷ (HasState "count" b m, Enum b) ⇒ m b
fresh = do
  i ← get @"count"
  put @"count" (succ i)
  pure (succ i)

emptyBlock ∷ Int → BlockState
emptyBlock i = BlockState i [] Nothing

createBlocks ∷ HasState "blocks" (HashMap Name BlockState) f ⇒ f [BasicBlock]
createBlocks = fmap makeBlock . sortBlocks . Map.toList <$> get @"blocks"

makeBlock ∷ (Name, BlockState) → BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = undefined -- error $ "Block has no terminator: " <> show l

sortBlocks ∷ [(a, BlockState)] → [(a, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

entryBlockName ∷ IsString p ⇒ p
entryBlockName = "entry"

--------------------------------------------------------------------------------
-- Module Level
--------------------------------------------------------------------------------

emptyModule ∷ ShortByteString → Module
emptyModule label = AST.defaultModule {moduleName = label}

addDefn ∷ HasState "moduleDefinitions" [Definition] m ⇒ Definition → m ()
addDefn d = modify @"moduleDefinitions" (<> [d])

define ∷
  HasState "moduleDefinitions" [Definition] m ⇒
  Type →
  Symbol →
  [(Type, Name)] →
  [BasicBlock] →
  m Operand
define retty label argtys body = do
  addDefn
    $ GlobalDefinition
    $ functionDefaults
      { Global.parameters = params,
        -- Figure out which is best!
        Global.callingConvention = CC.GHC,
        Global.returnType = retty,
        Global.basicBlocks = body,
        Global.name = (internName label)
      }
  return
    $ ConstantOperand
    $ C.GlobalReference
      (PointerType (FunctionType retty (fst <$> argtys) False) (AddrSpace 0))
      (internName label)
  where
    params = ((\(ty, nm) → Parameter ty nm []) <$> argtys, False)

--------------------------------------------------------------------------------
-- Block Stack
--------------------------------------------------------------------------------

entry ∷ (HasState "currentBlock" Name m) ⇒ m Name
entry = get @"currentBlock"

getBlock ∷ (HasState "currentBlock" Name m) ⇒ m Name
getBlock = entry

addBlock ∷
  ( HasState "blockCount" Int m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "names" Names m
  ) ⇒
  Symbol →
  m Name
addBlock bname = do
  bls ← get @"blocks"
  ix ← get @"blockCount"
  nms ← get @"names"
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
      name = internName qname
  put @"blocks" (Map.insert name new bls)
  put @"blockCount" (succ ix)
  put @"names" supply
  return name

setBlock ∷ HasState "currentBlock" Name m ⇒ Name → m Name
setBlock bName = bName <$ put @"currentBlock" bName

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

externf ∷
  ( HasState "symtab" SymbolTable m,
    HasThrow "err" Errors m
  ) ⇒
  Name →
  m Operand
externf name = getvar (nameToSymbol name)

nameToSymbol ∷ Name → Symbol
nameToSymbol (UnName n) = (intern (show n))
nameToSymbol (Name n) = (intern (show n))

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

terminator ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Named Terminator →
  m (Named Terminator)
terminator trm = do
  blk ← current
  modifyBlock (blk {term = Just trm})
  return trm

fdiv,
  fadd,
  fsub,
  fmul ∷
    ( HasThrow "err" Errors m,
      HasState "blocks" (HashMap Name BlockState) m,
      HasState "count" Word m,
      HasState "currentBlock" Name m
    ) ⇒
    Type →
    Operand →
    Operand →
    m Operand
fdiv t a b = instr t $ FDiv noFastMathFlags a b []
fadd t a b = instr t $ FAdd noFastMathFlags a b []
fsub t a b = instr t $ FSub noFastMathFlags a b []
fmul t a b = instr t $ FMul noFastMathFlags a b []

--------------------------------------------------------------------------------
-- Control Flow
--------------------------------------------------------------------------------

ret ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Operand →
  m (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retNull ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  m (Named Terminator)
retNull = terminator $ Do $ Ret Nothing []

cbr ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Operand →
  Name →
  Name →
  m (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi ∷
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m
  ) ⇒
  Type →
  [(Operand, Name)] →
  m Operand
phi ty incoming = instr ty $ Phi ty incoming []

switch ∷
  ( HasState "blocks" (HashMap Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) ⇒
  Operand →
  Name →
  [(C.Constant, Name)] →
  m (Named Terminator)
switch val default' dests = terminator $ Do $ Switch val default' dests []

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
  ( HasThrow "err" Errors m,
    HasState "blocks" (HashMap Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    Foldable t
  ) ⇒
  Symbol →
  t Operand →
  m Operand
createVariant variantName args = do
  varTable ← get @"varTab"
  typTable ← get @"typTab"
  case Map.lookup variantName varTable of
    Nothing →
      throw @"err" (NoSuchVariant (show variantName))
    Just
      ( S
          { sum' = sumName,
            offset = offset,
            tagSize' = tag
          }
        ) →
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
              (ConstantOperand (C.Int tag (toInteger offset)))
            -- TODO ∷ remove the ! call here
            let varType = typTable Map.! variantName
            casted ← bitCast sum varType
            foldM_
              ( \i inst → do
                  ele ←
                    instr varType $
                      GetElementPtr
                        { inBounds = True,
                          address = casted,
                          indices =
                            [ ConstantOperand (C.Int 32 0),
                              ConstantOperand (C.Int 32 i)
                            ],
                          metadata = []
                        }
                  store ele inst
                  pure (succ i)
              )
              1 -- not 0, as 0 is reserved for the tag that was set
              args
            pure casted

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign ∷
  (HasState "symtab" SymbolTable m) ⇒
  Symbol →
  Operand →
  m ()
assign var x = do
  modify @"symtab" (Map.insert var x)

getvar ∷
  ( HasState "symtab" SymbolTable m,
    HasThrow "err" Errors m
  ) ⇒
  Symbol →
  m Operand
getvar var = do
  syms ← get @"symtab"
  case Map.lookup var syms of
    Just x →
      return x
    Nothing →
      throw @"err"
        ( VariableNotInScope $
            "Local variable not in scope:"
              <> "\n syms: "
              <> show syms
              <> "\n var: "
              <> show var
        )

internName ∷ Symbol → Name
internName = mkName . unintern
