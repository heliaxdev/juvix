-- | Operations necessary to update nodes
module Juvix.LLVM.Codegen.Graph where

import Juvix.LLVM.Codegen.Block
import Juvix.LLVM.Codegen.Types
import Juvix.Library hiding (Type, local)


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
