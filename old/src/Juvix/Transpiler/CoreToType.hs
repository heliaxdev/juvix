module Juvix.Transpiler.CoreToType (
  coreToType
) where

import           Foundation

import qualified Juvix.Backends.Michelson    as M
import           Juvix.Core.CompilerTypes
import qualified Juvix.Core.GHC              as GHC
import           Juvix.Transpiler.TypeToType

coreToType ∷ GHC.CoreExpr → CompilerM M.Type M.Type
coreToType = typeToType . GHC.exprType
