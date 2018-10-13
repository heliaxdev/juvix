module Juvix.Frontend.Idris.CodeGen where

import           Foundation
import qualified Prelude                  as P

import qualified Idris.Core.TT            as I
import qualified IRTS.CodegenCommon       as I
import qualified IRTS.Lang                as I

import qualified Juvix.Backends.Michelson as M

codegenEmpty ∷ I.CodeGenerator
codegenEmpty ci = do
  let ldecls = P.filter ((\case (I.NS _ ["Contract"]) -> True; _ -> False) . fst) (I.defunDecls ci)
  --P.print (I.ttDecls ci)
  P.print (I.exportDecls ci)
  P.print ldecls
  return ()

-- LExp to Michelson
--gen ∷ LExp → Bool
--gen = undefined

{-
 - 1. Figure out how to run this function when Juvix is invoked
 - 2. Pick a level (LExp) and write the Michelson transpiler
 - 3. Test / reevaluate choices (optimization? primitive passthrough OK?)
 -
 - Priority: testable Idris ⇒ Michelson, REPL!
 -}
