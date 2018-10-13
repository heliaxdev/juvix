module Idris where

import           Idris.AbsSyntax
import           Idris.Core.TT
import           Idris.ElabDecls
import           Idris.Main
import           Idris.ModeCommon
import           Idris.Options
import           Idris.REPL
import           IRTS.Compiler
import           Prelude

import           IdrisCG

cg_main ∷ FilePath → FilePath → IO ()
cg_main input output = runMain $ do
  elabPrims
  loadInputs [input] Nothing
  mainProg <- Just `fmap` elabMain
  ir <- compile (Via IBCFormat "emptycg") output mainProg
  runIO $ codegenEmpty ir
