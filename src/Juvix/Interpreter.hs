module Juvix.Interpreter where

import           Protolude

data Command
  = Load Text
  | Eval Text

data Response
  = Loaded

loadAndRun ∷ IO (Command → IO Response)
loadAndRun = do
  return $ const undefined
