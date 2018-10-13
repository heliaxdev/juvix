module Interactive where

import           Control.Monad.IO.Class
import           Foundation
import qualified Prelude                      as P
import qualified System.Console.Haskeline     as H
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import           Config
import           Options

import qualified Juvix.Interpreter            as J

interactive ∷ Context → Config → IO ()
interactive ctx _ = do
  func ← J.loadAndRun
  H.runInputT (settings ctx) (mainLoop func)

settings ∷ Context → H.Settings IO
settings ctx = H.Settings {
  H.complete        = H.completeFilename,
  H.historyFile     = Just (contextHomeDirectory ctx <> "/.jvxi_history"),
  H.autoAddHistory  = True
  }

mainLoop ∷ (J.Command → IO J.Response) → H.InputT IO ()
mainLoop func = do
  input ← H.getInputLine "jvxi >> "
  case input of
    Nothing → return ()
    Just i  → do
      case i of
        (':' : special) → handleSpecial special >> mainLoop func
        i → do
          J.Evaluated str ← liftIO (func (J.Eval i))
          H.outputStrLn str
          mainLoop func

handleSpecial ∷ P.String → H.InputT IO ()
handleSpecial str = do
  case str of
    "?" → liftIO (putDoc specialsDoc)
    _   → H.outputStrLn "Unknown special command"

specialsDoc ∷ Doc
specialsDoc = mconcat [
  line,
  mconcat (fmap (flip (<>) line . specialDoc) specials),
  line
  ]

specialDoc ∷ Special → Doc
specialDoc (Special command helpDesc) = mconcat [":", text command, " - ", text helpDesc]

specials ∷ [Special]
specials = [
  Special "?" "Show this help message",
  Special "save" "Dump the interactive state to a file",
  Special "load" "Load interactive state from a file"
  ]

data Special = Special {
  specialCommand  ∷ P.String,
  specialHelpDesc ∷ P.String
}
