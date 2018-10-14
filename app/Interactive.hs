module Interactive where

import           Control.Monad.IO.Class
import qualified Data.Text                    as T
import           Protolude
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
        (':' : special) → handleSpecial (T.pack special) >> mainLoop func
        inp → do
          H.outputStrLn inp
          mainLoop func

handleSpecial ∷ Text → H.InputT IO ()
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
specialDoc (Special command helpDesc) = text $ T.unpack $ mconcat [":", command, " - ", helpDesc]

specials ∷ [Special]
specials = [
  Special "?" "Show this help message",
  Special "save" "Dump the interactive state to a file",
  Special "load" "Load interactive state from a file"
  ]

data Special = Special {
  specialCommand  ∷ Text,
  specialHelpDesc ∷ Text
}
