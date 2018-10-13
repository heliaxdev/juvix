module Options where

import           Foundation
import           Options.Applicative
import qualified Prelude             as P

data Context = Context {
  contextWorkingDirectory ∷ P.FilePath,
  contextHomeDirectory    ∷ P.FilePath
}

data Options = Options {
  optionsCommand    ∷ Command,
  optionsConfigPath ∷ P.FilePath
}

data Command
  = Transpile P.String P.String
  | Interactive
  | Version
  | Eval P.String Bool
  | Config
  | Idris P.String P.String

options ∷ Context → Parser Options
options ctx = Options <$> commandOptions <*> configOptions ctx

configOptions ∷ Context → Parser P.FilePath
configOptions ctx = strOption (short 'c' <> long "config" <> metavar "PATH" <> value (contextWorkingDirectory ctx <> "/juvix.yaml") <> showDefault <> help "Path to YAML configuration file")

commandOptions ∷ Parser Command
commandOptions = subparser (
      command "transpile" (info compileOptions (progDesc "Transpile a source file"))
  <>  command "interactive" (info interactiveOptions (progDesc "Launch interactive mode"))
  <>  command "version" (info versionOptions (progDesc "Display version information"))
  <>  command "eval" (info evalOptions (progDesc "Evaluate an expression in the dynamic environment"))
  <>  command "config" (info configurationOptions (progDesc "Adjust runtime configuration or generate an example config file"))
  <>  command "idris" (info idrisOptions (progDesc "Transpile Idris"))
  )

compileOptions ∷ Parser Command
compileOptions = Transpile <$> argument str (metavar "JUVIXFILE" <> help "Juvix file to compile (suggested extension: .jvx)") <*> argument str (metavar "MICHELSONFILE" <> help "Michelson file to write to (suggested extension: .tz)")

interactiveOptions ∷ Parser Command
interactiveOptions = pure Interactive

versionOptions ∷ Parser Command
versionOptions = pure Version

evalOptions ∷ Parser Command
evalOptions = Eval <$> argument str (metavar "EXPRESSION" <> help "Expression to evaluate") <*> switch (short 'j' <> long "json" <> help "Attempt to convert evaluation result to JSON")

configurationOptions ∷ Parser Command
configurationOptions = pure Config

idrisOptions ∷ Parser Command
idrisOptions = Idris <$> argument str (metavar "INPUT" <>  help "Input file") <*> argument str (metavar "OUTPUT" <> help "Output file")
