{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Development.GitRev
import           Foundation
import           Options.Applicative
import           System.Directory
import           System.Exit
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import           Text.RawString.QQ

import           Config
import           Idris
import           Interactive
import qualified Juvix                        as J
import           Options

context ∷ IO Context
context = do
  pwd   ← getCurrentDirectory
  home  ← getHomeDirectory
  return (Context pwd home)

main ∷ IO ()
main = do
  ctx ← context
  let opts = info (options ctx <**> helper) (fullDesc <> headerDoc (Just aboutDoc))
  run ctx =<< execParser opts

disclaimerDoc ∷ Doc
disclaimerDoc = mconcat [
  "This is ", red "experimental", " software – use at your own risk.",
  line,
  "The Tezos protocol is not finalized and Juvix may diverge from the canonical OCaml implementation in unexpected ways."
  ]

aboutDoc ∷ Doc
aboutDoc = mconcat [
  text "Juvix smart contract transpiler & toolkit",
  line,
  text "(c) Christopher Goes 2017-2018 • https://juvix.org",
  line,
  disclaimerDoc
  ]

versionDoc ∷ Doc
versionDoc = mconcat [
  aboutDoc,
  line <> line,
  mconcat ["Prerelease version.", line],
  mconcat ["Built from branch ", white $(gitBranch), " at commit ", magenta $(gitHash), " (commit date ", cyan $(gitCommitDate), ").", line]
  ]

interactiveDoc ∷ Doc
interactiveDoc = mconcat [
  aboutDoc,
  line,
  white [r|
     | \ \   / /\ \/ (_)
  _  | |\ \ / /  \  /| |
 | |_| | \ V /   /  \| |
  \___/   \_/   /_/\_\_|
|],
  mconcat [line, "Juvix interactive alpha. Currently supported backends: Tezos alphanet. GHC version: ", int J.ghcVersion, ".", line, "Enter :? for help. Enter :tutorial for an interactive tutorial.", line]
  ]

run ∷ Context → Options → IO ()
run ctx (Options cmd configPath) = do
  maybeConf ← loadConfig configPath
  case maybeConf of
    Nothing   → do
      T.putStrLn ("Error parsing configuration file " `T.append` T.pack configPath)
      exitFailure
    Just conf → do
      case cmd of
        Idris input output -> do
          cg_main input output
          exitSuccess
        Interactive → do
          putDoc interactiveDoc
          T.putStrLn ("Loaded runtime configuration from " `T.append` T.pack configPath `T.append` "\n")
          interactive ctx conf
          exitSuccess
        Version → do
          putDoc versionDoc
          exitSuccess
        (Transpile i o) → do
          r ← J.compileToTz i True
          case r of
            Right r → do
              T.writeFile o r
              exitSuccess
            Left er → do
              T.putStrLn ("Error during transpilation: " `T.append` J.prettyPrintValue er)
              exitFailure
        _ -> do
          T.putStrLn "Not yet implemented!"
          exitFailure
