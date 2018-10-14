module Juvix.CodeGen where

import           Protolude

import           Idris.AbsSyntax
import           Idris.Core.TT      hiding (Name)
import           Idris.ElabDecls
import           Idris.Main
import           Idris.Options
import           IRTS.CodegenCommon
import           IRTS.Compiler
import           IRTS.Lang

import           Juvix.Lang
import           Juvix.Utility

data Opts = Opts {inputs :: [FilePath],
                  output :: FilePath }

showUsage ∷ IO ()
showUsage = do putText "A code generator which is intended to be called by the compiler, not by a user."
               putText "Usage: idris-codegen-juvix <ibc-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts ∷ IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "main.jvx") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs)      = process (opts { inputs = x:inputs opts }) xs
    process opts []          = opts

codeGenSdecls ∷ CodeGenerator
codeGenSdecls ci = do
  putText $ "Output file : " <> show (outputFile ci)
  let decls = liftDecls ci
  putText $ "Number of decls: " <> show (length decls)
  let main = findMain decls
  putText $ "Main: " <> show main
  putText $ "Main PP: " <> prettyPrintValue (snd main)

findMain ∷ [(Name, LDecl)] → (Name, LDecl)
findMain decls = let Just f = head $ filter (\(name, _) -> name == NS (UN "main") ["Main"]) decls in f

sdeclMain ∷ Opts → Idris ()
sdeclMain opts = do elabPrims
                    _ <- loadInputs (inputs opts) Nothing
                    mainProg <- elabMain
                    ir <- compile (Via IBCFormat "juvix") (output opts) (Just mainProg)
                    runIO $ codeGenSdecls ir

main ∷ IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else runMain (sdeclMain opts)

{-
module IRTS.CodegenCommon where

data CodegenInfo = CodegenInfo {
    outputFile    :: String
  , outputType    :: OutputType
  , targetTriple  :: String
  , targetCPU     :: String
  , includes      :: [FilePath]
  , importDirs    :: [FilePath]
  , compileObjs   :: [String]
  , compileLibs   :: [String]
  , compilerFlags :: [String]
  , debugLevel    :: DbgLevel
  , simpleDecls   :: [(Name, SDecl)] -- most low level
  , defunDecls    :: [(Name, DDecl)]
  , liftDecls     :: [(Name, LDecl)]
  , interfaces    :: Bool
  , exportDecls   :: [ExportIFace]
  , ttDecls       :: [(Name, TTDecl)]
  }

type CodeGenerator = CodegenInfo -> IO ()
-}
