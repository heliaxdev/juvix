module Juvix.CodeGen where

import           Control.Monad.Except
import           Control.Monad.Writer
import           Protolude

import           Idris.AbsSyntax
import           Idris.Core.TT            hiding (Name)
import           Idris.ElabDecls
import           Idris.Main
import           Idris.Options
import           IRTS.CodegenCommon
import           IRTS.Compiler
import           IRTS.Lang

import qualified Juvix.Backends.Michelson as M
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
  putText $ "Main decl print: " <> show main
  let decl = snd main
  putText $ "Main decl prettified: " <> prettyPrintValue decl
  let LFun _ _ args body = decl
      expr = LLam args body
  putText $ "Main expr prettified: " <> prettyPrintValue expr
  case runWriter $ runExceptT (M.compileToMichelsonSourceFile expr ∷ (ExceptT M.CompilationError (Writer [M.CompilationLog]) Text)) of
    (res, logs) -> do
      mapM_ (putText . prettyPrintValue) logs
      case res of
        Right output -> do
          writeFile (outputFile ci) output
          putText "Compilation success!"
        Left err -> do
          putText $ "Error during transpilation: " <> prettyPrintValue err
          exitFailure

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
-}
