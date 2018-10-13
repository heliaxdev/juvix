module Juvix.CodeGen where

import           Protolude
import           System.Environment
import           System.Exit

import           Idris.AbsSyntax
import           Idris.ElabDecls
import           Idris.Main
import           Idris.Options
import           IRTS.Compiler

import           Idris.Core.TT
import           IRTS.CodegenCommon
import           IRTS.Simplified


data Opts = Opts {inputs :: [FilePath],
                  output :: FilePath }

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
  putText "codegen sdecls"
  let ofn = outputFile ci
  putText $ "outputFile : " <> show ofn
  putText $ "Decls: " <> show (length (simpleDecls ci))
  putText $ foldl (\x y -> x <> "\n" <> y) "" $ fmap show $ simpleDecls ci
  --let res = foldl (\x y-> show x <> "\n" <> show y) "" (fmap (\(a,b)->sdecls2str a b) $ simpleDecls ci)
  --putText res

--use IRTS.Simplified decl
-- data SDecl = SFun Name [Name] Int SExp
sdecls2str ∷ Name → SDecl → Text
sdecls2str fname aaa@(SFun _ fArgs i fBody) = (show fname) <> "--->" <> (show aaa)

sexp2str ∷ SExp → Text
sexp2str x = ""

sdeclMain ∷ Opts → Idris ()
sdeclMain opts = do elabPrims
                    loadInputs (inputs opts) Nothing
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
