module Interpreter where

import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Data.ByteString as BS
import Test.Tasty.Golden     (findByExtension)
import System.Directory   (doesFileExist)
import qualified Tmp.Compile as Compile
import qualified Juvix.Backends.Michelson.DSL.Interpret as Interpret
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import Juvix.Core.Types
import Data.Either (isRight)
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Pipeline as Pipeline
import Control.Arrow (left)
import qualified Michelson.Untyped.Aliases as Alias
import qualified Michelson.Interpret as Interpret

-- Read files
-- Compile
-- Interpret!

-- | 
-- 'expectSuccess' automatically fails on a 'Left' result and
-- 'expectFailure' automatically fails on a 'Right' result.
expectFailure, expectSuccess
  :: (Show a, Show b)
  => (FilePath -> IO (Either a b)) -- ^ The IO action to run that will give either failure or success
  -> FilePath
  -> IO T.TestTree
expectSuccess action file = (\v -> T.testCase file . T.assertBool (show $ fromLeft (panic "Expected Left!") v) $ isRight v) <$> withPrint (action file)
expectFailure action file = (\v -> T.testCase file . T.assertBool (show $ fromRight (panic "Expected Right!") v) $ isLeft v) <$> withPrint (action file)

withPrint :: Show a => IO a -> IO a
withPrint m = do
  a <- m
  print a
  return a

-- Path relative to library/Backends
readJuvixExamples :: IO [FilePath]
readJuvixExamples = findByExtension [".ju"] "../../test/examples"

data PErr = ParseError Pipeline.Error
          | TypecheckErr (PipelineError Param.PrimTy Param.RawPrimVal Param.CompErr)
          | CompileErr Param.CompilationError
          | InterpretErr Interpret.InterpretError
    deriving (Show)

compileJuvixFile :: FilePath -> IO (Either PErr Alias.Contract)
compileJuvixFile fpath = do
  pE <- Compile.parse fpath
  case pE of
    Left parseErr -> pure . Left $ ParseError parseErr
    Right pValue -> do
      tE <- Compile.typecheck pValue
      case tE of
        Left err -> pure . Left $ TypecheckErr err
        Right t -> pure $ left CompileErr (Compile.compile t)

interpretJuvixFile :: Alias.Contract -> Either PErr Interpret.InterpretResult
interpretJuvixFile = left InterpretErr . Interpret.dummyInterpretContract

top :: IO T.TestTree
top = do
  contractFiles <- readJuvixExamples
  tests <- traverse (expectSuccess action) contractFiles
  pure $ T.testGroup "Michelson Interpreter" tests
  where
    action f = do
      cE <- compileJuvixFile f
      case cE of
        Left err -> pure $ Left err
        Right c -> pure $ interpretJuvixFile c
