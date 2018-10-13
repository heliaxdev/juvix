{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Juvix.Interpreter where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import qualified DynFlags
import           Foundation
import qualified GHC
import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Paths              as Paths
import qualified GHC.Prim               as GHC
import qualified GHCi
import qualified GHCi.RemoteTypes       as GHCi
import qualified HscMain                as GHC
import qualified Linker                 as GHC
import qualified Outputable             as GHC
import qualified Prelude                as P

import           Juvix.Core.Utility

data Command
  = Load P.String
  | Eval P.String

data Response
  = Loaded
  | Evaluated P.String

newtype InterpreterM = InterpreterM ()

ghcVersion ∷ Int
ghcVersion = __GLASGOW_HASKELL__

loadAndRun ∷ IO (Command → IO Response)
loadAndRun = do
  cmdChan ← newChan
  rspChan ← newChan
  void $ forkIO $ runInterpreter cmdChan rspChan
  return $ \cmd → do
    writeChan cmdChan cmd
    readChan rspChan

{-
runInterpreter ∷ Chan Command → Chan Response → IO a
runInterpreter cmdChan respChan = do
  oldFlags ← GHC.runGhc (Just Paths.libdir) DynFlags.getDynFlags
  let newFlags      = (DynFlags.updOptLevel 0 oldFlags) {
                          GHC.packageFlags = [DynFlags.ExposePackage "juvix" (DynFlags.PackageArg "juvix") (DynFlags.ModRenaming True [])] <> DynFlags.packageFlags oldFlags,
                          GHC.ghcMode = GHC.CompManager,
                          GHC.ghcLink = GHC.LinkInMemory,
                          GHC.hscTarget = DynFlags.HscInterpreted
                        }
      setFlags      = []
      unsetFlags    = []
      newFlags'     = foldr (flip DynFlags.wopt_set) (foldr (flip DynFlags.wopt_unset) newFlags unsetFlags) setFlags
      setGFlags     = [GHC.Opt_AutoLinkPackages]
      unsetGFlags   = []
      newFlags''    = foldr (flip DynFlags.gopt_set) (foldr (flip DynFlags.gopt_unset) newFlags' unsetGFlags) setGFlags
      setOptions    = [LangExt.UnicodeSyntax, LangExt.RebindableSyntax]
      unsetOptions  = [LangExt.ImplicitPrelude]
      finalFlags    = foldr (flip DynFlags.xopt_set) (foldr (flip DynFlags.xopt_unset) newFlags'' unsetOptions) setOptions
  hscEnv ← GHC.newHscEnv finalFlags
  forever $ do
    cmd ← liftIO (readChan cmdChan)
    resp ← case cmd of
      Eval expr → do
        fval ← GHCi.EvalThis |<< (GHCi.mkFinalizedHValue hscEnv =<< GHCi.mkRemoteRef (GHCi.HValue (GHC.unsafeCoerce# expr)))
        GHCi.EvalComplete _ (GHCi.EvalSuccess [result]) ← GHCi.evalStmt hscEnv False fval
        value ← GHCi.wormhole finalFlags result
        return (Evaluated (P.show value))
    liftIO (writeChan respChan resp)
-}

runInterpreter ∷ Chan Command → Chan Response → IO a
runInterpreter cmdChan respChan = GHC.runGhc (Just Paths.libdir) $ do
  oldFlags ← DynFlags.getDynFlags
  --mod ← GHC.findModule (GHC.mkModuleName "Juvix") Nothing
  let newFlags      = (DynFlags.updOptLevel 0 oldFlags) {
                          GHC.packageFlags = [DynFlags.ExposePackage "juvix" (DynFlags.PackageArg "juvix") (DynFlags.ModRenaming True [])] <> DynFlags.packageFlags oldFlags,
                          GHC.ghcMode = GHC.CompManager,
                          GHC.ghcLink = GHC.LinkInMemory,
                          GHC.hscTarget = DynFlags.HscInterpreted
                        }
      setFlags      = []
      unsetFlags    = []
      newFlags'     = foldr (flip DynFlags.wopt_set) (foldr (flip DynFlags.wopt_unset) newFlags unsetFlags) setFlags
      setGFlags     = [GHC.Opt_AutoLinkPackages]
      unsetGFlags   = []
      newFlags''    = foldr (flip DynFlags.gopt_set) (foldr (flip DynFlags.gopt_unset) newFlags' unsetGFlags) setGFlags
      setOptions    = [LangExt.UnicodeSyntax, LangExt.RebindableSyntax]
      unsetOptions  = [LangExt.ImplicitPrelude]
      finalFlags    = foldr (flip DynFlags.xopt_set) (foldr (flip DynFlags.xopt_unset) newFlags'' unsetOptions) setOptions
  toLink ← GHC.setSessionDynFlags finalFlags
  hscEnv ← GHC.getSession
  GHC.setTargets []
  _ ← GHC.load GHC.LoadAllTargets
  liftIO $ GHC.linkPackages hscEnv toLink

  --Just inf ← GHC.getModuleInfo mod
  let mod = GHC.mainModIs newFlags
  --liftIO $ P.print $ GHC.showSDoc newFlags $ GHC.ppr mod

  {-
  GHC.addTarget (GHC.Target (GHC.TargetModule (GHC.mkModuleName "Juvix")) True Nothing)
  GHC.depanal [] True
  GHC.load GHC.LoadAllTargets
  -}

  {-
  summary ← GHC.getModSummary (GHC.mkModuleName "Juvix")
  parsed ← GHC.parseModule summary
  typechecked ← GHC.typecheckModule parsed
  GHC.loadModule typechecked
  liftIO $ P.print $ GHC.showSDoc newFlags $ GHC.ppr mod
  -}

  --GHC.setContext [GHC.IIModule (GHC.mkModuleName "Juvix")]
  --GHC.setContext [GHC.IIModule (GHC.mkModuleName "Foundation")]
  --GHC.setContext [GHC.IIModule (GHC.mkModuleName "Prelude")]

  forever $ do
    cmd ← liftIO (readChan cmdChan)
    resp ← case cmd of
      Eval expr → do
        compiled ← GHC.dynCompileExpr expr
        return (Evaluated (P.show compiled))
    liftIO (writeChan respChan resp)
