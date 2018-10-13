module Juvix.Frontend.GHC (
  compileModule
) where

import qualified Control.Exception      as E
import qualified DynFlags
import           Foundation
import qualified GHC
import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Paths              as Paths
import           Paths_juvix
import           Prelude                (FilePath)
import qualified Prelude                as P
import           System.Posix.Files

runGHC ∷ GHC.Ghc a → IO a
runGHC x = do
  {-  This is a silly way to fake a package import. To be reimplemented correctly as a Haskell package in the future.   -}
  libf ← getDataFileName ("lib/Juvix/Lib.hs" ∷ FilePath)
  let root = P.take (P.length libf - P.length ("Lib.hs" ∷ FilePath)) libf
  E.bracket
    (createSymbolicLink root "Juvix")
    (\_ → removeLink "Juvix")
    (\_ → GHC.runGhc (Just Paths.libdir) (do
      oldFlags ← DynFlags.getDynFlags
      let newFlags      = (DynFlags.updOptLevel 3 oldFlags) {
                            GHC.ghcLink = DynFlags.NoLink,
                            GHC.hscTarget = DynFlags.HscNothing
                          }
          setFlags      = []
          unsetFlags    = [DynFlags.Opt_WarnMissingMethods]
          newFlags'     = foldr (flip DynFlags.wopt_set) (foldr (flip DynFlags.wopt_unset) newFlags unsetFlags) setFlags
          {-  Juvix should support all GHC optimizations, which don't change the semantics of Core.
              However, GHC's underlying lazy STG evaluation model is quite different than our Michelson output, so a lot of GHC's optimizations don't help us.
              Some things like inlining and specialization are still quite useful.
              Worthy of more research / tweaking.   -}
          setGFlags     = [DynFlags.Opt_SpecialiseAggressively, DynFlags.Opt_FunToThunk, DynFlags.Opt_SpecConstr, DynFlags.Opt_ExposeAllUnfoldings, DynFlags.Opt_Specialise, DynFlags.Opt_CrossModuleSpecialise, DynFlags.Opt_DictsCheap]
          {-  Full laziness is useless since our evaluation isn't lazy (at the moment).   -}
          unsetGFlags   = [DynFlags.Opt_FullLaziness, DynFlags.Opt_IgnoreInterfacePragmas, DynFlags.Opt_OmitInterfacePragmas]
          newFlags''    = foldr (flip DynFlags.gopt_set) (foldr (flip DynFlags.gopt_unset) newFlags' unsetGFlags) setGFlags
          setOptions    = [LangExt.UnicodeSyntax, LangExt.RebindableSyntax]
          unsetOptions  = [LangExt.ImplicitPrelude]
          finalFlags    = foldr (flip DynFlags.xopt_set) (foldr (flip DynFlags.xopt_unset) newFlags'' unsetOptions) setOptions
      _ ← GHC.setSessionDynFlags finalFlags
      x))

compileModule ∷ FilePath → IO GHC.CoreModule
compileModule = runGHC . GHC.compileToCoreSimplified
