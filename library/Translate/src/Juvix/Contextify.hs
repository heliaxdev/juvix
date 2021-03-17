module Juvix.Contextify (fullyContextify, contextify, op) where

import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.FrontendContextualise.Contextify.ResolveOpenInfo as ResolveOpen
import qualified Juvix.FrontendContextualise.Contextify.Sexp as ContextSexp
import qualified Juvix.FrontendContextualise.Contextify.Types as Contextify
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp

type RunM =
  ExceptT Context.PathError IO

newtype M a = M (RunM a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasThrow "left" Context.PathError) via MonadError RunM

data ResolveErr
  = Path Context.PathError
  | Resolve ResolveOpen.Error
  deriving (Show, Eq)

type PathError t = Either Context.PathError t

--------------------------------------------------------------------------------
-- Main functionality
--------------------------------------------------------------------------------

op = undefined

-- | @fullyContextify@ runs @contextifyS@ along while running the
-- algorithm that resolves the opens to the modules in which they
-- should come from
fullyContextify ::
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  IO (Either ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
fullyContextify ts = do
  cont <- contextify ts
  case cont of
    Left e -> pure $ Left $ Path e
    Right x -> do
      addedOpens <- uncurry ResolveOpen.run x
      case addedOpens of
        Left e -> pure $ Left $ Resolve e
        Right x ->
          pure $ Right x

contextify ::
  NonEmpty (NameSymbol.T, [Sexp.T]) ->
  IO (PathError (Contextify.ContextSexp, [ResolveOpen.PreQualified]))
contextify t@((sym, _) :| _) = do
  emptyCtx <- Context.empty sym
  runM $
    foldM resolveOpensS (emptyCtx, []) (addTop <$> t)

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- we get the opens
resolveOpensS ::
  (MonadIO m, HasThrow "left" Context.PathError m) =>
  (Contextify.ContextSexp, [ResolveOpen.PreQualified]) ->
  (Context.NameSymbol, [Sexp.T]) ->
  m (Contextify.ContextSexp, [ResolveOpen.PreQualified])
resolveOpensS (ctx', openList) (sym, xs) = do
  ctx <- ContextSexp.run ctx' (sym, xs)
  case ctx of
    Right Contextify.PS {ctxS, opensS, modsDefinedS} ->
      pure
        ( ctxS,
          ResolveOpen.Pre
            { opens = opensS,
              explicitModule = sym,
              implicitInner = modsDefinedS
            }
            : openList
        )
    Left err -> throw @"left" err

------------------------------------------------------------
-- Misc Helpers
------------------------------------------------------------

addTop :: Bifunctor p => p NameSymbol.T c -> p NameSymbol.T c
addTop = first (NameSymbol.cons Context.topLevelName)

runM :: M a -> IO (Either Context.PathError a)
runM (M a) = runExceptT a
