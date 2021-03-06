module Juvix.FrontendContextualise.Environment where

import Control.Lens hiding ((|>))
import qualified Data.HashSet as Set
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.NameSpace as NameSpace
import qualified Juvix.Core.Common.Open as Open
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import Prelude (error)

type HasNew t ty s m = HasState "new" (Context.T t ty s) m

type HasOld t ty s m = HasState "old" (Context.T t ty s) m

-- to avoid casting all the time

class SymbLookup a where
  look ::
    a ->
    Context.T term ty sumRep ->
    Maybe (Context.From (Context.Definition term ty sumRep))

  lookCurr ::
    a ->
    Context.T term ty sumRep ->
    Maybe (NameSpace.From (Context.Definition term ty sumRep))

instance SymbLookup Symbol where
  look sym cont = Context.lookup (NameSymbol.fromSymbol sym) cont

  --
  lookCurr sym cont = Context.lookupCurrent (NameSymbol.fromSymbol sym) cont

instance SymbLookup NameSymbol.T where
  look sym cont = Context.lookup sym cont

  lookCurr sym cont = Context.lookupCurrent sym cont

lookup ::
  (HasNew term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (Context.From (Context.Definition term ty sumRep)))
lookup sy = do
  get @"new" >>| look sy

lookupCurrent ::
  (HasNew term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (NameSpace.From (Context.Definition term ty sumRep)))
lookupCurrent sy = do
  get @"new" >>| lookCurr sy

ask ::
  (HasOld term ty sumRep m, SymbLookup sym) =>
  sym ->
  m (Maybe (Context.From (Context.Definition term ty sumRep)))
ask sy = do
  get @"old" >>| look sy

add ::
  HasNew term ty sumRep m =>
  NameSpace.From Symbol ->
  Context.Definition term ty sumRep ->
  m ()
add sy def = Juvix.Library.modify @"new" (Context.add sy def)

addGlobal ::
  HasNew term ty sumRep m =>
  NameSymbol.T ->
  Context.Definition term ty sumRep ->
  m ()
addGlobal sy def = Juvix.Library.modify @"new" (Context.addGlobal sy def)

remove ::
  HasNew term ty sumRep m => NameSpace.From Symbol -> m ()
remove sy = Juvix.Library.modify @"new" (Context.remove sy)

removeGlobal ::
  HasNew term ty sumRep m => NameSymbol.T -> m ()
removeGlobal sy = Juvix.Library.modify @"new" (Context.removeNameSpace sy)

removeOld ::
  HasOld term ty sumRep m => NameSpace.From Symbol -> m ()
removeOld sy = Juvix.Library.modify @"old" (Context.remove sy)

addUnknown ::
  HasNew term ty sumRep m => NameSpace.From Symbol -> m ()
addUnknown sym =
  Juvix.Library.modify @"new"
    (Context.add sym (Context.Unknown Nothing))

addUnknownGlobal ::
  HasNew term ty sumRep m => Context.From Symbol -> m ()
addUnknownGlobal (Context.Current sym) = addUnknown sym
addUnknownGlobal (Context.Outside sym) =
  Juvix.Library.modify @"new"
    (Context.addGlobal (pure sym) (Context.Unknown Nothing))

------------------------------------------------------------
-- double module setup and dealings
------------------------------------------------------------

setupNewModule ::
  Context.T term1 ty1 sumRep1 -> IO (Context.T term2 ty2 sumRep2)
setupNewModule t = do
  empt <- Context.empty (Context.currentName t)
  empt
    |> set
      (Context._currentNameSpace . Context.qualifiedMap)
      (t ^. Context._currentNameSpace . Context.qualifiedMap)
    |> set Context._reverseLookup (t ^. Context._reverseLookup)
    |> pure

-- | @switchContext@ takes two modules representing the same context
-- and attempts to switch the namespace while keeping the pointers
-- consistent
switchContext ::
  NameSymbol.T ->
  Context.T term ty sumRep ->
  Context.T term2 ty2 sumRep2 ->
  IO
    ( Either
        Context.PathError
        (Context.T term ty sumRep, Context.T term2 ty2 sumRep2)
    )
switchContext sym ctx1 ctx2 = do
  let switched1 = Context.inNameSpace sym ctx1
      switched2 = Context.inNameSpace sym ctx2
  case (switched1, switched2) of
    (Just c1, Just c2) -> pure $ Right (c1, c2)
    (Nothing, Nothing) -> pure $ Left (Context.VariableShared sym)
    (Just c1, Nothing) -> setupFill sym c1 ctx2
    (Nothing, Just c2) -> setupFill sym c2 ctx1 >>| fmap swap

-- | @oneFilled@ takes a namesymbol and filled out parts of a currently
-- added module and creates a new context with that module filled out
oneFilled ::
  NameSymbol.T ->
  ([Open.TName NameSymbol.T], Context.SymbolMap) ->
  Context.T term ty sumRep ->
  IO (Either Context.PathError (Context.T term ty sumRep))
oneFilled sym (openList, qualifiedMap) ctx =
  Context.addPathWithValue
    sym
    (Context.Record (Context.Rec NameSpace.empty Nothing openList qualifiedMap))
    ctx

-- | @setupFilled@ takes two contexts, one that successfully switched
-- modules and another that hasn't and inserts the needed information
-- to keep the pointers consistent
setupFill ::
  NameSymbol.T ->
  Context.T term1 ty1 sumRep1 ->
  Context.T term2 ty2 sumRep2 ->
  IO
    ( Either
        Context.PathError
        (Context.T term1 ty1 sumRep1, Context.T term2 ty2 sumRep2)
    )
setupFill sym cWorks cDoesntWork = do
  let args =
        ( cWorks ^. Context._currentNameSpace . Context.openList,
          cWorks ^. Context._currentNameSpace . Context.qualifiedMap
        )
  newInserted <- oneFilled sym args cDoesntWork
  case newInserted of
    Right cInserted ->
      -- This will always be a just!
      let Just cNowWorks = Context.inNameSpace sym cInserted
       in pure $ Right (cWorks, cNowWorks)
    Left err -> pure $ Left err

----------------------------------------------------------------------
-- Sexp Helpers Code Above this Will likely be deleted
----------------------------------------------------------------------

-- Will make things more export friendly when the above code is gone

-- TODO ∷ make this a standard data structure

-- Currently we don't really use the signature however in the future
-- the mSig will be used to detect the types of modules we will have
-- open and any other information we wish to track here!?
data Information
  = Info
      { -- | @mSig@ represents the type of the term in the closure
        mSig :: Maybe Sexp.T,
        -- | @info@ represents all the information we have on the term
        info :: [Context.Information],
        -- | @mOpen@ represents a place where the term may have come
        -- from
        mOpen :: Maybe NameSymbol.T
      }
  deriving (Show, Eq)

newtype Closure'
  = Closure (Map.T Symbol Information)
  deriving (Show, Eq)

newtype ErrorS = CantResolve [Sexp.T] deriving (Show, Eq)

type SexpContext = Context.T Sexp.T Sexp.T Sexp.T

type HasClosure m = HasReader "closure" Closure' m

type ContextS m = HasState "context" SexpContext m

type ErrS m = HasThrow "error" ErrorS m

addToClosure :: Symbol -> Information -> Closure' -> Closure'
addToClosure k info (Closure m) =
  Closure $ Map.insert k info m

genericBind :: Symbol -> Closure' -> Closure'
genericBind name (Closure m) =
  Closure $ Map.insert name (Info Nothing [] Nothing) m

keys :: Closure' -> Set.HashSet Symbol
keys (Closure m) = Map.keysSet m

data Pass m
  = Pass
      { sumF :: Sexp.Atom -> Sexp.T -> m Sexp.T,
        termF :: Sexp.Atom -> Sexp.T -> m Sexp.T,
        tyF :: Sexp.Atom -> Sexp.T -> m Sexp.T
      }

-- | @passContextSingle@ Traverses the context firing off sexp
-- traversals based on the given trigger.
passContextSingle ::
  (HasClosure m, ErrS m) =>
  -- | the context in which our code resides in
  SexpContext ->
  -- | the trigger function that states what forms to fire off
  -- on. :atom for firing on every atom
  (NameSymbol.T -> Bool) ->
  -- | Our transformation function that is responsible for handling the triggers
  (Sexp.Atom -> Sexp.T -> m Sexp.T) ->
  m SexpContext
passContextSingle ctx trigger f =
  passContext ctx trigger (Pass f f f)

-- | @passContext@ like @passContextSingle@ but we supply a different
-- function for each type term and sum representation form.
passContext ::
  (HasClosure m, ErrS m) => SexpContext -> (NameSymbol.T -> Bool) -> Pass m -> m SexpContext
passContext ctx trigger Pass {sumF, termF, tyF} =
  Context.mapWithContext
    ctx
    Context.CtxForm
      { -- Need to do this consing of type to figure out we are in a type
        -- we then need to remove it, as it shouldn't be there
        sumF = \form ->
          fmap Sexp.cdr . pass sumF (Sexp.Cons (Sexp.atom "type") form),
        termF = pass termF,
        tyF = pass tyF
      }
  where
    pass func form ctx =
      Sexp.foldSearchPred
        form
        (trigger, func)
        (bindingForms, searchAndClosure ctx)

-- | @bindingForms@ is a predicate that answers true for every form
-- that instantiates a new variable
bindingForms :: (Eq a, IsString a) => a -> Bool
bindingForms x =
  x
    `elem` [ "type",
             ":open-in",
             ":let-type",
             ":let-match",
             "case",
             ":lambda-case",
             ":declaim",
             ":lambda"
           ]

-- | @searchAndClosure@ is responsible for properly updating the
-- closure based on any binders we may encounter. The signature is made
-- to fit into the @Sexp.foldSearchPred@'s binder dispatch clause, the
-- only difference is that we take an extra context before that
-- signature.
searchAndClosure ::
  (HasClosure f, ErrS f) =>
  -- | The Context, an extra function that is required the by the
  -- :open-in case.
  SexpContext ->
  -- | the atom to dispatch on
  Sexp.Atom ->
  -- | The sexp form in which the atom is called on
  Sexp.T ->
  -- | the continuation of continuing the changes
  (Sexp.T -> f Sexp.T) ->
  f Sexp.T
searchAndClosure ctx a as cont
  | named "case" = case' as cont
  -- this case happens at the start of every defun
  | named ":lambda-case" = lambdaCase as cont
  -- This case is a bit special, as we must check the context for
  -- various names this may introduce to the
  | named ":open-in" = openIn ctx as cont
  | named ":declaim" = declaim as cont
  | named ":let-match" = letMatch as cont
  | named ":let-type" = letType as cont
  | named "type" = type' as cont
  | named ":lambda" = lambda as cont
  where
    named = Sexp.isAtomNamed (Sexp.Atom a)
searchAndClosure _ _ _ _ = error "imporper closure call"

------------------------------------------------------------
-- searchAndClosure function dispatch table
------------------------------------------------------------

lambda :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
lambda (Sexp.List [arguments, body]) cont =
  local @"closure" (\cnt -> foldr genericBind cnt (nameStar arguments)) $ do
    args <- cont arguments
    bod <- cont body
    pure $ Sexp.list [args, bod]
lambda _ _ = error "malformed lambda"

letType :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
letType (Sexp.List [assocName, args, dat, body]) cont = do
  local @"closure" (\cnt -> foldr genericBind cnt (bindings <> consturctors)) $ do
    d <- cont dat
    assoc <- cont assocName
    bod <- cont body
    pure $ Sexp.list [assoc, args, d, bod]
  where
    bindings = nameStar args
    consturctors = nameGather dat
letType _ _ = error "malformed let-type"

type' :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
type' (assocName Sexp.:> args Sexp.:> dat) cont =
  local @"closure" (\cnt -> foldr genericBind cnt grabBindings) $ do
    d <- cont dat
    assoc <- cont assocName
    pure $ assoc Sexp.:> args Sexp.:> d
  where
    grabBindings = nameStar args
type' _ _ = error "malformed type"

-- | @openIn@ opens @mod@, adding the contents to the closure of
-- @body@. Note that we first =resolve= what mod is by calling the
-- continuation, @cont@, in case any transformations want to change
-- what the @mod@ is.
openIn ::
  (ErrS f, HasClosure f) => SexpContext -> Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
openIn ctx (Sexp.List [mod, body]) cont = do
  -- Fully run what we need to on mod
  newMod <- cont mod
  -- Now let us open up the box
  case Sexp.atomFromT newMod of
    Just Sexp.A {atomName} ->
      case ctx Context.!? atomName >>| Context.extractValue of
        Just (Context.Record record) ->
          let NameSpace.List {publicL} = NameSpace.toList (record ^. Context.contents)
              --
              newSymbs = fst <$> publicL
              --
              addSymbolInfo symbol =
                addToClosure symbol (Info Nothing [] (Just atomName))
           in --
              local @"closure" (\cnt -> foldr addSymbolInfo cnt newSymbs) do
                newBody <- cont body
                pure $ Sexp.list [newMod, newBody]
        _ ->
          throw @"error" (CantResolve [newMod])
    _ ->
      throw @"error" (CantResolve [newMod])
openIn _ _ _ = error "malformed open-in"

-- | @lambdaCase@ we encounter a @:lambda-case@ at the start of every
-- Definition in the context. This ensures the arguments are properly
-- bound for the inner computation.
lambdaCase :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
lambdaCase binds cont =
  mapF (`matchMany` cont) binds

letMatch :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
letMatch (Sexp.List [name, bindings, body]) cont
  | Just nameSymb <- eleToSymbol name =
    local @"closure" (genericBind nameSymb) $ do
      -- this just makes it consistent with the lambdaCase case
      let grouped = Sexp.groupBy2 bindings
      form <- mapF (`matchMany` cont) grouped
      bod <- cont body
      pure $ Sexp.list [name, Sexp.unGroupBy2 form, bod]
letMatch _ _ = error "malformed let-match"

-- | @case'@ is similar to @lambdaCase@ except that it has a term it's
-- matching on that it must first change without having an extra
-- binders around it
case' :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
case' (t Sexp.:> binds) cont = do
  op <- cont t
  binding <- mapF (`match` cont) binds
  pure (Sexp.Cons op binding)
case' _ _ = error "malformed case"

-- This works as we should only do a declaration after the function
-- locally, so if it gets overwritten its' not a big deal

-- | @declaim@ takes a declaration and adds the declaration information
-- to the context
declaim :: HasClosure f => Sexp.T -> (Sexp.T -> f Sexp.T) -> f Sexp.T
declaim (Sexp.List [d, e]) cont
  | Just (name, information) <- declaration d =
    local @"closure" (addToClosure name information) $ do
      -- safe to do dec here, as if we modify the declaration it
      -- would be fine to do it after, as all a pass would do is to
      -- make it a namesymbol, meaning it wouldn't work as is ☹
      dec <- cont d
      exp <- cont e
      pure $ Sexp.list [dec, exp]
declaim _ _ = error "malformed declaim"

------------------------------------------------------------
-- Helpers for the various Search and Closure dispatch
------------------------------------------------------------

-- | @matchMany@ deals with a @((binding-1 … binding-n) body) term, and
-- proper continues the transformation on the body, and the bindings
-- after making sure to register that they are indeed bound terms
matchMany :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
matchMany = matchGen nameStar

-- | @match@ deals with a @(bindings body)@ term coming down, see
-- @matchMany@ for more details
match :: HasClosure m => Sexp.T -> (Sexp.T -> m Sexp.T) -> m Sexp.T
match = matchGen nameStarSingle

-- | @matchGen@ is a generic/general version of match and matchMany as
-- the form that comes in may be a list of binders or a single term
-- being bound.
matchGen ::
  (HasClosure m, Foldable t) =>
  (Sexp.T -> t Symbol) ->
  Sexp.T ->
  (Sexp.T -> m Sexp.T) ->
  m Sexp.T
matchGen nameStarFunc (Sexp.List [path, body]) cont =
  -- Important we must do this first!
  local @"closure" (\cnt -> foldr genericBind cnt grabBindings) $ do
    -- THIS MUST happen in the local, as we don't want to have a pass
    -- confuse the variables here as something else... imagine if we
    -- are doing a pass which resolves symbols, then we'd try to
    -- resolve the variables we bind. However for constructors and what
    -- not they need to be ran through this pass
    newPath <- cont path
    newB <- cont body
    pure (Sexp.list [newPath, newB])
  where
    grabBindings = nameStarFunc path
matchGen _ _ _ = error "malformed match"

-- | @nameStarSingle@ like @nameStar@ but we are matching on a single
-- element
nameStarSingle :: Sexp.T -> [Symbol]
nameStarSingle = nameStar . (\x -> Sexp.list [x])

-- | @nameStar@ grabs names recursively
nameStar :: Sexp.T -> [Symbol]
nameStar ((_caar Sexp.:> cadr) Sexp.:> cdr) =
  -- we ignore the _caar as it's a cosntructor!
  nameStar cadr <> nameStar cdr
nameStar (x Sexp.:> xs)
  | Just symb <- eleToSymbol x =
    symb : nameStar xs
  | otherwise =
    -- the car is not a cons or an atom, thus a number, we should
    -- ignore it
    nameStar xs
nameStar Sexp.Atom {} = []
nameStar Sexp.Nil = []

-- Sexp.parse "((Cons (:arrow (:infix -> Int Int))) (Nil))" >>| nameGather

-- | @nameGather@ takes an adt sexp and extracts the constructors from it
nameGather :: Sexp.T -> [Symbol]
nameGather ((caar Sexp.:> _cdar) Sexp.:> cdr)
  | Just symb <- eleToSymbol caar,
    symb /= ":" || symb /= ":record-d" =
    symb : nameGather cdr
nameGather (_ Sexp.:> cdr) = nameGather cdr
nameGather _ = []

------------------------------------------------------------
-- Helpers for declaim
------------------------------------------------------------

-- | @declaration@ takes a declaration and tries to get the information
-- along with the name from it.
-- - Note :: we can only get symbol declarations to update, as we rely
--   on closure semantics whicich only work on symbols unfortunately.
declaration :: Sexp.T -> Maybe (Symbol, Information)
declaration (Sexp.List [inf, n, i])
  | Just Sexp.N {atomNum} <- Sexp.atomFromT i,
    Just atomName <- eleToSymbol n =
    let func =
          if  | Sexp.isAtomNamed inf "infix" ->
                Context.NonAssoc
              | Sexp.isAtomNamed inf "infixl" ->
                Context.Left
              | Sexp.isAtomNamed inf "infixr" ->
                Context.Right
              | otherwise -> error "malformed declaration"
     in Just
          ( atomName,
            Info
              Nothing
              [Context.Prec $ Context.Pred func (fromInteger atomNum)]
              Nothing
          )
declaration _ = Nothing

------------------------------------------------------------
-- Move to Sexp library
------------------------------------------------------------

mapF :: Applicative f => (Sexp.T -> f Sexp.T) -> Sexp.T -> f Sexp.T
mapF f (x Sexp.:> xs) =
  Sexp.Cons <$> f x <*> mapF f xs
mapF _ Sexp.Nil = pure Sexp.Nil
mapF _ a = pure a

eleToSymbol :: Sexp.T -> Maybe Symbol
eleToSymbol x
  | Just Sexp.A {atomName} <- Sexp.atomFromT x =
    Just (NameSymbol.toSymbol atomName)
  | otherwise = Nothing
