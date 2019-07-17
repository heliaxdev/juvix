module Juvix.Eal.Eal where

import           Juvix.Library hiding (link, reduce)
import           Data.Map.Strict as Map

data Eal = Term SomeSymbol
         | Lambda SomeSymbol Types Term
         | App Term Term
         deriving Show


data Term = Bang Integer Eal
          deriving Show

data Types = Lolly Types Types
           | BangT Integer Types
           | Forall
           | Specific SomeSymbol
           | UBang Integer Types
           deriving (Show, Eq)

data TypeErrors = MisMatchArguments
                | MissingOverUse
                | ExpectedFunction
                deriving Show

data BracketErrors = TooManyOpen
                   | TooManyClosing
                   deriving Show

type Info = Map SomeSymbol Types

newtype EnvError a = EnvError (ExceptT TypeErrors (State Info) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "ctxt" (Map SomeSymbol Types)) via
       MonadState (ExceptT TypeErrors (State Info))
  deriving (HasThrow "typ" TypeErrors) via
    MonadError (ExceptT TypeErrors (State Info))

runEnvError :: EnvError a -> (Either TypeErrors a, Info)
runEnvError (EnvError a) = runState (runExceptT a) Map.empty

newtype EitherBracket a =
  EitherBracket { runEither :: (Either BracketErrors a) }
  deriving (Functor, Applicative, Monad) via
    Except BracketErrors
  deriving (HasThrow "typ" BracketErrors) via
    MonadError (Except BracketErrors)

runBracketChecker :: Eal → Either BracketErrors ()
runBracketChecker t = runEither (bracketChecker t 0)

runTypeOf :: Eal → (Either TypeErrors Types, Info)
runTypeOf e = (reverseBangs <$> t, state)
  where (t, state) = runEnvError (typeOf e)

typeOf :: (HasState "ctxt" (Map SomeSymbol Types) m, HasThrow "typ" TypeErrors m)
       ⇒ Eal → m Types
typeOf (Term s) = do
  ctxt ← get @"ctxt"
  case ctxt Map.!? s of
    Nothing           → throw @"typ" MissingOverUse
    Just x@(BangT {}) → pure x
    Just x            → put @"ctxt" (Map.delete s ctxt) >> pure x

typeOf (Lambda sym symType term) = do
  modify @"ctxt" (Map.insert sym symType)
  typeTerm ← typeOfTerm term
  pure (Lolly symType typeTerm)

typeOf (App t1 t2) = do
  typeT2 ← typeOfTerm t2
  typeT1 ← typeOfTerm t1
  case getTypeInsideBang typeT1 of
    Lolly Forall target                 → pure target
    Lolly arg target
      | arg == typeT2
      ∨ Forall == getTypeInsideBang arg → pure target
      | otherwise                       → throw @"typ" MisMatchArguments
    _                                   → throw @"typ" ExpectedFunction

typeOfTerm :: (HasState "ctxt" (Map SomeSymbol Types) f, HasThrow "typ" TypeErrors f)
           ⇒ Term → f Types
typeOfTerm (Bang n e)
  | n > 0 = BangT n <$> typeOf e
  | n < 0 = UBang n <$> typeOf e
  | otherwise = typeOf e

getTypeInsideBang :: Types → Types
getTypeInsideBang (BangT _ t) = getTypeInsideBang t
getTypeInsideBang (UBang _ t) = getTypeInsideBang t
getTypeInsideBang t           = t

reverseBangs :: Types → Types
reverseBangs t = recursive t identity
  where
    -- Note this doesn't flatten all consecutive bangs and ubangs
    recursive (BangT n (BangT m t)) cps = recursive (BangT (n + m) t) cps
    recursive (UBang n (UBang m t)) cps = recursive (UBang (n + m) t) cps
    recursive (BangT n (UBang m t)) cps = recursive t (UBang m . BangT n . cps)
    recursive (UBang n (BangT m t)) cps = recursive t (BangT m . UBang n . cps)
    recursive (BangT m t)           cps = recursive t (BangT m . cps)
    recursive (UBang m t)           cps = recursive t (UBang m . cps)
    recursive t                     cps = cps t
-- Start of correct bracketing formula -----------------------------------------

bracketChecker :: HasThrow "typ" BracketErrors f ⇒ Eal → Integer → f ()
bracketChecker (Term _)       0 = pure ()
bracketChecker (Term _)       _ = throw @"typ" TooManyOpen
bracketChecker (Lambda _ _ t) n = bracketCheckerTerm t n
bracketChecker (App t1 t2)    n = bracketCheckerTerm t2 n >> bracketCheckerTerm t1 n

bracketCheckerTerm :: HasThrow "typ" BracketErrors f ⇒ Term → Integer → f ()
bracketCheckerTerm (Bang changeBy eal) n
  | changeBy + n < 0 = throw @"typ" TooManyClosing
  | otherwise        = bracketChecker eal (n + changeBy)
