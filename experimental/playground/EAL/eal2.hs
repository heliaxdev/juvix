{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  typeT1 <- typeOfTerm t1
  typeT2 <- typeOfTerm t2
  case typeT1 of
    Lolly arg target
      | arg == typeT2 → pure target
      | otherwise     → throw @"typ" MisMatchArguments
    _ → throw @"typ" ExpectedFunction

typeOfTerm :: (HasState "ctxt" (Map SomeSymbol Types) f, HasThrow "typ" TypeErrors f)
           ⇒ Term → f Types
typeOfTerm (Bang n e)
  | n > 0 = BangT n <$> typeOf e
  | n < 0 = UBang n <$> typeOf e
  | otherwise = typeOf e
