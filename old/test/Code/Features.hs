module Code.Features (
  featuresTranspilationTestCases
) where

import qualified Data.Text                as T
import           Text.RawString.QQ

import qualified Juvix.Backends.Michelson as M
import           Types

featuresTranspilationTestCases ∷ [TranspilationTestCase]
featuresTranspilationTestCases = [case', monads, datatypes, polymorphism]

case' ∷ TranspilationTestCase
case' = TranspilationTestCase {
  testName    = "case",
  testHaskell = [r|
module Case where

import           Juvix.Lib

data Action
  = One Int
  | Two Tez

type Result = Int

main ∷ (Action, ()) → (Result, ())
main (action, ()) =
  case action of
    Two _ → (0, ())
    One i → (i, ())
|],
  testInputs  = []
  }

monads ∷ TranspilationTestCase
monads = TranspilationTestCase {
  testName    = "monads",
  testHaskell = [r|
module Monads where

import           Juvix.Lib

data MyMaybe a
  = MySome a
  | MyNone

instance Functor MyMaybe where

  fmap f (MySome x) = MySome (f x)
  fmap _ MyNone     = MyNone

instance Applicative MyMaybe where

  pure = MySome

  (<*>) (MySome f) (MySome x) = MySome (f x)
  (<*>) _          _          = MyNone

instance Monad MyMaybe where

  return = MySome
  {-# INLINE return #-}

  (>>=) (MySome x) f = f x
  (>>=) MyNone     _ = MyNone
  {-# INLINE (>>=) #-}

{-# SPECIALIZE (>>=) ∷ MyMaybe a → (a → MyMaybe b) → MyMaybe b #-}
{-# SPECIALIZE return ∷ a → MyMaybe a #-}

bindInt :: MyMaybe Int -> (Int -> MyMaybe Int) -> MyMaybe Int
bindInt (MySome x) f = f x
bindInt MyNone     _ = MyNone

returnInt :: Int -> MyMaybe Int
returnInt x = MySome x

{-# RULES "bind/Int" (>>=) = bindInt #-}
{-# RULES "return/Int" return = returnInt #-}

addOneM ∷ MyMaybe Int → MyMaybe Int
addOneM m = do
  val ← m
  return (val + (1 ∷ Int))

main ∷ (MyMaybe Int, ()) → (MyMaybe Int, ())
main (param, ()) = (addOneM param, ())
|],
  testInputs  = []
  }

datatypes ∷ TranspilationTestCase
datatypes = TranspilationTestCase {
  testName    = "datatypes",
  testHaskell = [r|
module Datatypes where

import           Juvix.Lib

data Action
  = Pass
  | Send Hash Tez

data Result
  = Passed
  | Sent

run ∷ Action → IO Result
run Pass            = return Passed
run (Send keyHash tez)  = do
  transferTokens () tez (defaultAccount keyHash) ()
  return Sent

main ∷ (Action, ()) → IO (Result, ())
main (action, ()) = do
  result ← run action
  return (result, ())
|],
  testInputs  = []
  }

polymorphism ∷ TranspilationTestCase
polymorphism = TranspilationTestCase {
  testName    = "polymorphism",
  testHaskell = [r|
module Polymorphism where

import           Juvix.Lib

fst ∷ (a, b) → a
fst (x, _) = x

snd ∷ (a, b) → b
snd (_, y) = y

main ∷ ((String, String), (Int, String)) → (String, (Int, String))
main (param, storage) = (fst param, (fst storage, snd storage))
|],
  testInputs  = []
  }
