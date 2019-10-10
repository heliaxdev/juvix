--Tests for the type checker and evaluator in Core/MainLang.hs.
module MainLangTests where

import           Juvix.Core.MainLang
import           Juvix.Core.Parser

import           Control.Monad.Except
import           Numeric.Natural
import           Prelude

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Test.QuickCheck
import           Test.QuickCheck.Gen

--property tests of type checker:
--the term's inferred type equals to the input type
--property test of evaluator:
-- \x.x (any term) evaluates to (any term evaluated)
{-lamProp :: CTerm -> Env -> Property
lamProp cterm env = App (cEval (Lam Bound 0) env) cterm == cterm-}
-- any constant term evaluates to itself
constProp ∷ CTerm → Env → Bool
constProp (Star i) env = cEval (Star i) env == VStar i
constProp Nats env     = cEval Nats env == VNats
  --constProp _ _          = True --Not testing non-const terms

{-TODO need to combine generators to generate CTerms http://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Gen.html
instance Arbitrary CTerm where
  arbitrary = CTerm -}
natsTypeStar0 ∷ Assertion
natsTypeStar0 = cType 0 [] Nats (0, VStar 0) @?= Right ()

nat1Inferred ∷ Assertion
nat1Inferred = iType 0 [] (Nat 1) @?= Right (Omega, VNats)

--function has to be named test_ to be picked up by tasty.
test_core ∷ Test.Tasty.TestTree
test_core =
  testGroup
    "Core tests"
    [ testGroup
        "Type-checker, Units"
        [ testCase "Nats is of type * 0" natsTypeStar0
        , testCase "Inferred type of Nat 1 is (w, VNats)" nat1Inferred
        ]
    , testGroup
        "CTerms and ITerms Parser, Units"
        [ testCase
            "* n"
            (parseString (parseWhole cterm) "* 0" @?= Just (Star 0))
        , testCase
            "primitive type Nats"
            (parseString (parseWhole cterm) "Nat" @?= Just Nats)
        , testCase
            "dependent function"
            (parseString (parseWhole cterm) "[Π] 1 * 0 * 0" @?=
             Just (Pi 1 (Star 0) (Star 0)))
        , testCase
            "abstraction, the identity function"
            (parseString (parseWhole cterm) "\\x. Bound 0" @?=
             Just (Lam (Conv (Bound 0))))
        , testCase
            "conversion"
            (parseString (parseWhole cterm) "Conv 0" @?= Just (Conv (Nat 0)))
        , testCase
            "silent convert: nat"
            (parseString (parseWhole cterm) "0" @?= Just (Conv (Nat 0)))
        , testCase
            "silent convert: Bound var"
            (parseString (parseWhole cterm) "Bound 0" @?= Just (Conv (Bound 0)))
        , testCase
            "silent convert: Free var"
            (parseString (parseWhole cterm) "Free (Global aStringName)" @?=
             Just (Conv (Free (Global "aStringName"))))
        , testCase
            "silent convert: App" --doesn't make sense now because there is no ITerm that is a function atm.
            (parseString (parseWhole cterm) "App Bound 0 Nat" @?=
             Just (Conv (App (Bound 0) Nats)))
        , testCase
            "silent convert: Ann"
            (parseString (parseWhole cterm) "w Nat : * 0" @?=
             Just (Conv (Ann Omega Nats (Star 0))))
        ]
    --, testGroup "Evaluator Properties"
    --   [testProperty "Constant terms should evaluate to themselves" constProp]
    --, testGroup "Type-checker Properties" [testProperty "Quickcheck test" arith]
    ]
