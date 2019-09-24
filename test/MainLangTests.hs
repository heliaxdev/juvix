--Tests for the type checker and evaluator in Core/MainLang.hs.
module MainLangTests where

import           Juvix.Core.MainLang

import           Prelude
import           Test.Tasty
import           Test.Tasty.HUnit

--import           Test.Tasty.QuickCheck
--import qualified Test.Tasty.SmallCheck as SC
--import           Test.HUnit          (Assertion, (@?=))
--import qualified Test.QuickCheck     as QC
--property tests of type checker:
--the term's inferred type equals to the input type
--property test of evaluator:
-- \x.x (any term) evaluates to (any term evaluated)
{-lamProp :: CTerm -> Env -> Property
lamProp cterm env = App (cEval (Lam Bound 0) env) cterm == cterm-}
-- any constant term evaluates to itself
--constProp :: CTerm -> Env -> Property
--constProp
natsTypeStar0 ∷ Assertion
natsTypeStar0 = cType 0 [] Nats (0, VStar 0) @?= Right ()

nat1Inferred ∷ Assertion
nat1Inferred = iType 0 [] (Nat 1) @?= Right (Omega, VNats)

tests ∷ Test.Tasty.TestTree
tests =
  testGroup
    "Core tests"
    [ testGroup
        "Type-checker Units"
        [ testCase "Nats is of type * 0" natsTypeStar0
        , testCase "Inferred type of Nat 1 is (w, VNats)" nat1Inferred
        ]
   -- , testGroup "Type-checker Properties" [testProperty "Quickcheck test" arith]
   -- , testGroup "SmallCheck tests" [SC.testProperty "Negation" negation]
    ]
