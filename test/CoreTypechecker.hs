-- | Tests for the type checker and evaluator in Core/IR/Typechecker.hs
module CoreTypechecker where

import Juvix.Core.IR
import Juvix.Core.Usage
import Juvix.Library hiding (identity)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

identity ∷ CTerm
identity = Lam (Conv (Bound 0))

identityCompTy ∷ Annotation
identityCompTy =
  ( SNat 1,
    VPi (SNat 1) VNats (const (VNats))
  )

identityContTy ∷ Annotation
identityContTy =
  ( SNat 0,
    VPi (SNat 0) VNats (const (VNats))
  )

identityApplication ∷ CTerm
identityApplication = Conv (App (Ann (SNat 1) identity (Pi (SNat 1) Nats Nats)) (Conv (Nat 1)))

natTy ∷ Annotation
natTy = (SNat 1, VNats)

test_identity_computational ∷ T.TestTree
test_identity_computational = shouldCheck identity identityCompTy

test_identity_contemplation ∷ T.TestTree
test_identity_contemplation = shouldCheck identity identityContTy

test_identity_application ∷ T.TestTree
test_identity_application = shouldCheck identityApplication natTy

test_nats_type_star0 ∷ T.TestTree
test_nats_type_star0 = shouldCheck Nats (SNat 0, VStar 0)

test_nat1 ∷ T.TestTree
test_nat1 = shouldInfer (Nat 1) (Omega, VNats)

--unit tests for cType
shouldCheck ∷ CTerm → Annotation → T.TestTree
shouldCheck term ann =
  T.testCase (show term <> " should check as type " <> show ann) $
    cType 0 [] term ann T.@=? Right ()

--unit tests for iType
shouldInfer ∷ ITerm → Annotation → T.TestTree
shouldInfer term ann =
  T.testCase (show term <> " should infer to type " <> show ann) $
    iType0 [] term T.@=? Right ann

one ∷ CTerm
one =
  Lam
    $ Lam
    $ Conv
    $ App
      (Bound 1)
      (Conv (Bound 0))

oneCompTy ∷ Annotation
oneCompTy =
  ( SNat 1,
    VPi
      (SNat 1)
      ( VPi
          (SNat 1)
          VNats
          (const VNats)
      )
      ( const
          ( VPi
              (SNat 1)
              VNats
              (const VNats)
          )
      )
  )

two ∷ CTerm
two =
  Lam
    $ Lam
    $ Conv
    $ App
      (Bound 1)
      ( Conv
          ( App
              (Bound 1)
              (Conv (Bound 0))
          )
      )

twoCompTy ∷ Annotation
twoCompTy =
  ( SNat 1,
    VPi
      (SNat 2)
      ( VPi
          (SNat 1)
          VNats
          (const VNats)
      )
      ( const
          ( VPi
              (SNat 1)
              VNats
              (const VNats)
          )
      )
  )
-- property tests of type checker:
-- the term's inferred type equals to the input type
-- property test of evaluator:
-- \x.x (any term) evaluates to (any term evaluated)
{- lamProp ∷ CTerm → Env → Property
lamProp cterm env = App (cEval (Lam Bound 0) env) cterm == cterm-}
-- any constant term evaluates to itself
-- constProp ∷ CTerm → Env → Bool
-- constProp (Star i) env = cEval (Star i) env == VStar i
-- constProp Nats env     = cEval Nats env == VNats
-- constProp _ _          = True --Not testing non-const terms

{- TODO need to combine generators to generate
   CTerms http://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Gen.html
instance Arbitrary CTerm where
  arbitrary = CTerm -}
