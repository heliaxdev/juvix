-- | Tests for the type checker and evaluator in Core/IR/Typechecker.hs
module CoreTypechecker where

import qualified Juvix.Core.IR as IR
import Juvix.Core.Parameterisations.Naturals
import Juvix.Core.Usage
import Juvix.Library hiding (identity)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

type Term = IR.Term NatTy NatVal

type Elim = IR.Elim NatTy NatVal

type Value = IR.Value NatTy NatVal

type Annotation = IR.Annotation NatTy NatVal

identity ∷ Term
identity = IR.Lam (IR.Elim (IR.Bound 0))

identityCompTy ∷ Annotation
identityCompTy =
  ( SNat 1,
    IR.VPi (SNat 1) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat))
  )

identityContTy ∷ Annotation
identityContTy =
  ( SNat 0,
    IR.VPi (SNat 0) (IR.VPrimTy Nat) (const (IR.VPrimTy Nat))
  )

identityApplication ∷ Term
identityApplication = IR.Elim (IR.App (IR.Ann (SNat 1) identity (IR.Pi (SNat 1) (IR.PrimTy Nat) (IR.PrimTy Nat))) (IR.Elim (IR.Prim (Natural 1))))

natTy ∷ Annotation
natTy = (SNat 1, IR.VPrimTy Nat)

test_identity_computational ∷ T.TestTree
test_identity_computational = shouldCheck identity identityCompTy

test_identity_contemplation ∷ T.TestTree
test_identity_contemplation = shouldCheck identity identityContTy

test_identity_application ∷ T.TestTree
test_identity_application = shouldCheck identityApplication natTy

test_nats_type_star0 ∷ T.TestTree
test_nats_type_star0 = shouldCheck (IR.PrimTy Nat) (SNat 0, IR.VStar 0)

test_nat1 ∷ T.TestTree
test_nat1 = shouldInfer (IR.Prim (Natural 1)) (Omega, IR.VPrimTy Nat)

test_add_nat ∷ T.TestTree
test_add_nat = shouldInfer (IR.App (IR.App (IR.Prim Add) (IR.Elim (IR.Prim (Natural 1)))) (IR.Elim (IR.Prim (Natural 2)))) (Omega, IR.VPrimTy Nat)

test_eval_add ∷ T.TestTree
test_eval_add = shouldEval (IR.Elim (IR.App (IR.App (IR.Prim Add) (IR.Elim (IR.Prim (Natural 1)))) (IR.Elim (IR.Prim (Natural 2))))) (IR.VPrim (Natural 3))

test_eval_sub ∷ T.TestTree
test_eval_sub = shouldEval (IR.Elim (IR.App (IR.App (IR.Prim Sub) (IR.Elim (IR.Prim (Natural 5)))) (IR.Elim (IR.Prim (Natural 2))))) (IR.VPrim (Natural 3))

--unit tests for cType
shouldCheck ∷ Term → Annotation → T.TestTree
shouldCheck term ann =
  T.testCase (show term <> " should check as type " <> show ann) $
    IR.cType naturals 0 [] term ann T.@=? Right ()

--unit tests for iType
shouldInfer ∷ Elim → Annotation → T.TestTree
shouldInfer term ann =
  T.testCase (show term <> " should infer to type " <> show ann) $
    IR.iType0 naturals [] term T.@=? Right ann

shouldEval ∷ Term → Value → T.TestTree
shouldEval term res =
  T.testCase (show term <> " should evaluate to " <> show res) $
    IR.cEval naturals term IR.initEnv T.@=? res

one ∷ Term
one =
  IR.Lam
    $ IR.Lam
    $ IR.Elim
    $ IR.App
      (IR.Bound 1)
      (IR.Elim (IR.Bound 0))

oneCompTy ∷ Annotation
oneCompTy =
  ( SNat 1,
    IR.VPi
      (SNat 1)
      ( IR.VPi
          (SNat 1)
          (IR.VPrimTy Nat)
          (const (IR.VPrimTy Nat))
      )
      ( const
          ( IR.VPi
              (SNat 1)
              (IR.VPrimTy Nat)
              (const (IR.VPrimTy Nat))
          )
      )
  )

two ∷ Term
two =
  IR.Lam
    $ IR.Lam
    $ IR.Elim
    $ IR.App
      (IR.Bound 1)
      ( IR.Elim
          ( IR.App
              (IR.Bound 1)
              (IR.Elim (IR.Bound 0))
          )
      )

twoCompTy ∷ Annotation
twoCompTy =
  ( SNat 1,
    IR.VPi
      (SNat 2)
      ( IR.VPi
          (SNat 1)
          (IR.VPrimTy Nat)
          (const (IR.VPrimTy Nat))
      )
      ( const
          ( IR.VPi
              (SNat 1)
              (IR.VPrimTy Nat)
              (const (IR.VPrimTy Nat))
          )
      )
  )
-- property tests of type checker:
-- the term's inferred type equals to the input type
-- property test of evaluator:
-- \x.x (any term) evaluates to (any term evaluated)
{- lamProp ∷ Term → Env → Property
lamProp cterm env = IR.App (cEval (IR.Lam IR.Bound 0) env) cterm == cterm-}
-- any constant term evaluates to itself
-- constProp ∷ Term → Env → Bool
-- constProp (Star i) env = cEval (Star i) env == IR.VStar i
-- constProp IR.PrimTy Nat env     = cEval IR.PrimTy Nat env == IR.VPrimTy Nat
-- constProp _ _          = True --Not testing non-const terms

{- TODO need to combine generators to generate
   Terms http://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Gen.html
instance Arbitrary Term where
  arbitrary = Term -}
