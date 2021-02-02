-- | Tests for the type checker in Core/IR/Typechecker.hs
module Typechecker where

import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.Typechecker as TC
import qualified Juvix.Core.Parameterisations.All as All
import qualified Juvix.Core.Parameterisations.Naturals as Nat
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Core.Types
import Juvix.Library hiding (identity)
import qualified Juvix.Library.Usage as Usage
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Typechecker.Terms
import Typechecker.Types

assertIsRight :: (HasCallStack, Show a) => Either a b -> T.Assertion
assertIsRight (Right _) = pure ()
assertIsRight (Left l) =
  T.assertFailure $
    "expected a Right, got\n\t"
      ++ "Left ("
      ++ show l
      ++ ")"

-- unit test generator for typeTerm
shouldCheckWith ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Show (Arg primTy),
    Show (Arg (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    TC.PrimSubstValue primTy primVal,
    TC.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal
  ) =>
  Parameterisation primTy primVal ->
  IR.GlobalsT primTy primVal ->
  IR.Context primTy primVal ->
  IR.Term primTy primVal ->
  IR.AnnotationT primTy primVal ->
  T.TestTree
shouldCheckWith param globals ctx term ann =
  -- TODO: take out the logs and put them in an IO monad.
  let (res, _) = TC.exec globals $ TC.typeTermWith param mempty ctx term ann
   in T.testCase
        ( show term
            <> " should check as type "
            <> show ann
        )
        $ assertIsRight res

shouldCheck ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Show (Arg primTy),
    Show (Arg (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    TC.PrimSubstValue primTy primVal,
    TC.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal
  ) =>
  Parameterisation primTy primVal ->
  IR.Term primTy primVal ->
  IR.AnnotationT primTy primVal ->
  T.TestTree
shouldCheck param = shouldCheckWith param mempty []

-- unit test generator for typeElim
shouldInferWith ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Eq (Arg primTy),
    Show (Arg primTy),
    Eq (Arg (TypedPrim primTy primVal)),
    Show (Arg (TypedPrim primTy primVal)),
    Eq (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra primTy),
    Eq (ApplyErrorExtra (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    TC.PrimSubstValue primTy primVal,
    TC.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal
  ) =>
  Parameterisation primTy primVal ->
  IR.GlobalsT primTy primVal ->
  IR.Context primTy primVal ->
  IR.Elim primTy primVal ->
  IR.AnnotationT primTy primVal ->
  T.TestTree
shouldInferWith param globals ctx elim ann@(IR.Annotation {annUsage = σ}) =
  let (res, _) = TC.exec globals $ TC.typeElimWith param mempty ctx elim σ
      resTy = TC.getElimAnn . TC.loValue <$> res
   in T.testCase (show term <> " should infer to type " <> show ann) $
        resTy T.@?= Right ann

shouldInfer ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply (TypedPrim primTy primVal),
    CanApply primTy,
    Eq (Arg primTy),
    Show (Arg primTy),
    Eq (Arg (TypedPrim primTy primVal)),
    Show (Arg (TypedPrim primTy primVal)),
    Eq (ApplyErrorExtra primTy),
    Show (ApplyErrorExtra primTy),
    Eq (ApplyErrorExtra (TypedPrim primTy primVal)),
    Show (ApplyErrorExtra (TypedPrim primTy primVal)),
    TC.PrimSubstValue primTy primVal,
    TC.PrimPatSubstTerm primTy primVal,
    Eval.HasWeak primVal
  ) =>
  Parameterisation primTy primVal ->
  IR.Elim primTy primVal ->
  IR.AnnotationT primTy primVal ->
  T.TestTree
shouldInfer param = shouldInferWith param mempty []

typecheckerTests :: T.TestTree
typecheckerTests =
  T.testGroup
    "Core type checker tests"
    [ skiComp,
      natComp,
      dependentFunctionComp,
      dependentPairComp,
      letComp,
      skiCont,
      subtype
    ]

skiComp :: T.TestTree
skiComp =
  T.testGroup
    "SKI combinators Computational typing"
    [ shouldCheck Nat.t identity identityNatCompTy,
      shouldCheck Unit.t identity identityUnitCompTy,
      shouldCheck Nat.t identityApplication natTy,
      shouldInfer Nat.t identityAppINat1 natTy,
      shouldInfer Nat.t identityAppI identityNatCompTy,
      shouldCheck Nat.t kcombinator kCompTy,
      shouldCheck All.t kcombinator kCompTyWithUnit,
      shouldInfer Nat.t identityAppK kCompTy,
      shouldCheck Nat.t (IR.Elim kAppI) kAppICompTy,
      shouldCheck Nat.t (IR.Elim kAppINotAnnotated) kAppICompTy,
      shouldInfer Nat.t kApp1 natToNatTy,
      shouldInfer
        Nat.t
        kFunApp1
        kFunApp1CompTy
    ]

natComp :: T.TestTree
natComp =
  T.testGroup
    "Nat Computational typing"
    [ shouldCheck Nat.t natT' (mempty `ann` IR.VStar 0),
      shouldCheck Nat.t (nat 1) (Usage.Omega `ann` natT),
      shouldCheck Nat.t (IR.Prim Nat.Add) (Usage.Omega `ann` addTy)
    ]

dependentFunctionComp :: T.TestTree
dependentFunctionComp =
  T.testGroup
    "Dependent Functions Computational typing"
    [ shouldCheck
        All.t
        depIdentity
        depIdentityCompTy,
      shouldCheck
        All.t
        depIdentity
        depIdentityCompTyOmega,
      shouldCheck
        All.t
        depK
        depKCompTy
    ]

letComp :: T.TestTree
letComp =
  T.testGroup
    "'let' Computational typing"
    [ -- let 0 x = 0 in 0
      shouldCheck
        Nat.t
        (IR.Let mempty nzero (IR.Elim nzero))
        (Usage.Omega `ann` natT),
      -- let ω x = 0 in x
      shouldCheck
        Nat.t
        (IR.Let Usage.Omega nzero (IR.Elim (IR.Bound 0)))
        (Usage.Omega `ann` natT),
      -- λx. let 0 y = 0 in x
      shouldCheck
        Nat.t
        (IR.Lam (IR.Let mempty nzero (IR.Elim (IR.Bound 1))))
        (natToNatTy' one)
    ]
  where
    nzero = IR.Ann Usage.Omega (nat 0) natT' 0

skiCont :: T.TestTree
skiCont =
  T.testGroup
    "SKI combinators contemplational typing"
    [ shouldCheck Nat.t identity identityNatContTy
    ]

subtype :: T.TestTree
subtype =
  T.testGroup
    "Subtyping"
    [ shouldCheckWith Unit.t typGlobals [] aTerm $ mempty `ann` IR.VStar 0,
      shouldCheckWith Unit.t typGlobals [] aTerm $ mempty `ann` IR.VStar 1,
      shouldCheckWith Unit.t typGlobals [] fTerm $ mempty `ann` typ2typ 1 1,
      shouldCheckWith Unit.t typGlobals [] fTerm $ mempty `ann` typ2typ 0 1,
      shouldCheckWith Unit.t typGlobals [] fTerm $ mempty `ann` typ2typ 1 2,
      shouldInferWith Unit.t typGlobals [] faElim $ mempty `ann` IR.VStar 1
    ]
  where
    typ2typ i j = IR.VPi mempty (IR.VStar i) (IR.VStar j)

dependentPairComp :: T.TestTree
dependentPairComp =
  T.testGroup "Dependent pair typing" $
    [shouldCheck Nat.t boxNat boxNatAnn]
