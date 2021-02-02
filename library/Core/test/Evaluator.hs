-- | Tests for the evaluator in Core/IR/Evaluator.hs
module Evaluator where

import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Evaluator as Eval
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import qualified Juvix.Core.Parameterisations.Naturals as Nat
import Juvix.Core.Types
import Juvix.Library hiding (identity)
import Juvix.Library.HashMap as Map
import qualified Juvix.Library.Usage as Usage
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Typechecker.Terms

-- unit test generator for evalTerm
shouldEval' ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply primVal,
    CanApply primTy,
    Eq (Eval.Error IR.NoExt IR.NoExt primTy primVal),
    Show (Eval.Error IR.NoExt IR.NoExt primTy primVal),
    Eval.HasPatSubstTerm (OnlyExts.T IR.NoExt) primTy primVal primTy,
    Eval.HasPatSubstTerm (OnlyExts.T IR.NoExt) primTy primVal primVal,
    Eval.HasSubstValue IR.NoExt primTy primVal primTy,
    Eval.HasSubstValue IR.NoExt primTy primVal primVal
  ) =>
  IR.Globals primTy primVal ->
  IR.Term primTy primVal ->
  IR.Value primTy primVal ->
  T.TestTree
shouldEval' g term res =
  T.testCase (show term <> " should evaluate to " <> show res) $
    (IR.evalTerm (\x -> Map.lookup x g) term) T.@=? Right res

shouldEval ::
  ( HasCallStack,
    Show primTy,
    Show primVal,
    Eq primTy,
    Eq primVal,
    CanApply primVal,
    CanApply primTy,
    Eq (Eval.Error IR.NoExt IR.NoExt primTy primVal),
    Show (Eval.Error IR.NoExt IR.NoExt primTy primVal),
    Eval.HasPatSubstTerm (OnlyExts.T IR.NoExt) primTy primVal primTy,
    Eval.HasPatSubstTerm (OnlyExts.T IR.NoExt) primTy primVal primVal,
    Eval.HasSubstValue IR.NoExt primTy primVal primTy,
    Eval.HasSubstValue IR.NoExt primTy primVal primVal
  ) =>
  IR.Term primTy primVal ->
  IR.Value primTy primVal ->
  T.TestTree
shouldEval = shouldEval' mempty

evaluatorTests :: T.TestTree
evaluatorTests =
  T.testGroup
    "Evaluator tests"
    [ shouldEval add12 (natV 3),
      shouldEval sub52 (natV 3),
      shouldEval identityApplication (natV 1),
      shouldEval (IR.Elim identityAppINat1) (natV 1),
      shouldEval (IR.Elim identityAppI) videntity,
      shouldEval (IR.Elim kApp1_2) (natV 1),
      shouldEval' typGlobals (IR.Elim (IR.Free (IR.Global "ty"))) (IR.VStar 0),
      shouldEval' typGlobals (name "tz") (vname "tz"),
      shouldEval' typGlobals (name "B") (vname "A"),
      shouldEval' typGlobals (name "C") (vname "A")
    ]
  where
    add12 = IR.Elim $ add `IR.App` nat 1 `IR.App` nat 2
    sub52 = IR.Elim $ sub `IR.App` nat 5 `IR.App` nat 2
    sub = IR.Ann Usage.Omega (IR.Prim Nat.Sub) addTyT 0
    videntity = IR.VLam $ IR.VBound 0
    name = IR.Elim . IR.Free . IR.Global
    vname = IR.VFree . IR.Global
