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

type TestEval primTy primVal =
  ( HasCallStack,
    TestEvalPrim primTy primVal primTy,
    TestEvalPrim primTy primVal primVal,
    Eq (Eval.Error IR.NoExt IR.NoExt primTy primVal),
    Show (Eval.Error IR.NoExt IR.NoExt primTy primVal)
  )

type TestEvalPrim primTy primVal a =
  ( Eq a, Show a, CanApply a,
    Eval.HasPatSubstTerm (OnlyExts.T IR.NoExt) primTy primVal a,
    Eval.HasSubstValue IR.NoExt primTy primVal a
  )


-- unit test generator for evalTerm
shouldEval' ::
  TestEval primTy primVal =>
  T.TestName ->
  IR.Globals primTy primVal ->
  IR.Term primTy primVal ->
  IR.Value primTy primVal ->
  T.TestTree
shouldEval' name g term res =
  let look x = Map.lookup x g in
  T.testCase name $ IR.evalTerm look term T.@=? Right res

shouldEval ::
  TestEval primTy primVal =>
  T.TestName ->
  IR.Term primTy primVal ->
  IR.Value primTy primVal ->
  T.TestTree
shouldEval name = shouldEval' name mempty

evaluatorTests :: T.TestTree
evaluatorTests =
  T.testGroup
    "Evaluator tests"
    [ shouldEval "1 + 2" add12 (natV 3),
      shouldEval "5 - 2" sub52 (natV 3),
      shouldEval "I 1" identityApplication (natV 1),
      shouldEval "I I 1" (IR.Elim identityAppINat1) (natV 1),
      shouldEval "I I" (IR.Elim identityAppI) videntity,
      shouldEval "K 1 2" (IR.Elim kApp1_2) (natV 1),
      shouldEval' "ty" typGlobals (name "ty") (IR.VStar 0),
      shouldEval' "tz" typGlobals (name "tz") (vname "tz"),
      shouldEval' "B" typGlobals (name "B") (vname "A"),
      shouldEval' "C" typGlobals (name "C") (vname "A")
    ]
  where
    add12 = IR.Elim $ add `IR.App` nat 1 `IR.App` nat 2
    sub52 = IR.Elim $ sub `IR.App` nat 5 `IR.App` nat 2
    sub = IR.Ann Usage.Omega (IR.Prim Nat.Sub) addTyT 0
    videntity = IR.VLam $ IR.VBound 0
    name = IR.Elim . IR.Free . IR.Global
    vname = IR.VFree . IR.Global
