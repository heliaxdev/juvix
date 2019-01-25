import           Protolude

import qualified Data.Text        as T
import qualified System.IO        as IO
import qualified System.IO.Temp   as Temp
import qualified System.Process   as P
import qualified Test.Tasty       as T
import qualified Test.Tasty.HUnit as T

import qualified Juvix            as J
import qualified Juvix.Backends   as J
import qualified Juvix.Utility    as J

import           Code
import           Types

main ∷ IO ()
main = T.defaultMain (T.testGroup "Tests" [
  optimizationTests,
  compilationTests
  ])

compilationTests ∷ T.TestTree
compilationTests = T.testGroup "Compilation" (map compilationTestCase compilationTestCases)

optimizationTests ∷ T.TestTree
optimizationTests = T.testGroup "Optimization" [
  T.testCase "NOP removed" (J.optimizeNoLogs (J.Seq J.Nop J.Nop) == (J.Nop ∷ J.Expr (J.Stack ()) (J.Stack ())) T.@? "NOP was not removed"),
  T.testCase "Duplicate swap removed" (J.optimizeNoLogs (J.Seq J.Swap J.Swap) == (J.Nop ∷ J.Expr (J.Stack (Integer, (Integer, ()))) (J.Stack (Integer, (Integer, ())))) T.@? "Duplicate swap was not removed")
  ]

wrappedInterpret ∷ J.SomeExpr → J.DynamicValue → J.Tez → J.Timestamp → J.DynamicValue → T.Assertion
wrappedInterpret (J.SomeExpr (expr ∷ J.Expr (J.Stack a) (J.Stack b))) (J.DynamicValue (arg ∷ argType)) amount timestamp (J.DynamicValue (ret ∷ retType)) =
  case (eqT ∷ Maybe (a :~: (argType, ())), eqT ∷ Maybe (b :~: (retType, ()))) of
    (Nothing, _) → T.assertFailure ("Failed to unify argument type: expected " <> T.unpack (J.prettyPrintType arg))
    (_, Nothing) → T.assertFailure ("Failed to unify return type: expected " <> T.unpack (J.prettyPrintType ret))
    (Just Refl, Just Refl) ->
      let origination = J.OriginationNonce () 0
          context = J.Storage
          result ∷ Either J.InterpretError (retType, Int, J.Context, J.OriginationNonce)
          result = J.interpret origination maxBound undefined undefined amount context (J.Lambda' expr) arg
      in case result of
        Right (res, _, _, _) → T.assertBool ("Expected output did not match: expected " <> T.unpack (J.prettyPrintValue ret) <> " but instead got " <> T.unpack (J.prettyPrintValue res)) (res == ret)
        Left err → T.assertFailure ("Interpretation failed with error: " <> T.unpack (J.prettyPrintValue err))

compilationTestCase ∷ CompilationTestCase → T.TestTree
compilationTestCase (CompilationTestCase name idris inputs) =
  T.testCaseSteps (T.unpack name) $ \step → do
    path ← Temp.emptySystemTempFile "test.idr"
    step "Compiling to Michelson..."
    IO.writeFile path (T.unpack idris)
    P.callProcess "stack" ["exec", "--", "idris", "--interface", "--noprelude", "-p", "tezos", "--ibcsubdir", "tmp", "--codegen", "juvix", path, "-o", "test.tz"]
    {-
    compileResult ← J.compileToTyped path
    case compileResult of
      Left err → T.assertFailure ("Compilation failed with error: " <> T.unpack (J.prettyPrintValue err))
      Right (someExpr, paramTy, retTy, storageTy) → do
        step ("compilation OK; param type " <> T.unpack (J.prettyPrintValue paramTy) <> ", return type " <> T.unpack (J.prettyPrintValue retTy) <> ", storage type " <> T.unpack (J.prettyPrintValue storageTy))
        step ("Result: " <> (T.unpack (case someExpr of J.SomeExpr e → J.emit e)))
    -}
