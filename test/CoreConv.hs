module CoreConv where

import Juvix.Core
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

shouldConvertHR ∷ HR.Term () () → IR.Term () () → T.TestTree
shouldConvertHR hr ir =
  T.testCase (show hr <> " should convert to " <> show ir) (ir T.@=? hrToIR hr)

shouldConvertIR ∷ IR.Term () () → HR.Term () () → T.TestTree
shouldConvertIR ir hr =
  T.testCase (show ir <> " should convert to " <> show hr) (hr T.@=? irToHR ir)

test_identity_hr ∷ T.TestTree
test_identity_hr = shouldConvertHR (HR.Lam "x" (HR.Elim (HR.Var "x"))) (IR.Lam (IR.Elim (IR.Bound 0)))

test_const_hr ∷ T.TestTree
test_const_hr = shouldConvertHR (HR.Lam "x" (HR.Lam "y" (HR.Elim (HR.Var "x")))) (IR.Lam (IR.Lam (IR.Elim (IR.Bound 1))))

test_ignore_hr ∷ T.TestTree
test_ignore_hr = shouldConvertHR (HR.Lam "x" (HR.Lam "y" (HR.Elim (HR.Var "y")))) (IR.Lam (IR.Lam (IR.Elim (IR.Bound 0))))

test_identity_ir ∷ T.TestTree
test_identity_ir = shouldConvertIR (IR.Lam (IR.Elim (IR.Bound 0))) (HR.Lam "0" (HR.Elim (HR.Var "0")))

test_const_ir ∷ T.TestTree
test_const_ir = shouldConvertIR (IR.Lam (IR.Lam (IR.Elim (IR.Bound 1)))) (HR.Lam "0" (HR.Lam "1" (HR.Elim (HR.Var "0"))))

test_ignore_ir ∷ T.TestTree
test_ignore_ir = shouldConvertIR (IR.Lam (IR.Lam (IR.Elim (IR.Bound 0)))) (HR.Lam "0" (HR.Lam "1" (HR.Elim (HR.Var "1"))))
