module CoreParser where

import Juvix.Core.HR
import Juvix.Core.Usage
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (String)

-- | Term parser unit test generator.
shouldParse ∷ String → STerm → T.TestTree
shouldParse str parsed =
  T.testCase (show str <> " should parse as " <> show parsed) $
    Just parsed T.@=? parseString (parseWhole term) str

test_star_n ∷ T.TestTree
test_star_n = shouldParse "* 0" (Star 0)

test_star_n_parens ∷ T.TestTree
test_star_n_parens = shouldParse "(* 1)" (Star 1)

-- test_primitive_type_Nats ∷ T.TestTree
-- test_primitive_type_Nats = shouldParse "Nat" Nats

test_dependent_fun ∷ T.TestTree
test_dependent_fun =
  shouldParse
    "[Π] 1 * 0 * 0"
    (Pi (SNat 1) (Star 0) (Star 0))

test_lam_identity ∷ T.TestTree
test_lam_identity = shouldParse "\\x -> x" (Lam "x" (Elim (Var "x")))

test_lam_basic ∷ T.TestTree
test_lam_basic = shouldParse "\\x -> y" (Lam "x" (Elim (Var "y")))

test_lam_nested ∷ T.TestTree
test_lam_nested = shouldParse "\\x -> \\y -> x" (Lam "x" (Lam "y" (Elim (Var "x"))))

test_lam_nested_app ∷ T.TestTree
test_lam_nested_app = shouldParse "\\x -> \\y -> x y" (Lam "x" (Lam "y" (Elim (App (Elim (Var "x")) (Var "y")))))

test_parse_app ∷ T.TestTree
test_parse_app = shouldParse "(\\x -> x) y" (Elim (App (Lam "x" (Elim (Var "x"))) (Var "y")))

-- test_conversion ∷ T.TestTree
-- test_conversion = shouldParse "Elim 0" (Elim (Nat 0))

-- test_silent_convert_nat ∷ T.TestTree
-- test_silent_convert_nat = shouldParse "0" (Elim (Nat 0))

test_silent_convert_var ∷ T.TestTree
test_silent_convert_var = shouldParse "xyz" (Elim (Var "xyz"))

test_silent_convert_app ∷ T.TestTree
test_silent_convert_app = shouldParse "fun var" (Elim (App (Elim (Var "fun")) (Var "var")))

test_silent_convert_ann ∷ T.TestTree
test_silent_convert_ann = shouldParse "(* 0) : w (* 0)" (Elim (Ann Omega (Star 0) (Star 0)))
