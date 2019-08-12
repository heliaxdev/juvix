module Example where

open import AlgebraExtras
open import Function
open import Data.Nat
open import Data.Fin
open import Relation.Binary.PropositionalEquality

open import Three
open import DL (DecPOSR′.posr′ decPosr)

variable
  n : ℕ
  e : Elim n

`f = Elim 2 ∋ ` zero
`x = Elim 2 ∋ ` suc zero

postulate
  A : ∀ {n} → Tm n
  A-ground : A {n} ≡ substₜ A e

-- the problem judgment from the bottom of p14 of the QTT paper (almost)
-- the example wouldn't mean much as-is because f is free so its usage wouldn't
-- be checked, so it's put in a λ here
-- ⊢ 1 (1 f: (1 x: A) → A) (ω x: A) → A ∋ λf x. f x
_ : ε ⊢ 1# - Π 1# (Π 1# A A) (Π ω# A A) ∋ Λ (Λ [ `f ∙ [ `x ] ]) ▷ ε
_ =
  lam refl
      (lam (1# ⊑ω#) -- ← hey look, subusaging‼
           (elim refl
                 (app 1# A refl refl A-ground
                      (var refl (ε ⨟0# ⨟ 1#))
                      (elim refl (var refl (ε ⨟ 1# ⨟0#))))))
