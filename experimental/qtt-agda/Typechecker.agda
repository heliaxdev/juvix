open import AlgebraExtras

module Typechecker {c ℓ} (decPosr : DecPOSR′ c ℓ) where

open import Data.Nat
open import Data.Product
open import Data.Maybe

open module Quantity = DecPOSR′ decPosr
  using (0# ; _≟_) renaming (_≤?_ to _⊑?_)
open import DL Quantity.posr′

private
 variable
  n : ℕ

chk : ∀ (Γ : Ctx n) t T ρ → Maybe (∃  (Γ ⊢ ρ - T ∋ t ▷_))
syn : ∀ (Γ : Ctx n) e   ρ → Maybe (∃₂ (Γ ⊢ ρ - e ∈_▷_))

chk Γ t T ρ = {!!}

syn Γ e ρ = {!!}
