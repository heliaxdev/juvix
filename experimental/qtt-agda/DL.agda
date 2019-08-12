open import AlgebraExtras

-- todo: use a posr with arbitrary equivalence instead of _≡_?
-- not really the point tho
module DL {c ℓ} (posr′ : POSR′ c ℓ) where

open import Algebra.Structures
open import Level using (0ℓ ; _⊔_)
open import Function
open import Data.Nat as ℕ hiding (_⊔_)
open import Data.Fin as Fin
open import Data.Product hiding (Σ)
open import Relation.Binary
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary

private variable m n : ℕ

module Quantity = POSR′ posr′
open Quantity using (0# ; 1#)
  renaming (Carrier to Quantity ; _+_ to _⊕_ ; _*_ to _⊗_ ; _≤_ to _⊑_)
private variable π π′ ρ ρ′ : Quantity

Var = Fin
private variable x y : Var n

Universe = ℕ
private variable u v : Universe

data Tm n : Set c
data Elim n : Set c
Typ = Tm

data Tm n where
  sort : (u : Universe) → Typ n
  Π    : (π : Quantity) (S : Typ n) (T : Typ (suc n)) → Typ n
  Λ    : (t : Tm (suc n)) → Tm n
  [_]  : (e : Elim n) → Tm n
private variable s s′ t t′ : Tm n ; S S′ T T′ R R′ : Typ n

data Elim n where
  `_  : (x : Var n) → Elim n
  _∙_ : (f : Elim n) (s : Tm n) → Elim n
  _⦂_ : (s : Tm n) (S : Typ n) → Elim n
infix 1000 `_ ; infixl 200 _∙_ ; infix 100 _⦂_
private variable e e′ f f′ : Elim n


data _≼_ : Rel (Typ n) 0ℓ where
  sort : u ℕ.≤ v → sort u ≼ sort {n} v
  Π    : S′ ≼ S → T ≼ T′ → Π π S T ≼ Π π S′ T′
  refl : S ≼ S
  -- (maybe recurse into other structures?)
infix 4 _≼_

weakₜ′ : Var (suc n) → Tm n → Tm (suc n)
weakₑ′ : Var (suc n) → Elim n → Elim (suc n)
weakₜ′ i (sort u)  = sort u
weakₜ′ i (Π π S T) = Π π (weakₜ′ i S) (weakₜ′ (suc i) T)
weakₜ′ i (Λ t)     = Λ (weakₜ′ (suc i) t)
weakₜ′ i [ e ]     = [ weakₑ′ i e ]
weakₑ′ i (` x)     = ` punchIn i x
weakₑ′ i (f ∙ s)   = weakₑ′ i f ∙ weakₜ′ i s
weakₑ′ i (s ⦂ S)   = weakₜ′ i s ⦂ weakₜ′ i S

weakₜ : Tm n → Tm (suc n)
weakₜ = weakₜ′ zero
weakₑ : Elim n → Elim (suc n)
weakₑ = weakₑ′ zero

substₜ′ : Var (suc n) → Tm (suc n) → Elim n → Tm n
substₑ′ : Var (suc n) → Elim (suc n) → Elim n → Elim n
substₜ′ i (sort u) e = sort u
substₜ′ i (Π π S T) e = Π π (substₜ′ i S e) (substₜ′ (suc i) T (weakₑ′ i e))
substₜ′ i (Λ t) e = Λ (substₜ′ (suc i) t (weakₑ′ i e))
substₜ′ i [ f ] e = [ substₑ′ i f e ]
substₑ′ i (` x) e =
  case i Fin.≟ x of λ{(yes _) → e ; (no i≢x) → ` Fin.punchOut i≢x}
substₑ′ i (f ∙ s) e = substₑ′ i f e ∙ substₜ′ i s e
substₑ′ i (s ⦂ S) e = substₜ′ i s e ⦂ substₜ′ i S e

substₜ : Tm (suc n) → Elim n → Tm n
substₜ = substₜ′ zero
substₑ : Elim (suc n) → Elim n → Elim n
substₑ = substₑ′ zero

data _⟿ₜ_ : Rel (Tm n) 0ℓ
data _⟿ₑ_ : Rel (Elim n) 0ℓ
infix 1 _⟿ₜ_ _⟿ₑ_

data _⟿ₜ_ where
  υ : [ t ⦂ T ] ⟿ₜ t
  Πˡ : S ⟿ₜ S′ → Π π S T ⟿ₜ Π π S′ T
  -- reducing under binders?
  Πʳ : T ⟿ₜ T′ → Π π S T ⟿ₜ Π π S T′
  Λ : t ⟿ₜ t′ → Λ t ⟿ₜ Λ t′
  [_] : e ⟿ₑ e′ → [ e ] ⟿ₜ [ e′ ]

data _⟿ₑ_ where
  β : (Λ t ⦂ Π π S T) ∙ s ⟿ₑ substₑ (t ⦂ T) (s ⦂ S)
  ∙ˡ : f ⟿ₑ f′ → f ∙ s ⟿ₑ f′ ∙ s
  ∙ʳ : s ⟿ₜ s′ → f ∙ s ⟿ₑ f ∙ s′
  ⦂ˡ : s ⟿ₜ s′ → s ⦂ S ⟿ₑ s′ ⦂ S
  ⦂ʳ : S ⟿ₜ S′ → s ⦂ S ⟿ₑ s ⦂ S′


data Ctx′ {a} (F : ℕ → Set a) : ℕ → Set a where
  ε   : Ctx′ F 0
  _⨟_ : Ctx′ F n → F n → Ctx′ F (suc n)
infixl 5 _⨟_

module _ where
  private variable F G H : ℕ → Set _

  czipWith : (∀ {n} → F n → G n → H n) → Ctx′ F n → Ctx′ G n → Ctx′ H n
  czipWith f ε ε = ε
  czipWith f (xs ⨟ x) (ys ⨟ y) = czipWith f xs ys ⨟ f x y

  clookup′ : (∀ {n} → F n → F (suc n)) → Ctx′ F n → Var n → F n
  clookup′ f (xs ⨟ x) zero = f x
  clookup′ f (xs ⨟ x) (suc i) = f $ clookup′ f xs i

Ctx = Ctx′ Typ
private variable Γ Γ₁ Γ₂ : Ctx n

clookup : Ctx n → Var n → Typ n
clookup = clookup′ weakₜ

Skel = Ctx′ λ _ → Quantity
private variable Σ Σ₁ Σ₂ : Skel n

slookup : Skel n → Var n → Quantity
slookup = clookup′ id

data Zero : Skel n → Set where
  ε    : Zero ε
  _⨟0# : Zero Σ → Zero (Σ ⨟ 0#)
infixl 5 _⨟0#

data Only : Quantity → Fin n → Skel n → Set where
  _⨟_  : Zero Σ → ∀ ρ → Only ρ zero (Σ ⨟ ρ)
  _⨟0# : Only ρ x Σ → Only ρ (suc x) (Σ ⨟ 0#)

_⊞_ : Skel n → Skel n → Skel n
_⊞_ = czipWith _⊕_
infixl 10 _⊞_

data _⊑*_ : Rel (Skel n) ℓ where
  ε : ε ⊑* ε
  _⨟_ : Σ₁ ⊑* Σ₂ → π ⊑ ρ → Σ₁ ⨟ π ⊑* Σ₂ ⨟ ρ
infix 4 _⊑*_


data _⊢_-_∋_▷_ : Ctx n → Quantity → Typ n → Tm n → Skel n → Set (c ⊔ ℓ)
data _⊢_-_∈_▷_ : Ctx n → Quantity → Elim n → Typ n → Skel n → Set (c ⊔ ℓ)
infix 0 _⊢_-_∋_▷_ _⊢_-_∈_▷_

data _⊢_-_∋_▷_ where
  pre : T ⟿ₜ R →
        Γ ⊢ ρ - R ∋ t ▷ Σ →
        Γ ⊢ ρ - T ∋ t ▷ Σ
  sort : u ℕ.< v → Zero Σ →
         Γ ⊢ 0# - sort v ∋ sort u ▷ Σ
  fun : Zero Σ →
        Γ ⊢ 0# - sort u ∋ S ▷ Σ →
        Γ ⨟ S ⊢ 0# - sort u ∋ T ▷ Σ ⨟ 0# →
        Γ ⊢ 0# - sort u ∋ Π π S T ▷ Σ
  lam : ρ′ ⊑ ρ ⊗ π →   -- for no subusaging: ρ′ ≡ ρ ⊗ π
        Γ ⨟ S ⊢ ρ - T ∋ t ▷ Σ ⨟ ρ′ →
        Γ ⊢ ρ - Π π S T ∋ Λ t ▷ Σ
  elim : S ≼ T →
         Γ ⊢ ρ - e ∈ S ▷ Σ →
         Γ ⊢ ρ - T ∋ [ e ] ▷ Σ

data _⊢_-_∈_▷_ where
  post : Γ ⊢ ρ - e ∈ S ▷ Σ → S ⟿ₜ R →
         Γ ⊢ ρ - e ∈ R ▷ Σ
  var : clookup Γ x ≡ S → Only ρ x Σ →
        Γ ⊢ ρ - ` x ∈ T ▷ Σ
    -- var just use whatever ρ it's told. lam will check that it's ok later.
  app : ∀ π S → -- (these seem to be hard for Agda to infer)
        ρ′ ≡ ρ ⊗ π → Σ ≡ Σ₁ ⊞ Σ₂ → T′ ≡ substₜ T (s ⦂ S) → 
        Γ ⊢ ρ - f ∈ Π π S T ▷ Σ₁ →
        Γ ⊢ ρ′ - S ∋ s ▷ Σ₂ →
        Γ ⊢ ρ - f ∙ s ∈ T′ ▷ Σ
  cut : Zero Σ₁ →
        Γ ⊢ 0# - sort u ∋ S ▷ Σ₁ →
        Γ ⊢ ρ - S ∋ s ▷ Σ₂ →
        Γ ⊢ ρ - s ⦂ S ∈ S ▷ Σ₂
