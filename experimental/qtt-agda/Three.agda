module _ where

open import Level
open import Algebra.Structures
open import Algebra.FunctionProperties
open import AlgebraExtras
open import Data.Product
open import Relation.Binary
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary


data Three : Set where 0# 1# ω# : Three
private variable π π′ ρ ρ′ : Three

_⊕_ : Op₂ Three
0# ⊕ ρ  = ρ
1# ⊕ 0# = 1#
1# ⊕ 1# = ω#
1# ⊕ ω# = ω#
ω# ⊕ _  = ω#
infixl 10 _⊕_

_⊗_ : Op₂ Three
0# ⊗ _  = 0#
1# ⊗ ρ  = ρ
ω# ⊗ 0# = 0#
ω# ⊗ 1# = ω#
ω# ⊗ ω# = ω#
infixl 11 _⊗_

data _⊑_ : Rel Three zero where
  refl : π ⊑ π
  _⊑ω# : ∀ π → π ⊑ ω#
infix 4 _⊑_ _⊑ω#

_≟_ : Decidable (_≡_ {A = Three})
0# ≟ 0# = yes refl
0# ≟ 1# = no (λ ())
0# ≟ ω# = no (λ ())
1# ≟ 0# = no (λ ())
1# ≟ 1# = yes refl
1# ≟ ω# = no (λ ())
ω# ≟ 0# = no (λ ())
ω# ≟ 1# = no (λ ())
ω# ≟ ω# = yes refl

_⊑?_ : Decidable _⊑_
0# ⊑? 0# = yes refl
0# ⊑? 1# = no (λ ())
0# ⊑? ω# = yes (0# ⊑ω#)
1# ⊑? 0# = no (λ ())
1# ⊑? 1# = yes refl
1# ⊑? ω# = yes (1# ⊑ω#)
ω# ⊑? 0# = no (λ ())
ω# ⊑? 1# = no (λ ())
ω# ⊑? ω# = yes refl


-- the tedious proofs are entirely solvable by auto, thankfully

⊑-isPartialOrder : IsPartialOrder _≡_ _⊑_
⊑-isPartialOrder =
  record {
    isPreorder = record {
      isEquivalence = isEquivalence ;
      reflexive = λ{refl → refl} ;
      trans = ⊑-trans
    } ;
    antisym = ⊑-antisym
  }
  where
    ⊑-trans : ∀ {i j k} → i ⊑ j → j ⊑ k → i ⊑ k
    ⊑-trans     ij refl    = ij
    ⊑-trans {i} _  (j ⊑ω#) = i ⊑ω#

    ⊑-antisym : ∀ {i j} → i ⊑ j → j ⊑ i → i ≡ j
    ⊑-antisym refl     _ = refl
    ⊑-antisym (ω# ⊑ω#) _ = refl
 
isSemiring : IsSemiring _≡_ _⊕_ _⊗_ 0# 1#
isSemiring =
  record {
    isSemiringWithoutAnnihilatingZero = record {
      +-isCommutativeMonoid = record {
        isSemigroup = record {
          isMagma = record {
            isEquivalence = isEquivalence ;
            ∙-cong = λ{refl refl → refl}
          } ;
          assoc = ⊕-assoc
        } ;
        identityˡ = λ x → refl ;
        comm = ⊕-comm
      } ;
      *-isMonoid = record {
        isSemigroup = record {
          isMagma = record {
            isEquivalence = isEquivalence ;
            ∙-cong = λ{refl refl → refl}
          } ;
          assoc = ⊗-assoc
        } ;
        identity = (λ x → refl) , (λ{0# → refl ; 1# → refl ; ω# → refl})
      } ;
      distrib = distribˡ , distribʳ
    } ;
    zero = (λ x → refl) , (λ{0# → refl ; 1# → refl ; ω# → refl})
  }
  where
    ⊕-assoc : ∀ x y z → x ⊕ y ⊕ z ≡ x ⊕ (y ⊕ z)
    ⊕-assoc 0# _  _  = refl
    ⊕-assoc 1# 0# _  = refl
    ⊕-assoc 1# 1# 0# = refl
    ⊕-assoc 1# 1# 1# = refl
    ⊕-assoc 1# 1# ω# = refl
    ⊕-assoc 1# ω# _  = refl
    ⊕-assoc ω# _  _  = refl

    ⊕-comm : ∀ x y → x ⊕ y ≡ y ⊕ x
    ⊕-comm 0# 0# = refl
    ⊕-comm 0# 1# = refl
    ⊕-comm 0# ω# = refl
    ⊕-comm 1# 0# = refl
    ⊕-comm 1# 1# = refl
    ⊕-comm 1# ω# = refl
    ⊕-comm ω# 0# = refl
    ⊕-comm ω# 1# = refl
    ⊕-comm ω# ω# = refl

    ⊗-assoc : ∀ x y z → x ⊗ y ⊗ z ≡ x ⊗ (y ⊗ z)
    ⊗-assoc 0# _  _  = refl
    ⊗-assoc 1# _  _  = refl
    ⊗-assoc ω# 0# _  = refl
    ⊗-assoc ω# 1# _  = refl
    ⊗-assoc ω# ω# 0# = refl
    ⊗-assoc ω# ω# 1# = refl
    ⊗-assoc ω# ω# ω# = refl

    distribˡ : ∀ x y z → x ⊗ (y ⊕ z) ≡ x ⊗ y ⊕ x ⊗ z
    distribˡ 0# _  _  = refl
    distribˡ 1# _  _  = refl
    distribˡ ω# 0# _  = refl
    distribˡ ω# 1# 0# = refl
    distribˡ ω# 1# 1# = refl
    distribˡ ω# 1# ω# = refl
    distribˡ ω# ω# _  = refl

    distribʳ : ∀ x y z → (y ⊕ z) ⊗ x ≡ y ⊗ x ⊕ z ⊗ x
    distribʳ _  0# _  = refl
    distribʳ 0# 1# 0# = refl
    distribʳ 0# 1# 1# = refl
    distribʳ 0# 1# ω# = refl
    distribʳ 1# 1# 0# = refl
    distribʳ 1# 1# 1# = refl
    distribʳ 1# 1# ω# = refl
    distribʳ ω# 1# 0# = refl
    distribʳ ω# 1# 1# = refl
    distribʳ ω# 1# ω# = refl
    distribʳ 0# ω# 0# = refl
    distribʳ 0# ω# 1# = refl
    distribʳ 0# ω# ω# = refl
    distribʳ 1# ω# _  = refl
    distribʳ ω# ω# _  = refl

isDecPosr : IsDecPOSR _≡_ _⊑_ _⊕_ _⊗_ 0# 1#
isDecPosr =
  record {
    isPosr = record {
      isSemiring     = isSemiring ;
      isPartialOrder = ⊑-isPartialOrder
    } ;
    _≟_ = _≟_ ;
    _≤?_ = _⊑?_
  }

decPosr : DecPOSR′ _ _
decPosr = record { isDecPosr = isDecPosr }
