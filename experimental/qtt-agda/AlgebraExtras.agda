module AlgebraExtras where

open import Level
open import Algebra.Structures
open import Algebra.FunctionProperties
open import Relation.Binary
open import Relation.Binary.PropositionalEquality

-- "POSR" = Partially-Ordered SemiRing

module _ {a ℓ₁ ℓ₂} {A : Set a}
         (≈ : Rel A ℓ₁) (≤ : Rel A ℓ₂) (+ * : Op₂ A) (0# 1# : A)
 where
  record IsPOSR : Set (suc (a ⊔ ℓ₁ ⊔ ℓ₂)) where
    field
      isSemiring     : IsSemiring ≈ + * 0# 1#
      isPartialOrder : IsPartialOrder ≈ ≤
  
    open IsSemiring isSemiring public
      renaming (refl to ≈-refl ; reflexive to ≈-reflexive)
    open IsPartialOrder isPartialOrder public
      renaming (refl to ≤-refl ; reflexive to ≤-reflexive)

  record IsDecPOSR : Set (suc (a ⊔ ℓ₁ ⊔ ℓ₂)) where
    infix 4 _≟_ _≤?_
    field
      isPosr : IsPOSR
      _≟_ : Decidable ≈
      _≤?_ : Decidable ≤      


record RawPOSR′ c ℓ : Set (suc (c ⊔ ℓ)) where
  infixl 7 _*_
  infixl 6 _+_
  infix 4 _≤_
  field
    Carrier : Set c
    _≤_     : Rel Carrier ℓ
    _+_ _*_ : Op₂ Carrier
    0# 1#   : Carrier

record RawPOSR c ℓ₁ ℓ₂ : Set (suc (c ⊔ ℓ₁ ⊔ ℓ₂)) where
  field rawPosr′ : RawPOSR′ c ℓ₁
  open RawPOSR′ rawPosr′ public
  infix 4 _≈_
  field _≈_ : Rel Carrier ℓ₂


record POSR′ c ℓ : Set (suc (c ⊔ ℓ)) where
  field rawPosr′ : RawPOSR′ c ℓ
  open RawPOSR′ rawPosr′ public
  field isPosr : IsPOSR _≡_ _≤_ _+_ _*_ 0# 1#
  open IsPOSR isPosr public

record POSR c ℓ₁ ℓ₂ : Set (suc (c ⊔ ℓ₁ ⊔ ℓ₂)) where
  field rawPosr : RawPOSR c ℓ₁ ℓ₂
  open RawPOSR rawPosr public
  field isPosr : IsPOSR _≈_ _≤_ _+_ _*_ 0# 1#
  open IsPOSR isPosr public


record DecPOSR′ c ℓ : Set (suc (c ⊔ ℓ)) where
  field rawPosr′ : RawPOSR′ c ℓ
  open RawPOSR′ rawPosr′ public
  field isDecPosr : IsDecPOSR _≡_ _≤_ _+_ _*_ 0# 1#
  open IsDecPOSR isDecPosr public
  posr′ : POSR′ c ℓ
  posr′ = record { isPosr = isPosr }

record DecPOSR c ℓ₁ ℓ₂ : Set (suc (c ⊔ ℓ₁ ⊔ ℓ₂)) where
  field rawPosr : RawPOSR c ℓ₁ ℓ₂
  open RawPOSR rawPosr public
  field isDecPosr : IsDecPOSR _≈_ _≤_ _+_ _*_ 0# 1#
  open IsDecPOSR isDecPosr public
  posr : POSR c ℓ₁ ℓ₂
  posr = record { isPosr = isPosr }
