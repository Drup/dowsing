open import Data.Product as Product
    using (_,_ ; ∃-syntax ; proj₁ ; proj₂)

open import Data.Empty as Empty
    using (⊥-elim)

import Data.Fin as Fin

open import Data.Vec as Vec
    using ([] ; _∷_)

open import Function
    using (_$_ ; _∘_)

open import Relation.Binary.PropositionalEquality as Eq
    using (_≡_ ; _≢_ ; refl)

open import Relation.Nullary
    using (¬_)

open import Type

infix 30 ↑_
↑_ : Type → Type
↑ (_ ⇒ τ) = ↑ τ
↑ τ = τ

↑-idempotent : ∀ {τ} →
    ↑ ↑ τ ≡ ↑ τ
↑-idempotent {var _} = refl
↑-idempotent {_ ⇒ τ} = ↑-idempotent {τ}
↑-idempotent {_ * _} = refl
↑-idempotent {cons (Σ.cons _) _} = refl

↑∉⇒ : ∀ {τ} →
    ↑ τ ∉⇒
↑∉⇒ {_ ⇒ τ} ↑τ∈⇒ = ↑∉⇒ {τ} ↑τ∈⇒

↑≢⇒ : ∀ {τ f τ‥} →
    ↑ τ ≡ cons f τ‥ →
    f ≢ Σ.⇒
↑≢⇒ {τ = τ} {τ‥ = τ‥} ↑τ≡f refl = ↑∉⇒ {τ} (τ‥ , ↑τ≡f)

↑-∉⇒ : ∀ {τ} →
    τ ∉⇒ →
    ↑ τ ≡ τ
↑-∉⇒ {var _} _ = refl
↑-∉⇒ {τ₁ ⇒ τ₂} τ∉⇒ = ⊥-elim $ τ∉⇒ (τ₁ ∷ τ₂ ∷ [] , refl)
↑-∉⇒ {_ * _} _ = refl
↑-∉⇒ {cons (Σ.cons _) _} _ = refl

↑-≢⇒ : ∀ {f τ‥} →
    f ≢ Σ.⇒ →
    ↑ (cons f τ‥) ≡ cons f τ‥
↑-≢⇒ f≢⇒ = ↑-∉⇒ $ λ τ∈⇒ → f≢⇒ $ cons-injective $ proj₂ τ∈⇒

≅-↑ : ∀ {τ₁ τ₂} →
    τ₁ ≅ τ₂ →
    ↑ τ₁ ≅ ↑ τ₂
≅-↑ τ₁≅τ₂@(≅-ax ∼-*-assoc) = τ₁≅τ₂
≅-↑ τ₁≅τ₂@(≅-ax ∼-*-comm) = τ₁≅τ₂
≅-↑ (≅-ax ∼-curry) = ≅-refl
≅-↑ ≅-refl = ≅-refl
≅-↑ (≅-trans τ₁≅τ₂ τ₂≅τ₃) = ≅-trans (≅-↑ τ₁≅τ₂) (≅-↑ τ₂≅τ₃)
≅-↑ (≅-sym τ₁≅τ₂) = ≅-sym (≅-↑ τ₁≅τ₂)
≅-↑ (≅-cong {Σ.⇒} {i} {τ‥} τᵢ≅τᵢ′) with τ‥
... | τ₁′ ∷ τ₂′ ∷ [] with i
... | Fin.zero = ≅-refl
... | Fin.suc Fin.zero = ≅-↑ τᵢ≅τᵢ′
≅-↑ τ₁≅τ₂@(≅-cong {Σ.*} _) = τ₁≅τ₂
≅-↑ τ₁≅τ₂@(≅-cong {Σ.cons _} _) = τ₁≅τ₂

↑-σ : ∀ {τ α} →
    ↑ (σ α τ) ≡ ↑ (σ α (↑ τ))
↑-σ {var _} = refl
↑-σ {_ ⇒ τ} = ↑-σ {τ}
↑-σ {_ * _} = refl
↑-σ {cons (Σ.cons _) _} = refl

postulate
    ≅-cons : ∀ {f₁ f₂ τ‥₁ τ‥₂} →
        cons f₁ τ‥₁ ≅ cons f₂ τ‥₂ →
        f₁ ≡ f₂

-- ≅-cons : ∀ {f₁ f₂ τ‥₁ τ‥₂} →
--     cons f₁ τ‥₁ ≅ cons f₂ τ‥₂ →
--     f₁ ≡ f₂
-- ≅-cons {f₁} {f₂} {τ‥₁} {τ‥₂} τ₁≅τ₂ with cons f₁ τ‥₁ | cons f₂ τ‥₂
-- ... | τ₁ | τ₂ with τ₁≅τ₂ | f₁ | f₂
-- ... |(≅-ax ∼-*-assoc) | Σ.* | Σ.* = refl
-- ... | (≅-ax ∼-*-comm) | Σ.* | Σ.* = refl
-- ... | (≅-ax ∼-curry) | Σ.⇒ | Σ.⇒ = refl
-- ... | (≅-refl) |

-- ≅-cons : ∀ {τ₁ τ₂ f₁ f₂ τ‥₁ τ‥₂} →
--     {τ₁≡f₁ : τ₁ ≡ cons f₁ τ‥₁} →
--     {τ₂≡f₂ : τ₂ ≡ cons f₂ τ‥₂} →
--     τ₁ ≅ τ₂ →
--     f₁ ≡ f₂
-- ≅-cons {τ₁≡f₁ = τ₁≡f₁} {τ₂≡f₂ = τ₂≡f₂} τ₁≅τ₂
--     rewrite τ₁≡f₁ | τ₂≡f₂
--     with τ₁≅τ₂
-- ... | (≅-ax ∼-*-assoc) = ?

byhead : ∀ {τ₁ τ₂ f₁ f₂} →
    ∃[ τ‥ ] (↑ τ₁ ≡ cons f₁ τ‥) →
    ∃[ τ‥ ] (↑ τ₂ ≡ cons f₂ τ‥) →
    τ₁ ⋈ τ₂ →
    f₁ ≡ f₂
byhead {τ₁} {τ₂} {f₁} {f₂} (τ‥₁ , ↑τ₁≡f₁) (τ‥₂ , ↑τ₂≡f₂) (α , στ₁≅στ₂)
    with
        ≅-↑ στ₁≅στ₂
... | ↑στ₁≅↑στ₂
    rewrite
        ↑-σ {τ₁} {α} |
        ↑-σ {τ₂} {α} |
        ↑τ₁≡f₁ |
        ↑τ₂≡f₂ |
        ↑-≢⇒ {τ‥ = Vec.map (σ α) τ‥₁} $ ↑≢⇒ {τ₁} ↑τ₁≡f₁ |
        ↑-≢⇒ {τ‥ = Vec.map (σ α) τ‥₂} $ ↑≢⇒ {τ₂} ↑τ₂≡f₂ =
            ≅-cons ↑στ₁≅↑στ₂
