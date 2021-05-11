open import Data.Product as Product
    using (_,_)

open import Data.Nat as ℕ
    using (_+_ ; _≤_ ; ℕ)

open import Relation.Binary.PropositionalEquality as Eq
    using (_≡_)

open import Relation.Nullary
    using (¬_ ; no ; yes)

open import Type

μ′ : Σ → Type → ℕ
μ′ _ (var _) = 0
μ′ f (τ₁ * τ₂) = μ′ f τ₁ + μ′ f τ₂
μ′ f (cons f′ _) with f Σ.≟ f′
... | no _ = 0
... | yes _ = 1

μ : Σ → Type → ℕ
μ f (τ₁ ⇒ τ₂) = μ′ f τ₁ + μ f τ₂
μ _ _ = 0

μᵥ′ : Type → ℕ
μᵥ′ (var _) = 0
μᵥ′ (τ₁ * τ₂) = μᵥ′ τ₁ + μᵥ′ τ₂
μᵥ′ _ = 0

μᵥ : Type → ℕ
μᵥ (τ₁ ⇒ τ₂) = μᵥ′ τ₁ + μᵥ τ₂
μᵥ _ = 0

Simple : Type → Set
Simple τ = μᵥ τ ≡ 0

postulate
    ≅-⇒-≡μ : ∀ {τ₁ τ₂ f} →
        τ₁ ≅ τ₂ →
        μ f τ₁ ≡ μ f τ₂

postulate
    simple-⇒-μσ≡μ : ∀ {τ f α} →
        Simple τ →
        μ f (σ α τ) ≡ μ f τ

postulate
    μ-≤-μσ : ∀ {τ f α} →
        μ f τ ≤ μ f (σ α τ)

bytail : ∀ {τ₁ τ₂ f} →
    Simple τ₁ →
    τ₁ ⋈ τ₂ →
    μ f τ₂ ≤ μ f τ₁
bytail {τ₁} {τ₂} {f} simple-τ₁ (α , στ₁≅στ₂)
    with
        ≅-⇒-≡μ {σ α τ₁} {σ α τ₂} {f} στ₁≅στ₂
... | μστ₁≡μστ₂
    rewrite
        simple-⇒-μσ≡μ {τ₁} {f} {α} simple-τ₁
    with
        μ-≤-μσ {τ₂} {f} {α}
... | μτ₂≤μστ₂
    rewrite
        Eq.sym μστ₁≡μστ₂ =
            μτ₂≤μστ₂
