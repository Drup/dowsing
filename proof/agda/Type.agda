open import Data.Nat as ℕ
    using (ℕ)

open import Data.Product as Product
    using (_×_ ; _,_ ; Σ-syntax ; ∃-syntax)

open import Data.Vec as Vec
    using ([] ; _∷_ ; Vec ; _[_]≔_)
infix 20 _[_]
_[_] = Vec.lookup

import Data.Vec.Properties as VecProperties

open import Function
    using (_$_ ; _∘_)

open import Relation.Binary.Definitions
    using (DecidableEquality)

open import Relation.Binary.PropositionalEquality as Eq
    using (_≡_ ; refl)

open import Relation.Nullary
    using (¬_ ; no ; yes)

open import Cons using (Cons)
open import Var as 𝓥 using (𝓥)

module Σ where

    data Σ : Set where
        * : Σ
        ⇒ : Σ
        cons : Cons → Σ

    arity : Σ → ℕ
    arity * = 2
    arity ⇒ = 2
    arity (cons c) = Cons.arity c

    cons-injective : ∀ {c₁ c₂} →
        cons c₁ ≡ cons c₂ →
        c₁ ≡ c₂
    cons-injective refl = refl

    _≟_ : DecidableEquality Σ
    * ≟ * = yes refl
    * ≟ ⇒ = no $ λ ()
    * ≟ (cons _) = no $ λ ()
    ⇒ ≟ * = no $ λ ()
    ⇒ ≟ ⇒ = yes refl
    ⇒ ≟ (cons _) = no $ λ ()
    (cons _) ≟ * = no $ λ ()
    (cons _) ≟ ⇒ = no $ λ ()
    (cons c₁) ≟ (cons c₂) with c₁ Cons.≟ c₂
    ... | yes refl = yes refl
    ... | no c₁≢c₂ = no $ λ refl → c₁≢c₂ $ cons-injective refl

Σ = Σ.Σ

-- types

data Type : Set where
    var : 𝓥 → Type
    cons : ∀ f → Vec Type (Σ.arity f) → Type

infixr 10 _⇒_
infixr 15 _*_
pattern _*_ τ₁ τ₂ = cons Σ.* (τ₁ ∷ τ₂ ∷ [])
pattern _⇒_ τ₁ τ₂ = cons Σ.⇒ (τ₁ ∷ τ₂ ∷ [])

infix 20 _∈𝓥
_∈𝓥 : Type → Set
τ ∈𝓥 = ∃[ v ] (τ ≡ var v)

infix 20 _∉𝓥
_∉𝓥 = ¬_ ∘ _∈𝓥

infix 20 _∈⇒
_∈⇒ : Type → Set
τ ∈⇒ = ∃[ τ‥ ] (τ ≡ cons Σ.⇒ τ‥)

infix 20 _∉⇒
_∉⇒ = ¬_ ∘ _∈⇒

var-injective : ∀ {v₁ v₂} →
    var v₁ ≡ var v₂ →
    v₁ ≡ v₂
var-injective refl = refl

cons-injective : ∀ {f₁ f₂ τ‥₁ τ‥₂} →
    cons f₁ τ‥₁ ≡ cons f₂ τ‥₂ →
    f₁ ≡ f₂
cons-injective refl = refl

-- substitution

Assignment = 𝓥 → Type

{-# TERMINATING #-}
σ : Assignment → Type → Type
σ α (var v) = α v
σ α (cons f τ‥) = cons f $ Vec.map (σ α) τ‥

-- equational axioms

infix 5 _∼_
data _∼_ : Type → Type → Set where
    ∼-*-assoc :
        var "x" * (var "y" * var "z") ∼
        (var "x" * var "y") * var "z"
    ∼-*-comm :
        var "x" * var "y" ∼
        var "y" * var "x"
    ∼-curry :
        var "x" * var "y" ⇒ var "z" ∼
        var "x" ⇒ var "y" ⇒ var "z"

-- equational theory

infix 5 _≅_
data _≅_ : Type → Type → Set where
    -- ≅-ax : ∀ {τ₁ τ₂} →
    --     τ₁ ∼ τ₂ →
    --     τ₁ ≅ τ₂
    -- ≅-σ : ∀ {τ₁ τ₂ τ₁′ τ₂′ α} →
    --     τ₁′ ≅ τ₂′ →
    --     τ₁ ≡ σ α τ₁′ →
    --     τ₂ ≡ σ α τ₂′ →
    --     τ₁ ≅ τ₂
    ≅-ax : ∀ {α τ₁ τ₂} →
        τ₁ ∼ τ₂ →
        σ α τ₁ ≅ σ α τ₂
    ≅-refl : ∀ {τ} →
        τ ≅ τ
    ≅-trans : ∀ {τ₁ τ₂ τ₃} →
        τ₁ ≅ τ₂ →
        τ₂ ≅ τ₃ →
        τ₁ ≅ τ₃
    ≅-sym : ∀ {τ₁ τ₂} →
        τ₁ ≅ τ₂ →
        τ₂ ≅ τ₁
    ≅-cong : ∀ {f i τ‥ τᵢ′} →
        τ‥ [ i ] ≅ τᵢ′ →
        cons f τ‥ ≅ cons f (τ‥ [ i ]≔ τᵢ′)

≅-σ : ∀ {τ₁ τ₂ α} →
    τ₁ ≅ τ₂ →
    σ α τ₁ ≅ σ α τ₂
≅-σ {α = α} (≅-ax {α = α′} ∼-*-assoc) = ≅-ax {α = σ α ∘ α′} ∼-*-assoc
≅-σ {α = α} (≅-ax {α = α′} ∼-*-comm) = ≅-ax {α = σ α ∘ α′} ∼-*-comm
≅-σ {α = α} (≅-ax {α = α′} ∼-curry) = ≅-ax {α = σ α ∘ α′} ∼-curry
≅-σ ≅-refl = ≅-refl
≅-σ (≅-trans τ₁≅τ₂ τ₂≅τ₃) = ≅-trans (≅-σ τ₁≅τ₂) (≅-σ τ₂≅τ₃)
≅-σ (≅-sym τ₁≅τ₂) = ≅-sym (≅-σ τ₁≅τ₂)
≅-σ {α = α} (≅-cong {f} {i} {τ‥} {τᵢ′} τᵢ≅τᵢ′)
    rewrite
        VecProperties.map-[]≔ (σ α) τ‥ i {τᵢ′}
    with
        ≅-σ {α = α} τᵢ≅τᵢ′
... | στᵢ≅στᵢ′
    rewrite
        Eq.sym $ VecProperties.lookup-map i (σ α) τ‥ =
            ≅-cong στᵢ≅στᵢ′

-- unifiability

_⋈_ : Type → Type → Set
τ₁ ⋈ τ₂ = ∃[ α ] (σ α τ₁ ≅ σ α τ₂)
