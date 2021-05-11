module _ where

open import Data.Bool as Bool
    using (Bool ; false ; true)

open import Data.Nat as ℕ
    using (ℕ)

open import Data.Product as Product
    using (_×_ ; _,_)

open import Data.String as String
    using (String)

open import Data.Vec as Vec
    using ([] ; _∷_ ; Vec ; _[_]≔_)
infix 20 _[_]
_[_] = Vec.lookup

open import Function
    using (_$_)

open import Relation.Binary.Definitions
    using (DecidableEquality)

open import Relation.Binary.PropositionalEquality as Eq
    using (_≡_ ; refl)

open import Relation.Nullary
    using (no ; yes)

open import Cons using (Cons)
open import Type using (Type)
open import Var as 𝓥 using (𝓥)

module Σ where

    data Σ : Set where
        * : Σ
        cons : Cons → Σ

    arity : Σ → ℕ
    arity * = 2
    arity (cons f) = Cons.arity f

    cons-injective : ∀ {c₁ c₂} →
        cons c₁ ≡ cons c₂ →
        c₁ ≡ c₂
    cons-injective refl = refl

    _≟_ : DecidableEquality Σ
    * ≟ * = yes refl
    * ≟ (cons _) = no $ λ ()
    (cons _) ≟ * = no $ λ ()
    (cons c₁) ≟ (cons c₂) with c₁ String.≟ c₂
    ... | yes refl = yes refl
    ... | no c₁≢c₂ = no $ λ refl → c₁≢c₂ $ cons-injective refl

module MultiSet {A : Set} (_≟_ : A → A → Bool) where

    MultiSet = A → ℕ

    infixl 15 _+_
    _+_ : MultiSet → MultiSet → MultiSet
    (m₁ + m₂) a = m₁ a ℕ.+ m₂ a

    infix 15 _+₁_
    _+₁_ : MultiSet → A → MultiSet
    (m +₁ a) a′ with a ≟ a′
    ... | true = ℕ.suc $ m a
    ... | false = m a′

    singleton : A → MultiSet
    singleton a a′ with a ≟ a′
    ... | true = 1
    ... | false = 0

    IsEmpty : MultiSet → Set
    IsEmpty m = ∀ a → m a ≡ 0

mutual

    infix 10 _⇒_
    {-# NO_POSITIVITY_CHECK #-}
    data NType : Set where
        var : 𝓥 → NType
        cons : ∀ f → Vec NType (Σ.arity f) → NType
        _⇒_ : MSet → NType → NType

    postulate _≟_ : NType → NType → Bool

    module MSet = MultiSet _≟_
    MSet = MSet.MultiSet

mutual

    infix 5 _≅_
    data _≅_ : NType → NType → Set where
        ≅-refl : ∀ {ν} →
            ν ≅ ν
        ≅-trans : ∀ {ν₁ ν₂ ν₃} →
            ν₁ ≅ ν₂ →
            ν₂ ≅ ν₃ →
            ν₁ ≅ ν₃
        ≅-sym : ∀ {ν₁ ν₂} →
            ν₁ ≅ ν₂ →
            ν₂ ≅ ν₁
        ≅-cong : ∀ {f i ν‥ νᵢ′} →
            ν‥ [ i ] ≅ νᵢ′ →
            cons f ν‥ ≅ cons f (ν‥ [ i ]≔ νᵢ′)
        ≅-⇒ : ∀ {m₁ m₂ ν₁ ν₂} →
            m₁ ≅ᵐ m₂ →
            ν₁ ≅ ν₂ →
            m₁ ⇒ ν₁ ≅ m₂ ⇒ ν₂

    infix 5 _≅ᵐ_
    data _≅ᵐ_ : MSet → MSet → Set where
        ≅ᵐ-empty : ∀ {m₁ m₂} →
            MSet.IsEmpty m₁ →
            MSet.IsEmpty m₂ →
            m₁ ≅ᵐ m₂
        ≅ᵐ-+₁ : ∀ {m₁ m₂ ν₁ ν₂} →
            m₁ ≅ᵐ m₂ →
            ν₁ ≅ ν₂ →
            m₁ MSet.+₁ ν₁ ≅ᵐ m₂ MSet.+₁ ν₂

mutual

    {-# TERMINATING #-}
    normalize : Type → NType
    normalize (Type.var v) = var v
    normalize (τ₁ Type.⇒ τ₂) = normalize-mset (normalize-* τ₁) (normalize τ₂)
    normalize (τ₁ Type.* τ₂) = cons Σ.* (normalize τ₁ ∷ normalize τ₂ ∷ [])
    normalize (Type.cons (Type.Σ.cons c) τ‥) = cons (Σ.cons c) (Vec.map normalize τ‥)

    normalize-* : Type → MSet
    normalize-* (τ₁ Type.* τ₂) = normalize-* τ₁ MSet.+ normalize-* τ₂
    normalize-* τ = MSet.singleton $ normalize τ

    normalize-mset : MSet → NType → NType
    normalize-mset m (m′ ⇒ ν) = m MSet.+ m′ ⇒ ν
    normalize-mset m ν = m ⇒ ν
