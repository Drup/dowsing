open import Data.Nat as ℕ
    using (ℕ)

open import Data.String as String
    using (String)

Cons = String

postulate arity : Cons → ℕ

_≟_ = String._≟_
