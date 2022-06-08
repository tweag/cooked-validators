module Even where

open import Data.Nat
open import Data.Empty
open import Relation.Nullary renaming (¬_ to not)
open import Function
open import Relation.Unary

data Even : ℕ → Set where
  ez : Even 0
  es : ∀ {n} → Even n → Even (suc (suc n))

Odd : ℕ → Set
Odd = not ∘ Even

decEven : Decidable Even
decEven zero = yes ez
decEven (suc zero) = no λ ()
decEven (suc (suc n)) with decEven n
... | yes p = yes (es p)
... | no ¬p = no λ { (es x) → ¬p x }

even→sucodd : ∀ {n} → Even n → Odd (suc n)
even→sucodd ez = λ ()
even→sucodd (es p) = λ { (es x) → even→sucodd p x}

odd→suceven : ∀ {n} → Odd n → Even (suc n)
odd→suceven {zero} p = ⊥-elim (p ez)
odd→suceven {suc zero} p = es ez
odd→suceven {suc (suc _)} p = es (odd→suceven (p ∘ es))

decOdd : Decidable Odd
decOdd zero = no (λ z → z ez)
decOdd (suc n) with decEven n
... | yes p = yes (even→sucodd p)
... | no ¬p = no λ x → x (odd→suceven ¬p)

os : ∀ {n} → Odd n → Odd (suc (suc n))
os = even→sucodd ∘ odd→suceven
