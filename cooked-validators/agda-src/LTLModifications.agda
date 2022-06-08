module LTLModifications where

open import LTL
open import Modifications
open import Data.Nat
open import Data.Nat.Properties
open import Data.Vec
open import Data.Fin using (#_)
open import Relation.Nullary.Decidable.Core
open import Relation.Binary.PropositionalEquality

-- This builds an environment made of predicates stating if
-- the state has been modified by the corresponding modification
-- There is a lot of noise in this function because of ordering of
-- elements and proof requirements, but basically if Γ is the resulting
-- environment then Γ (i) checks if the state S has be marked as
-- modified by i.
ModEnv : ∀ {a} {A : Set a} → (n : ℕ) → Env (State A n) n
ModEnv {A = A} n = aux {n} {0} {n} refl [] where
  -- This auxiliary function is required for two reasons:
  -- 1) we want the list to be reversed so we use a buffer
  -- 2) n is used twice, it is fixed once and it decreases once so it has to be split into two integers
  aux : ∀ {fixed done todo : ℕ}
    -- this is the invariant of the function
    → done + todo ≡ fixed
    -- this is the buffer
    → Env (State A fixed) done
    → Env (State A fixed) fixed
  aux {done = done} {zero} p v rewrite (sym p) | +-identityʳ done =
    -- returning the buffer when todo is 0
    v
  aux {done = done} {suc n} refl v =
    -- calling aux when more elements need to be processed
    aux {todo = n}
      -- computing the invariant
      (sym (+-suc done n))
      -- appending the new predicate to the buffer
      ((λ x → applied x (#_ n {m<n = fromWitness (m≤n+m _ _)})) ∷ v)
