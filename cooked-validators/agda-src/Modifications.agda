module Modifications where

open import LTL
open import Data.Maybe
open import Data.Vec
open import Data.Nat
open import Data.List hiding (lookup) renaming (map to mapₗ)
open import Data.Fin
open import Data.Product
open import Function
open import Data.List.Membership.Propositional

-- A modifier is a function that might change an element of type A
Modifier : ∀ {a} → Set a → Set a
Modifier A = A → Maybe A

-- Modifiers are vectors of modifiers
Modifiers : ∀ {a} → Set a → ℕ → Set a
Modifiers A n = Vec (Modifier A) n

-- A state is an element of type A coupled with its list of modifications
State : ∀ {a} → Set a → ℕ → Set a
State A n = A × List (Fin n)

-- Applying a modification from a modifier i consists in trying to modify the element
-- in the state and adding i to the list of modification if it was successful and
-- doing nothing otherwise
apply : ∀ {n a} {A : Set a} → State A n → Modifiers A n → Fin n → State A n
apply s@(a , l) ms i = maybe (_,(i ∷ l)) s (lookup ms i a)

-- A state has been modified by a modifier i when it appears in its modification list
applied : ∀ {n a} {A : Set a} → State A n → Fin n → Set
applied (_ , l) i = i ∈ l

-- A StateTrace is just a list of states
StateTrace : ∀ {a} (A : Set a) → ℕ → Set a
StateTrace = List ∘₂ State

-- We can create a StateTrace from an integer and a list of elements
buildTrace : ∀ {a} {A : Set a} → (n : ℕ) → List A → StateTrace A n
buildTrace _ = mapₗ (_, [])

