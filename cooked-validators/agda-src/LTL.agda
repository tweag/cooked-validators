module LTL where

open import Function hiding (_⇔_)
open import Agda.Primitive
open import Data.Sum
open import Data.List hiding (lookup)
open import Data.Vec.Relation.Unary.All renaming (lookup to lookupₐ)
open import Data.Vec
open import Data.Nat
open import Data.Fin
open import Data.Product
open import Relation.Binary.PropositionalEquality
open import Relation.Binary hiding (_⇒_) renaming (Decidable to Decidable₂)
open import Relation.Nullary renaming (¬_ to not)
open import Relation.Unary using (Decidable)

-- Fixity and priority of LTL operators

infix 2 _⊢_
infixr 3 _⇒_
infix 4 X_
infix 5 _U_
infix 7 Lit_

-- The environment corresponds to the set of properties on the states
-- It is represented as a vector of a certain length n

Env : ∀ {a} (A : Set a) → ℕ → Set _
Env A n = Vec (A → Set) n

-- The inductive type of LTL formula

data LTL {a n} {A : Set a} (Γ : Env A n) : Set (lsuc a) where
  -- The false formula
  ⊥ : LTL Γ
  -- An encapsulated predicate
  Lit_ : Fin n → LTL Γ
  -- Implication
  _⇒_ : Fun₂ $ LTL Γ
  -- Next
  X_ : Fun₁ $ LTL Γ
  -- Until
  _U_ : Fun₂ $ LTL Γ

-- The satisfiability of LTL formulas by a finite trace, represented by a List
-- This requires positivity to be ignored since the "impSatLeft" constructor is
-- clearly negative. However, I have not found a single non-negative definition
-- of LTL satisfiability.

{-# NO_POSITIVITY_CHECK #-}
data _⊢_ {a n} {A : Set a} {Γ : Env A n} : List A → LTL Γ → Set (lsuc a) where
  litSat      : ∀ {x l i  } → lookup Γ i $ x        → x ∷ l ⊢ Lit i
  impSatLeft  : ∀ {x l ϕ ψ} → not $ x ∷ l ⊢ ϕ       → x ∷ l ⊢ ϕ ⇒ ψ
  impSatRight : ∀ {x l ϕ ψ} → x ∷ l ⊢ ψ             → x ∷ l ⊢ ϕ ⇒ ψ
  nexSat      : ∀ {x l ϕ  } → l ⊢ ϕ                 → x ∷ l ⊢ X ϕ
  untSatNow   : ∀ {x l ϕ ψ} → x ∷ l ⊢ ψ             → x ∷ l ⊢ ϕ U ψ
  untSatLater : ∀ {x l ϕ ψ} → x ∷ l ⊢ ϕ → l ⊢ ϕ U ψ → x ∷ l ⊢ ϕ U ψ

-- From now on, we consider a level, a type of that level, a certain length
-- and an environment of that length contains properties on that type.
-- These will be seen as implicit parameters by external modules.

module _ {a n} {A : Set a} {Γ : Env A n} where

  -- Additional fixity and priority

  infixr 3 _∨_ _∧_
  infix 4 F_ G_
  infix 6 ¬_

  -- Additional LTL operators deduced from the canonical ones

  -- Negation
  ¬_ : Fun₁ $ LTL Γ
  ¬ ϕ = ϕ ⇒ ⊥

  -- True
  ⊤ : LTL Γ
  ⊤ = ¬ ⊥

  -- Disjunction
  _∨_ : Fun₂ $ LTL Γ
  ϕ ∨ ψ = (¬ ϕ) ⇒ ψ

  -- Conjunction
  _∧_ : Fun₂ $ LTL Γ
  ϕ ∧ ψ = ¬ (¬ ϕ ∨ ¬ ψ)

  -- Eventually
  F_ : Fun₁ $ LTL Γ
  F ϕ = ⊤ U ϕ

  -- Always
  G_ : Fun₁ $ LTL Γ
  G ϕ = ¬ (F ¬ ϕ)


  -- Trivial properties to check for the soundness of our definitions:
  -- there are not trace satisfying false, the empty trace satisfies
  -- no formula, and all non-empty trace satisfy true.
  
  ⊥-unsat : ∀ {l} → not (_⊢_ {Γ = Γ} l ⊥)
  ⊥-unsat ()
  
  []-unsat : ∀ {ϕ : LTL Γ} → not ([] ⊢ ϕ)
  []-unsat ()
  
  ⊤-sat : ∀ {x l} → _⊢_ {Γ = Γ} (x ∷ l) ⊤
  ⊤-sat = impSatLeft ⊥-unsat
  
  -- Decidability of LTL satisfiability based on the decidability of the literals
  
  -- For now, we postulate termination because it has to be manually proven
  -- since the function structurally recurses on two arguments instead of ones
  -- which does not necessarily terminate in the global case.
  {-# TERMINATING #-}
  ⊢-decidable : All Decidable Γ → Decidable₂ (_⊢_ {Γ = Γ})
  ⊢-decidable _ [] _ = no []-unsat
  ⊢-decidable _ _ ⊥ = no ⊥-unsat
  ⊢-decidable aDec (x ∷ _) (Lit i) with lookupₐ i aDec x
  ... | yes p = yes (litSat p)
  ... | no ¬p = no λ {(litSat x) → ¬p x}
  ⊢-decidable aDec (x ∷ l) (ϕ ⇒ ψ) with ⊢-decidable aDec (x ∷ l) ϕ
  ... | no ¬p = yes (impSatLeft ¬p)
  ... | yes p with ⊢-decidable aDec (x ∷ l) ψ
  ... | yes q = yes (impSatRight q)
  ... | no ¬q = no λ { (impSatLeft u) → u p ; (impSatRight u) → ¬q u}
  ⊢-decidable aDec (x ∷ l) (X ϕ) with ⊢-decidable aDec l ϕ
  ... | yes p = yes (nexSat p)
  ... | no ¬p = no λ {(nexSat x₁) → ¬p x₁}
  ⊢-decidable aDec (x ∷ l) (ϕ U ψ) with ⊢-decidable aDec (x ∷ l) ψ
  ... | yes p = yes (untSatNow p)
  ... | no ¬p with ⊢-decidable aDec (x ∷ l) ϕ
  ... | no ¬q = no λ { (untSatNow u) → ¬p u ; (untSatLater u _) → ¬q u }
  ... | yes q with ⊢-decidable aDec l (ϕ U ψ)
  ... | yes r = yes (untSatLater q r)
  ... | no ¬r = no λ { (untSatNow u) → ¬p u ; (untSatLater _ v) → ¬r v}

