module LTLExamples where

open import LTL
open import Even
open import ListUtils
open import Data.List hiding ([_])
open import Data.Vec hiding ([_])
open import Data.Vec.Relation.Unary.All
open import Data.Nat
open import Relation.Unary hiding (_⊢_)
open import Data.Fin
open import Function

Γ : Env ℕ 2
Γ = fromList ([ Even , Odd ])

ΓDec : All Decidable Γ
ΓDec = decEven ∷ decOdd ∷ []

Trace = List ℕ

-----------------------
-- Some LTL formulas --
-----------------------

-- Current elements is even
firstEven : LTL Γ
firstEven = Lit # 0

-- All elements are even
allEven = G firstEven

-- All elements are odd
allOdd : LTL Γ
allOdd = G (Lit # 1)

------------------------------------------------------
-- Some proofs that certain numbers are even or odd -- 
------------------------------------------------------

e2 = es ez
e4 = es e2
e8 = es $ es e4
e10 = es e8

o3 : Odd 3
o3 = λ {(es ())}
o5 = os o3
o7 = os o5
o9 = os o7

------------------------------------------
-- Example n°1:                         --
-- A trace containing only even numbers --
------------------------------------------
trace₁ : Trace
trace₁ = [ 4 , 2 , 4 , 8 , 10 ]

e₁⊢lit : trace₁ ⊢ firstEven
e₁⊢lit = litSat e4

e₁⊢always : trace₁ ⊢ allEven
e₁⊢always = impSatLeft λ {
  (untSatNow (impSatLeft x)) → x $ litSat e4 ;
  (untSatLater _ (untSatNow (impSatLeft x))) → x $ litSat e2 ;
  (untSatLater _ (untSatLater _ (untSatNow (impSatLeft x)))) → x $ litSat e4 ;
  (untSatLater _ (untSatLater _ (untSatLater _ (untSatNow (impSatLeft x))))) → x $ litSat e8 ;
  (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatNow (impSatLeft x)))))) → x $ litSat e10}

---------------------------------------------
-- Example n°2:                            --
-- A trace containing even and odd numbers --
---------------------------------------------

trace₂ : Trace
trace₂ = [ 2 , 4 , 3 , 4 , 6 , 5 , 7 , 9 ]
