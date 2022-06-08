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

-- Current element is Odd
firstOdd : LTL Γ
firstOdd = Lit # 1

-- All elements are odd
allOdd : LTL Γ
allOdd = G firstOdd

-- Eventually there is an odd number
evOdd : LTL Γ
evOdd = F firstOdd

-- Eventually, we have an even numbers followed by endless odd numbers
evEvenUntilOdd : LTL Γ
evEvenUntilOdd = F (_U_ firstEven allOdd)

------------------------------------------------------
-- Some proofs that certain numbers are even or odd -- 
------------------------------------------------------

e2 = es ez
e4 = es e2
e6 = es e4
e8 = es e6
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

-- trace₂ contains an odd number at some point
e₂⊢evOdd : trace₂ ⊢ evOdd
e₂⊢evOdd = untSatLater ⊤-sat (untSatLater ⊤-sat (untSatNow (litSat o3)))

-- trace₂ contains an even number followed by only odd numbers at some point
e₂⊢ev : trace₂ ⊢ evEvenUntilOdd
e₂⊢ev = untSatLater ⊤-sat (untSatLater ⊤-sat (untSatLater ⊤-sat
  (untSatLater ⊤-sat (untSatNow (untSatLater (litSat e6) (untSatNow
  (impSatLeft λ {
    (untSatNow (impSatLeft x)) → x (litSat o5) ;
    (untSatLater _ (untSatNow (impSatLeft x))) → x (litSat o7) ;
    (untSatLater _ (untSatLater _ (untSatNow (impSatLeft x)))) → x (litSat o9)})))))))

-- trace₂ never contains an endless stream of even numbers
¬e₂⊢allEven : trace₂ ⊢ ¬ (F allEven)
¬e₂⊢allEven = impSatLeft λ {
  (untSatNow (impSatLeft x)) →
    x (untSatLater ⊤-sat (untSatLater ⊤-sat (untSatNow (impSatLeft λ {(litSat x) → o3 x})))) ;
  (untSatLater _ (untSatNow (impSatLeft x))) →
    x (untSatLater ⊤-sat (untSatNow (impSatLeft λ {(litSat x) → o3 x}))) ;
  (untSatLater _ (untSatLater _ (untSatNow (impSatLeft x)))) →
    x (untSatNow (impSatLeft λ {(litSat x) → o3 x})) ;
  (untSatLater _ (untSatLater _ (untSatLater _ (untSatNow (impSatLeft x))))) →
    x (untSatLater ⊤-sat (untSatLater ⊤-sat (untSatNow (impSatLeft λ {(litSat x) → o5 x})))) ;
  (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatNow (impSatLeft x)))))) →
    x (untSatLater ⊤-sat (untSatNow (impSatLeft λ {(litSat x) → o5 x}))) ;
  (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatNow (impSatLeft x))))))) →
    x (untSatNow (impSatLeft λ {(litSat x) → o5 x})) ;
  (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatNow (impSatLeft x)))))))) →
    x (untSatNow (impSatLeft λ {(litSat x) → o7 x})) ;
  (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatLater _ (untSatNow (impSatLeft x))))))))) →
    x (untSatNow (impSatLeft λ {(litSat x) → o9 x}))}
