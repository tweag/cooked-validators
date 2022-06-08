module ListUtils where

open import Agda.Primitive
open import Data.List
open import Function

infix 3 [_
infixr 5 _,_
infix 6 _]

variable
  {a} : Level
  {A} : Set a
  
_] : A → List A
x ] = x ∷ []

[_ : List A → List A
[_ = id

_,_ : A → List A → List A
_,_ = _∷_

