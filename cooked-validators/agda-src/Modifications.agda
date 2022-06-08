module Modifications where

open import LTL
open import Data.Maybe

Modifier : ∀ {a} → Set a → Set a
Modifier A = A → Maybe A

