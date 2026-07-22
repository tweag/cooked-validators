-- | Centralized module with automated attacks:
--
-- - Add extraneous tokens to transactions
--
-- - Hijack outputs and redirecting them to some address
--
-- - Duplicate minted tokens
--
-- - Tamper datum or redeemers
--
-- These attacks usually rely on applying specific tweaks from `Cooked.Tweak`
-- at specific position in traces using Ltl formulae using `Cooked.Ltl`
module Cooked.Attack (module X) where

import Cooked.Attack.DatumHijacking as X
import Cooked.Attack.TamperDatum as X
import Cooked.Attack.TamperRedeemer as X
import Cooked.Attack.TokenDuplication as X
