-- | Centralized module with automated attacks:
--
-- - Add extraneous tokens to transactions
--
-- - Hijack outputs be redirecting them to some address
--
-- - Perform double satisfaction on outputs
--
-- - Duplicate minted tokens
--
-- These attacks usually rely on applying specific tweaks from `Cooked.Tweak`
-- at specific position in traces using Ltl formulae using `Cooked.Ltl`
module Cooked.Attack (module X) where

import Cooked.Attack.AddToken as X
import Cooked.Attack.DatumHijacking as X
import Cooked.Attack.DoubleSat as X
