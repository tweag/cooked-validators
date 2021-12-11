module Cooked.MockChain.Time where

import qualified Ledger.TimeSlot as Pl
import qualified Plutus.V1.Ledger.Time as Pl

msecDiffToSlotCount :: Pl.POSIXTime -> Pl.SlotConfig -> Integer
msecDiffToSlotCount msec sc = Pl.getPOSIXTime msec `div` Pl.scSlotLength sc
