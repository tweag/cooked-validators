module Cooked.MockChain.Time where

import qualified Cooked.PlutusDeps as Pl

msecDiffToSlotCount :: Pl.POSIXTime -> Pl.SlotConfig -> Integer
msecDiffToSlotCount msec sc = Pl.getPOSIXTime msec `div` Pl.scSlotLength sc
