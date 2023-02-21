{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cooked.Time where

import Cardano.Node.Emulator.TimeSlot qualified as Emulator
import Ledger.Slot qualified as Ledger
import Plutus.V2.Ledger.Api qualified as PV2

class TimeUnit a where
  getSlot :: a -> Emulator.SlotConfig -> Ledger.Slot
  getFirstTimeUnit :: Emulator.SlotConfig -> Ledger.Slot -> a
  getLastTimeUnit :: Emulator.SlotConfig -> Ledger.Slot -> a
  appendUnits :: a -> Integer -> a

instance TimeUnit PV2.POSIXTime where
  getSlot = flip Emulator.posixTimeToEnclosingSlot
  getFirstTimeUnit = Emulator.slotToBeginPOSIXTime
  getLastTimeUnit = Emulator.slotToEndPOSIXTime
  appendUnits (PV2.POSIXTime t) = PV2.POSIXTime . (t +)

instance TimeUnit Ledger.Slot where
  getSlot a _ = a
  getFirstTimeUnit _ a = a
  getLastTimeUnit _ a = a
  appendUnits (Ledger.Slot t) = Ledger.Slot . (t +)

class TimeTranslate a b where
  leftApproximate :: PV2.Interval a -> PV2.Interval b
  rightApproximate :: PV2.Interval b -> PV2.Interval a
