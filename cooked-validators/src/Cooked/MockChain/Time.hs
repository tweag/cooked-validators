module Cooked.MockChain.Time where

import Data.Default
import qualified Ledger.TimeSlot as Pl
import qualified Plutus.V1.Ledger.Slot as Pl
import qualified Plutus.V1.Ledger.Time as Pl

data SlotCounter = SlotCounter
  { autoIncrease :: Bool,
    currentSlot :: Integer,
    slotConfig :: Pl.SlotConfig
  }
  deriving (Eq, Show)

scIncrease :: SlotCounter -> SlotCounter
scIncrease sc =
  let auto = autoIncrease sc
   in sc {currentSlot = (if auto then (+ 1) else id) $ currentSlot sc}

scFreezeTime :: SlotCounter -> SlotCounter
scFreezeTime sc =
  sc {autoIncrease = False}

scWaitSlots :: Integer -> SlotCounter -> SlotCounter
scWaitSlots n sc =
  sc {currentSlot = currentSlot sc + n}

scWait :: Pl.POSIXTime -> SlotCounter -> SlotCounter
scWait mSec sc =
  let slotLength = Pl.scSlotLength $ slotConfig sc
   in scWaitSlots (Pl.getPOSIXTime mSec `div` slotLength) sc

scSlotIs :: Integer -> SlotCounter -> SlotCounter
scSlotIs n sc =
  if n < currentSlot sc
    then -- Time travel is a dangerous feature (Back to the Future should convince you about it),
    -- hence we do not allow it in the current version.
      error "One cannot set time to a previous instant."
    else sc {autoIncrease = False, currentSlot = n}

scTimeIs :: Pl.POSIXTime -> SlotCounter -> SlotCounter
scTimeIs t sc =
  let s = Pl.posixTimeToEnclosingSlot (slotConfig sc) t
   in scSlotIs (Pl.getSlot s) sc

slotCounter0 :: SlotCounter
slotCounter0 =
  SlotCounter True 0 def
