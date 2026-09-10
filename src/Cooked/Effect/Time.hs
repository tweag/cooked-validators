-- | This module exposes the user-facing primitives to query and manipulate the
-- current time of the blockchain, expressed in terms of slots and POSIX time.
-- It regroups the time-related read primitives (the current slot, the
-- conversions between slots and POSIX time ranges) as well as the primitives to
-- wait for a given slot or time. The lower-level configuration primitives (era
-- history and system start) live in the internal
-- 'Cooked.Effect.Params.Params' effect, which the node
-- interpreter of this effect relies on.
module Cooked.Effect.Time
  ( -- * The 'Time' effect
    Time,

    -- * 'Time' interpreters
    runMockChainTime,
    runBlockChainTime,

    -- * Queries related to the current time
    currentSlot,
    currentMSRange,
    getEnclosingSlot,
    slotRangeBefore,
    slotRangeAfter,
    slotToMSRange,

    -- * Modifications of the current time
    waitNSlots,
    awaitSlot,
    awaitEnclosingSlot,
    waitNMSFromSlotLowerBound,
    waitNMSFromSlotUpperBound,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Cardano.Slotting.Time qualified as Time
import Control.Concurrent (threadDelay)
import Control.Lens qualified as Lens
import Control.Monad
import Cooked.Effect.Params
import Cooked.Runtime.State
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Ledger.Slot qualified as P.Ledger
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Reader
import Polysemy.State

-- | An effect that offers primitives to query, convert, and wait on the current
-- time of the mockchain. The read-only primitives ('currentSlot',
-- 'slotToMSRange', 'getEnclosingSlot') do not alter the state, while the waiting
-- primitive ('waitNSlots') advances the current slot.
data Time :: Effect where
  CurrentSlot :: Time m P.Ledger.Slot
  SlotToMSRange :: P.Ledger.Slot -> Time m (Api.POSIXTime, Api.POSIXTime)
  GetEnclosingSlot :: Api.POSIXTime -> Time m P.Ledger.Slot
  WaitNSlots :: Integer -> Time m P.Ledger.Slot

makeSem_ ''Time

-- | Returns the current slot
currentSlot ::
  (Member Time effs) =>
  Sem effs P.Ledger.Slot

-- | Returns the closed ms interval corresponding to the slot with the given
-- number.
slotToMSRange ::
  (Members '[Time, Fail] effs) =>
  P.Ledger.Slot ->
  Sem effs (Api.POSIXTime, Api.POSIXTime)

-- | Returns the closed ms interval corresponding to the current slot
currentMSRange ::
  (Members '[Time, Fail] effs) =>
  Sem effs (Api.POSIXTime, Api.POSIXTime)
currentMSRange = slotToMSRange =<< currentSlot

-- | Return the slot that contains the given time. See 'slotToMSRange' for
-- some satisfied equational properties.
getEnclosingSlot ::
  (Member Time effs) =>
  Api.POSIXTime ->
  Sem effs P.Ledger.Slot

-- | The infinite range of slots ending before or at the given time
slotRangeBefore ::
  (Members '[Time, Fail] effs) =>
  Api.POSIXTime ->
  Sem effs P.Ledger.SlotRange
slotRangeBefore t = do
  n <- getEnclosingSlot t
  (_, b) <- slotToMSRange n
  -- If the given time @t@ happens to be the last ms of its slot, we can include
  -- the whole slot. Otherwise, the only way to be sure that the returned slot
  -- range contains no time after @t@ is to go to the preceding slot.
  return $ Api.to $ if t == b then n else n - 1

-- | The infinite range of slots starting after or at the given time
slotRangeAfter ::
  (Members '[Time, Fail] effs) =>
  Api.POSIXTime ->
  Sem effs P.Ledger.SlotRange
slotRangeAfter t = do
  n <- getEnclosingSlot t
  (a, _) <- slotToMSRange n
  return $ Api.from $ if t == a then n else n + 1

-- | Waits a certain number of slots and returns the new slot
waitNSlots ::
  (Member Time effs) =>
  Integer ->
  Sem effs P.Ledger.Slot

-- | Wait for a certain slot, or throws an error if the slot is already past
awaitSlot :: (Member Time effs) => P.Ledger.Slot -> Sem effs P.Ledger.Slot
awaitSlot (P.Ledger.Slot targetSlot) = do
  P.Ledger.Slot now <- currentSlot
  waitNSlots (targetSlot - now)

-- | Waits until the current slot becomes greater or equal to the slot
--  containing the given POSIX time.  Note that that it might not wait for
--  anything if the current slot is large enough.
awaitEnclosingSlot :: (Member Time effs) => Api.POSIXTime -> Sem effs P.Ledger.Slot
awaitEnclosingSlot time = getEnclosingSlot time >>= awaitSlot

-- | Wait a given number of ms from the lower bound of the current slot and
-- returns the current slot after waiting.
waitNMSFromSlotLowerBound :: (Members '[Time, Fail] effs) => Integer -> Sem effs P.Ledger.Slot
waitNMSFromSlotLowerBound duration = currentMSRange >>= awaitEnclosingSlot . (+ fromIntegral duration) . fst

-- | Wait a given number of ms from the upper bound of the current slot and
-- returns the current slot after waiting.
waitNMSFromSlotUpperBound :: (Members '[Time, Fail] effs) => Integer -> Sem effs P.Ledger.Slot
waitNMSFromSlotUpperBound duration = currentMSRange >>= awaitEnclosingSlot . (+ fromIntegral duration) . snd

-- | The interpretation for the time effect with a stored 'EmulatorState'
runMockChainTime ::
  forall effs a.
  ( Members
      '[ State EmulatorState,
         Fail
       ]
      effs
  ) =>
  Sem (Time : effs) a ->
  Sem effs a
runMockChainTime = interpret $ \case
  CurrentSlot -> gets $ view $ emulatorStateLedgerStateL % to Emulator.getSlot
  SlotToMSRange slot -> do
    slotConfig <- gets $ Emulator.pSlotConfig . emulatorStateParams
    case Emulator.slotToPOSIXTimeRange slotConfig slot of
      Api.Interval
        (Api.LowerBound (Api.Finite l) leftclosed)
        (Api.UpperBound (Api.Finite r) rightclosed) ->
          return
            ( if leftclosed then l else l + 1,
              if rightclosed then r else r - 1
            )
      _ -> fail "Unexpected unbounded slot: please report a bug at https://github.com/tweag/cooked-validators/issues"
  GetEnclosingSlot t -> gets $ (`Emulator.posixTimeToEnclosingSlot` t) . Emulator.pSlotConfig . emulatorStateParams
  WaitNSlots n -> do
    cs <- gets $ Emulator.getSlot . emulatorStateLedgerState
    -- Waiting for a non-positive number of slots does not change the current
    -- slot, and we simply return it unchanged.
    if n <= 0
      then return cs
      else do
        let newSlot = cs + fromIntegral n
        modify' $ over emulatorStateLedgerStateL $ Lens.set Emulator.elsSlotL $ fromIntegral newSlot
        return newSlot

-- | Interpret the `Time` effect by talking to a deployed node through a
-- `Cardano.LocalNodeConnectInfo` (socket path and network id) provided via a
-- `Reader`, running in a stack featuring @IO@ (via `Embed`). Waiting is
-- performed by suspending the thread for the appropriate amount of time. The
-- fixed chain configuration is resolved through the internal
-- 'Cooked.Effect.Params.Params' effect.
runBlockChainTime ::
  forall effs a.
  ( Members
      '[ Embed IO,
         Params,
         Error Cardano.PastHorizonException,
         Reader Cardano.LocalNodeConnectInfo
       ]
      effs
  ) =>
  Sem (Time : effs) a ->
  Sem effs a
runBlockChainTime = interpret $ \case
  CurrentSlot -> getCurrentSlot
  SlotToMSRange slot -> do
    eraHistory <- getEraHistory
    systemStart <- getSystemStart
    (relStart, slotLen) <- fromEither $ Cardano.getProgress (toSlotNo slot) eraHistory
    let startUTC = Time.fromRelativeTime systemStart relStart
        endUTC = Time.getSlotLength slotLen `addUTCTime` startUTC
    return (utcToPOSIXTime startUTC, utcToPOSIXTime endUTC - 1)
  GetEnclosingSlot ms -> msToSlot ms
  WaitNSlots n -> do
    when (n > 0) $ do
      nowSlot <- getCurrentSlot
      eraHistory <- getEraHistory
      slotLength <- Time.slotLengthToMillisec . snd <$> fromEither (Cardano.getProgress (toSlotNo nowSlot) eraHistory)
      embed $ threadDelay $ fromIntegral $ n * slotLength * 1000
    getCurrentSlot
  where
    -- Returns the current time according to the thread clock
    getCurrentSlot :: Sem effs P.Ledger.Slot
    getCurrentSlot = embed getCurrentTime >>= msToSlot . utcToPOSIXTime
    -- Gives the slot in which a certain POSIX time belong
    msToSlot :: Api.POSIXTime -> Sem effs P.Ledger.Slot
    msToSlot t = do
      eraHistory <- getEraHistory
      systemStart <- getSystemStart
      let relTime = Time.toRelativeTime systemStart $ posixTimeToUTC t
      fromSlotNo <$> fromEither (Cardano.getSlotForRelativeTime relTime eraHistory)
    -- Converts a Plutus slot to a Cardano slot
    toSlotNo = Cardano.SlotNo . fromInteger . P.Ledger.getSlot
    -- Converts a Cardano slot to a Plutus slot
    fromSlotNo (Cardano.SlotNo w) = P.Ledger.Slot (toInteger w)
    -- Converts a POSIX time to a UTC time
    posixTimeToUTC = posixSecondsToUTCTime . fromRational . (/ 1000) . toRational . Api.getPOSIXTime
    -- Converts a UTC time to a POSIX time
    utcToPOSIXTime = Api.POSIXTime . round . (1000 *) . utcTimeToPOSIXSeconds
