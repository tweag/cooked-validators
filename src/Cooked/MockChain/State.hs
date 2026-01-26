-- | This module exposes the internal state in which our direct simulation is
-- run (`MockChainState`), as well as a restricted and simplified version
-- (`UtxoState`). The latter only consists of Utxos with a focus on who owns
-- those Utxos. You can see this as having some sort of an "account" view of the
-- ledger state, which typically does not exist in Cardano. This is useful for
-- two reasons:
--
-- - For printing purposes, where it is much more convient to see the available
--   assets as "who owns what" rather than a set of mixed Utxos.
--
-- - For testings purposes, when querying the final state of a run is
--   ineeded. For instance, properties such as "does Alice indeed owns 3 XXX
--   tokens at the end of this run?" become much easier to express.
module Cooked.MockChain.State
  ( -- * `MockChainState` and associated optics
    MockChainState (..),
    mcstParamsL,
    mcstLedgerStateL,
    mcstOutputsL,
    mcstConstitutionL,
    mcstMOutputL,

    -- * Helpers to add or remove outputs from a `MockChainState`
    addOutput,
    removeOutput,

    -- * `UtxoState`: A simplified, address-focused view on a `MockChainState`
    UtxoPayloadDatum (..),
    utxoPayloadDatumKindAT,
    utxoPayloadDatumTypedAT,
    UtxoPayload (..),
    utxoPayloadTxOutRefL,
    utxoPayloadValueL,
    utxoPayloadDatumL,
    utxoPayloadMReferenceScriptHashL,
    utxoPayloadReferenceScriptHashAT,
    UtxoPayloadSet (..),
    utxoPayloadSetListI,
    UtxoState (..),
    availableUtxosL,
    consumedUtxosL,

    -- * Querying the assets owned by a given address
    holdsInState,

    -- * Transforming a `MockChainState` into an `UtxoState`
    mcstToUtxoState,
  )
where

import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Cooked.Skeleton
import Data.Default
import Data.Function (on)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Typeable
import Ledger.Orphans ()
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api

-- | The state used to run the simulation in 'Cooked.MockChain.Direct'
data MockChainState where
  MockChainState ::
    { -- | The parametors of the emulated blockchain
      mcstParams :: Emulator.Params,
      -- | The ledger state of the emulated blockchain
      mcstLedgerState :: Emulator.EmulatedLedgerState,
      -- | Associates to each 'Api.TxOutRef' the 'TxSkelOut' that produced it,
      -- alongside a boolean to state whether this UTxO is still present in the
      -- index ('True') or has already been consumed ('False').
      mcstOutputs :: Map Api.TxOutRef (TxSkelOut, Bool),
      -- | The constitution script to be used with proposals
      mcstConstitution :: Maybe VScript
    } ->
    MockChainState
  deriving (Show)

-- | A lens to set or get the parameters of the 'MockChainState'
makeLensesFor [("mcstParams", "mcstParamsL")] ''MockChainState

-- | A lens to set or get the ledger state of the 'MockChainState'
makeLensesFor [("mcstLedgerState", "mcstLedgerStateL")] ''MockChainState

-- | A lens to set or get the outputs of the 'MockChainState'
makeLensesFor [("mcstOutputs", "mcstOutputsL")] ''MockChainState

-- | A lens to set or get the constitution script of the 'MockChainState'
makeLensesFor [("mcstConstitution", "mcstConstitutionL")] ''MockChainState

instance Default MockChainState where
  def = MockChainState def (Emulator.initialState def) Map.empty Nothing

-- | Accesses a given available Utxo from a `MockChainState`
mcstMOutputL :: Api.TxOutRef -> Lens' MockChainState (Maybe TxSkelOut)
mcstMOutputL oRef = mcstOutputsL % at oRef % iso (fmap fst) (fmap (,True))

-- | Stores an output in a 'MockChainState'
addOutput :: Api.TxOutRef -> TxSkelOut -> MockChainState -> MockChainState
addOutput oRef = set (mcstMOutputL oRef) . Just

-- | Removes an output from the 'MockChainState'
removeOutput :: Api.TxOutRef -> MockChainState -> MockChainState
removeOutput oRef = set (mcstOutputsL % at oRef) Nothing

-- | A simplified version of a 'Cooked.Skeleton.Datum.TxSkelOutDatum' which only
-- stores the actual datum and whether it is hashed (@True@) or inline
-- (@False@). The only difference is that whether the datum was resolved in the
-- transaction creating it on the ledger is absent, which makes sense after the
-- fact.
data UtxoPayloadDatum where
  NoUtxoPayloadDatum :: UtxoPayloadDatum
  SomeUtxoPayloadDatum :: (DatumConstrs dat) => dat -> Bool -> UtxoPayloadDatum

-- | Focuses on whether on not this `UtxoPayloadDatum` isHashed
utxoPayloadDatumKindAT :: AffineTraversal' UtxoPayloadDatum Bool
utxoPayloadDatumKindAT =
  atraversal
    ( \case
        NoUtxoPayloadDatum -> Left NoUtxoPayloadDatum
        SomeUtxoPayloadDatum _ b -> Right b
    )
    ( flip
        ( \kind -> \case
            NoUtxoPayloadDatum -> NoUtxoPayloadDatum
            SomeUtxoPayloadDatum content _ -> SomeUtxoPayloadDatum content kind
        )
    )

-- | Extracts, or sets, the typed datum of a 'UtxoPayloadDatum' following the
-- same rules as `txSkelOutDatumTypedAT`
utxoPayloadDatumTypedAT :: (DatumConstrs a, DatumConstrs b) => AffineTraversal UtxoPayloadDatum UtxoPayloadDatum a b
utxoPayloadDatumTypedAT =
  atraversal
    ( \case
        (SomeUtxoPayloadDatum content _) | Just content' <- cast content -> Right content'
        (SomeUtxoPayloadDatum content _) | Just content' <- Api.fromBuiltinData $ Api.toBuiltinData content -> Right content'
        dc -> Left dc
    )
    ( flip
        ( \content -> \case
            NoUtxoPayloadDatum -> NoUtxoPayloadDatum
            SomeUtxoPayloadDatum _ kind -> SomeUtxoPayloadDatum content kind
        )
    )

deriving instance Show UtxoPayloadDatum

instance Ord UtxoPayloadDatum where
  compare NoUtxoPayloadDatum NoUtxoPayloadDatum = EQ
  compare NoUtxoPayloadDatum _ = LT
  compare _ NoUtxoPayloadDatum = GT
  compare
    (SomeUtxoPayloadDatum (Api.toBuiltinData -> dat) b)
    (SomeUtxoPayloadDatum (Api.toBuiltinData -> dat') b') =
      compare (dat, b) (dat', b')

instance Eq UtxoPayloadDatum where
  dat == dat' = compare dat dat' == EQ

-- | A convenient wrapping of the interesting information of a UTxO.
data UtxoPayload where
  UtxoPayload ::
    { -- | The reference of this UTxO
      utxoPayloadTxOutRef :: Api.TxOutRef,
      -- | The value stored in this UTxO
      utxoPayloadValue :: Api.Value,
      -- | The optional datum stored in this UTxO
      utxoPayloadDatum :: UtxoPayloadDatum,
      -- | The hash of the optional reference script stored in this UTxO
      utxoPayloadReferenceScriptHash :: Maybe Api.ScriptHash
    } ->
    UtxoPayload
  deriving (Eq, Show)

makeLensesFor [("utxoPayloadTxOutRef", "utxoPayloadTxOutRefL")] ''UtxoPayload

makeLensesFor [("utxoPayloadValue", "utxoPayloadValueL")] ''UtxoPayload

makeLensesFor [("utxoPayloadDatum", "utxoPayloadDatumL")] ''UtxoPayload

makeLensesFor [("utxoPayloadReferenceScriptHash", "utxoPayloadMReferenceScriptHashL")] ''UtxoPayload

utxoPayloadReferenceScriptHashAT :: AffineTraversal' UtxoPayload Api.ScriptHash
utxoPayloadReferenceScriptHashAT = utxoPayloadMReferenceScriptHashL % _Just

-- | Represents a /set/ of payloads.
newtype UtxoPayloadSet = UtxoPayloadSet
  { -- | List of UTxOs contained in this 'UtxoPayloadSet'
    utxoPayloadSet :: [UtxoPayload]
    -- We use a list instead of a set because 'Api.Value' doesn't implement 'Ord'
    -- and because it is possible that we want to distinguish between utxo states
    -- that have additional utxos, even if these could have been merged together.
  }
  deriving (Show)

utxoPayloadSetListI :: Iso' UtxoPayloadSet [UtxoPayload]
utxoPayloadSetListI = iso utxoPayloadSet UtxoPayloadSet

instance Eq UtxoPayloadSet where
  (UtxoPayloadSet xs) == (UtxoPayloadSet ys) = xs' == ys'
    where
      k (UtxoPayload ref val dat rs) = (ref, Api.flattenValue val, dat, rs)
      xs' = List.sortBy (compare `on` k) xs
      ys' = List.sortBy (compare `on` k) ys

instance Semigroup UtxoPayloadSet where
  UtxoPayloadSet a <> UtxoPayloadSet b = UtxoPayloadSet $ a ++ b

instance Monoid UtxoPayloadSet where
  mempty = UtxoPayloadSet []

-- | A description of who owns what in a blockchain. Owners are addresses and
-- they each own a 'UtxoPayloadSet'.
data UtxoState where
  UtxoState ::
    { -- | Utxos available to be consumed
      availableUtxos :: Map Api.Address UtxoPayloadSet,
      -- | Utxos already consumed
      consumedUtxos :: Map Api.Address UtxoPayloadSet
    } ->
    UtxoState
  deriving (Eq)

makeLensesFor [("availableUtxos", "availableUtxosL")] ''UtxoState

makeLensesFor [("consumedUtxos", "consumedUtxosL")] ''UtxoState

instance Semigroup UtxoState where
  (UtxoState a c) <> (UtxoState a' c') = UtxoState (Map.unionWith (<>) a a') (Map.unionWith (<>) c c')

instance Monoid UtxoState where
  mempty = UtxoState Map.empty Map.empty

-- | Total value accessible to what's pointed by the address.
holdsInState :: (Script.ToAddress a) => a -> UtxoState -> Api.Value
holdsInState (Script.toAddress -> address) = maybe mempty utxoPayloadSetTotal . view (availableUtxosL % at address)

-- | Computes the total value in a set
utxoPayloadSetTotal :: UtxoPayloadSet -> Api.Value
utxoPayloadSetTotal = foldOf (utxoPayloadSetListI % folded % utxoPayloadValueL)

-- | Builds a 'UtxoState' from a 'MockChainState'
mcstToUtxoState :: MockChainState -> UtxoState
mcstToUtxoState =
  foldl extractPayload mempty . Map.toList . mcstOutputs
  where
    extractPayload :: UtxoState -> (Api.TxOutRef, (TxSkelOut, Bool)) -> UtxoState
    extractPayload utxoState (txOutRef, (txSkelOut, bool)) =
      let newAddress = view txSkelOutAddressG txSkelOut
          newPayloadSet =
            UtxoPayloadSet
              [ UtxoPayload
                  txOutRef
                  (view txSkelOutValueL txSkelOut)
                  ( case txSkelOut ^. txSkelOutDatumL of
                      NoTxSkelOutDatum -> NoUtxoPayloadDatum
                      SomeTxSkelOutDatum content kind -> SomeUtxoPayloadDatum content (kind /= Inline)
                  )
                  (preview txSkelOutReferenceScriptHashAF txSkelOut)
              ]
       in if bool
            then utxoState {availableUtxos = Map.insertWith (<>) newAddress newPayloadSet (availableUtxos utxoState)}
            else utxoState {consumedUtxos = Map.insertWith (<>) newAddress newPayloadSet (consumedUtxos utxoState)}
