-- | This module provides a direct (not staged) implementation of the
-- `MonadBlockChain` specification. This rely on the emulator from
-- cardano-node-emulator for transaction validation, although we have our own
-- internal state. This choice might be revised in the future.
module Cooked.MockChain.Direct where

import Cardano.Api qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Ledger.Shelley.LedgerState qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Applicative
import Control.Arrow
import Control.Monad (when, (<=<))
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cooked.Conversion.ToScript
import Cooked.Conversion.ToScriptHash
import Cooked.InitialDistribution
import Cooked.MockChain.Balancing
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.MinAda
import Cooked.MockChain.UtxoState
import Cooked.Output
import Cooked.Skeleton
import Data.Bifunctor (bimap)
import Data.Default
import Data.Either.Combinators (mapLeft)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Ledger.Index qualified as Ledger
import Ledger.Orphans ()
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core (view)
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- * Direct Emulation

-- $mockchaindocstr
--
-- The MockChainT monad provides a direct emulator; that is, it gives us a
-- simple way to call validator scripts directly, without the need for all the
-- complexity the 'Contract' monad introduces.
--
-- Running a 'MockChain' produces a 'UtxoState', a simplified view on
-- 'Api.UtxoIndex', which we also keep in our state.

mcstToUtxoState :: MockChainSt -> UtxoState
mcstToUtxoState MockChainSt {mcstIndex, mcstDatums} =
  UtxoState
    . foldr (\(address, utxoValueSet) acc -> Map.insertWith (<>) address utxoValueSet acc) Map.empty
    . mapMaybe
      ( extractPayload
          . bimap
            Ledger.fromCardanoTxIn
            Ledger.fromCardanoTxOutToPV2TxInfoTxOut'
      )
    . Map.toList
    . Cardano.unUTxO
    $ mcstIndex
  where
    extractPayload :: (Api.TxOutRef, Api.TxOut) -> Maybe (Api.Address, UtxoPayloadSet)
    extractPayload (txOutRef, out@Api.TxOut {Api.txOutAddress, Api.txOutValue, Api.txOutDatum}) =
      do
        let mRefScript = outputReferenceScriptHash out
        txSkelOutDatum <-
          case txOutDatum of
            Api.NoOutputDatum -> Just TxSkelOutNoDatum
            Api.OutputDatum datum -> fst <$> Map.lookup (Script.datumHash datum) mcstDatums
            Api.OutputDatumHash hash -> fst <$> Map.lookup hash mcstDatums
        return
          ( txOutAddress,
            UtxoPayloadSet [UtxoPayload txOutRef txOutValue txSkelOutDatum mRefScript]
          )

-- | Slightly more concrete version of 'UtxoState', used to actually run the
-- simulation.
data MockChainSt = MockChainSt
  { mcstParams :: Emulator.Params,
    mcstIndex :: Ledger.UtxoIndex,
    -- map from datum hash to (datum, count), where count is the number of UTxOs
    -- that currently have the datum. This map is used to display the contents
    -- of the state to the user, and to recover datums for transaction
    -- generation.
    mcstDatums :: Map Api.DatumHash (TxSkelOutDatum, Integer),
    mcstValidators :: Map Script.ValidatorHash (Script.Versioned Script.Validator),
    mcstCurrentSlot :: Ledger.Slot
  }
  deriving (Show)

-- | Generating an emulated state for the emulator from a mockchain state and
-- some parameters, based on a standard initial state
mcstToEmulatedLedgerState :: MockChainSt -> Emulator.EmulatedLedgerState
mcstToEmulatedLedgerState MockChainSt {..} =
  let els@(Emulator.EmulatedLedgerState le mps) = Emulator.initialState mcstParams
   in els
        { Emulator._ledgerEnv = le {Shelley.ledgerSlotNo = fromIntegral mcstCurrentSlot},
          Emulator._memPoolState =
            mps
              { Shelley.lsUTxOState =
                  Shelley.smartUTxOState
                    (Emulator.emulatorPParams mcstParams)
                    (Ledger.fromPlutusIndex mcstIndex)
                    (Emulator.Coin 0)
                    (Emulator.Coin 0)
                    def
                    (Emulator.Coin 0)
              }
        }

instance Eq MockChainSt where
  (MockChainSt params1 index1 datums1 validators1 currentSlot1)
    == (MockChainSt params2 index2 datums2 validators2 currentSlot2) =
      and
        [ params1 == params2,
          index1 == index2,
          datums1 == datums2,
          validators1 == validators2,
          currentSlot1 == currentSlot2
        ]

newtype MockChainT m a = MockChainT
  {unMockChain :: StateT MockChainSt (ExceptT MockChainError m) a}
  deriving newtype (Functor, Applicative, MonadState MockChainSt, MonadError MockChainError)

type MockChain = MockChainT Identity

-- | Custom monad instance made to increase the slot count automatically
instance (Monad m) => Monad (MockChainT m) where
  return = pure
  MockChainT x >>= f = MockChainT $ x >>= unMockChain . f

instance (Monad m) => MonadFail (MockChainT m) where
  fail = throwError . FailWith

instance MonadTrans MockChainT where
  lift = MockChainT . lift . lift

instance (Monad m, Alternative m) => Alternative (MockChainT m) where
  empty = MockChainT $ StateT $ const $ ExceptT empty
  (<|>) = combineMockChainT (<|>)

combineMockChainT ::
  (Monad m) =>
  (forall a. m a -> m a -> m a) ->
  MockChainT m x ->
  MockChainT m x ->
  MockChainT m x
combineMockChainT f ma mb = MockChainT $
  StateT $ \s ->
    let resA = runExceptT $ runStateT (unMockChain ma) s
        resB = runExceptT $ runStateT (unMockChain mb) s
     in ExceptT $ f resA resB

mapMockChainT ::
  (m (Either MockChainError (a, MockChainSt)) -> n (Either MockChainError (b, MockChainSt))) ->
  MockChainT m a ->
  MockChainT n b
mapMockChainT f = MockChainT . mapStateT (mapExceptT f) . unMockChain

-- | Executes a 'MockChainT' from some initial state; does /not/ convert the
-- 'MockChainSt' into a 'UtxoState'.
runMockChainTRaw ::
  (Monad m) =>
  MockChainSt ->
  MockChainT m a ->
  m (Either MockChainError (a, MockChainSt))
runMockChainTRaw i0 = runExceptT . flip runStateT i0 . unMockChain

-- | Executes a 'MockChainT' from an initial state set up with the given initial
-- value distribution. Similar to 'runMockChainT', uses the default
-- environment. Returns a 'UtxoState' instead of a 'MockChainSt'. If you need
-- the later, use 'runMockChainTRaw'
runMockChainTFrom ::
  (Monad m) =>
  InitialDistribution ->
  MockChainT m a ->
  m (Either MockChainError (a, UtxoState))
runMockChainTFrom i0 = fmap (fmap $ second mcstToUtxoState) . runMockChainTRaw (mockChainSt0From i0)

-- | Executes a 'MockChainT' from the canonical initial state and environment.
-- The canonical environment uses the default 'SlotConfig' and
-- @Cooked.Wallet.wallet 1@ as the sole wallet signing transactions.
runMockChainT :: (Monad m) => MockChainT m a -> m (Either MockChainError (a, UtxoState))
runMockChainT = runMockChainTFrom def

-- | See 'runMockChainTRaw'
runMockChainRaw :: MockChain a -> Either MockChainError (a, MockChainSt)
runMockChainRaw = runIdentity . runMockChainTRaw def

-- | See 'runMockChainTFrom'
runMockChainFrom :: InitialDistribution -> MockChain a -> Either MockChainError (a, UtxoState)
runMockChainFrom i0 = runIdentity . runMockChainTFrom i0

-- | See 'runMockChainT'
runMockChain :: MockChain a -> Either MockChainError (a, UtxoState)
runMockChain = runIdentity . runMockChainT

-- * Canonical initial values

utxoState0 :: UtxoState
utxoState0 = mcstToUtxoState mockChainSt0

mockChainSt0 :: MockChainSt
mockChainSt0 = MockChainSt def utxoIndex0 Map.empty Map.empty 0

-- * Initial `MockChainSt` from an initial distribution

mockChainSt0From :: InitialDistribution -> MockChainSt
mockChainSt0From i0 = MockChainSt def (utxoIndex0From i0) (datumMap0From i0) (referenceScriptMap0From i0) 0

instance Default MockChainSt where
  def = mockChainSt0

-- | Reference scripts from initial distributions should be accounted for in the
-- `MockChainSt` which is done using this function.
referenceScriptMap0From :: InitialDistribution -> Map Script.ValidatorHash (Script.Versioned Script.Validator)
referenceScriptMap0From (InitialDistribution initDist) =
  -- This builds a map of entries from the reference scripts contained in the
  -- initial distribution
  Map.fromList $ mapMaybe unitMaybeFrom initDist
  where
    -- This takes a single output and returns a possible map entry when it
    -- contains a reference script
    unitMaybeFrom :: TxSkelOut -> Maybe (Script.ValidatorHash, Script.Versioned Script.Validator)
    unitMaybeFrom (Pays output) = do
      refScript <- view outputReferenceScriptL output
      let vScript@(Script.Versioned script version) = toScript refScript
          Api.ScriptHash scriptHash = toScriptHash vScript
      return (Script.ValidatorHash scriptHash, Script.Versioned (Script.Validator script) version)

-- | Datums from initial distributions should be accounted for in the
-- `MockChainSt` which is done using this function.
datumMap0From :: InitialDistribution -> Map Api.DatumHash (TxSkelOutDatum, Integer)
datumMap0From (InitialDistribution initDist) =
  -- This concatenates singleton maps from inputs and accounts for the number of
  -- occurrences of similar datums
  foldl' (\m -> Map.unionWith (\(d, n1) (_, n2) -> (d, n1 + n2)) m . unitMapFrom) Map.empty initDist
  where
    -- This takes a single output and creates an empty map if it contains no
    -- datum, or a singleton map if it contains one
    unitMapFrom :: TxSkelOut -> Map Api.DatumHash (TxSkelOutDatum, Integer)
    unitMapFrom txSkelOut =
      let datum = view txSkelOutDatumL txSkelOut
       in maybe Map.empty (flip Map.singleton (datum, 1) . Script.datumHash) $ txSkelOutUntypedDatum datum

-- | This creates the initial UtxoIndex from an initial distribution by
-- submitting an initial transaction with the appropriate content:
--
-- - inputs consist of a single dummy pseudo input
--
-- - all non-ada assets in outputs are considered minted
--
-- - outputs are translated from the `TxSkelOut` list in the initial
--   distribution
--
-- Two things to note:
--
-- - We don't know what "Magic" means for the network ID (TODO)
--
-- - The genesis key hash has been taken from
--   https://github.com/input-output-hk/cardano-node/blob/543b267d75d3d448e1940f9ec04b42bd01bbb16b/cardano-api/test/Test/Cardano/Api/Genesis.hs#L60
utxoIndex0From :: InitialDistribution -> Ledger.UtxoIndex
utxoIndex0From (InitialDistribution initDist) = case mkBody of
  Left err -> error $ show err
  -- TODO: There may be better ways to generate this initial state, see
  -- createGenesisTransaction for instance
  Right body -> Ledger.initialise [[Emulator.unsafeMakeValid $ Ledger.CardanoEmulatorEraTx $ Cardano.Tx body []]]
  where
    mkBody :: Either GenerateTxError (Cardano.TxBody Cardano.ConwayEra)
    mkBody = do
      value <- mapLeft (ToCardanoError "Value error") $ Ledger.toCardanoValue (foldl' (\v -> (v <>) . view txSkelOutValueL) mempty initDist)
      let mintValue = flip (Cardano.TxMintValue Cardano.MaryEraOnwardsConway) (Cardano.BuildTxWith mempty) . Cardano.filterValue (/= Cardano.AdaAssetId) $ value
          theNetworkId = Cardano.Testnet $ Cardano.NetworkMagic 42
          genesisKeyHash = Cardano.GenesisUTxOKeyHash $ Shelley.KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194"
          inputs = [(Cardano.genesisUTxOPseudoTxIn theNetworkId genesisKeyHash, Cardano.BuildTxWith $ Cardano.KeyWitness Cardano.KeyWitnessForSpending)]
      outputs <- mapM (generateTxOut theNetworkId) initDist
      left (TxBodyError "Body error") $
        Cardano.createAndValidateTransactionBody Cardano.ShelleyBasedEraConway $
          Ledger.emptyTxBodyContent {Cardano.txMintValue = mintValue, Cardano.txOuts = outputs, Cardano.txIns = inputs}

utxoIndex0 :: Ledger.UtxoIndex
utxoIndex0 = utxoIndex0From def

-- * Direct Interpretation of Operations

getIndex :: Ledger.UtxoIndex -> Map Api.TxOutRef Ledger.TxOut
getIndex =
  Map.fromList
    . map (bimap Ledger.fromCardanoTxIn (Ledger.TxOut . toCtxTxTxOut))
    . Map.toList
    . Cardano.unUTxO
  where
    -- We need to convert a UTxO context TxOut to a Transaction context Tx out.
    -- It's complicated because the datum type is indexed by the context.
    toCtxTxTxOut :: Cardano.TxOut Cardano.CtxUTxO era -> Cardano.TxOut Cardano.CtxTx era
    toCtxTxTxOut (Cardano.TxOut addr val d refS) =
      let dat = case d of
            Cardano.TxOutDatumNone -> Cardano.TxOutDatumNone
            Cardano.TxOutDatumHash s h -> Cardano.TxOutDatumHash s h
            Cardano.TxOutDatumInline s sd -> Cardano.TxOutDatumInline s sd
       in Cardano.TxOut addr val dat refS

instance (Monad m) => MonadBlockChainBalancing (MockChainT m) where
  getParams = gets mcstParams
  validatorFromHash valHash = gets $ Map.lookup valHash . mcstValidators
  txOutByRefLedger outref = gets $ Map.lookup outref . getIndex . mcstIndex
  datumFromHash datumHash = (txSkelOutUntypedDatum <=< Just . fst <=< Map.lookup datumHash) <$> gets mcstDatums
  utxosAtLedger addr = filter ((addr ==) . outputAddress . txOutV2FromLedger . snd) <$> allUtxosLedger

instance (Monad m) => MonadBlockChainWithoutValidation (MockChainT m) where
  allUtxosLedger = gets $ Map.toList . getIndex . mcstIndex
  setParams newParams = modify (\st -> st {mcstParams = newParams})
  currentSlot = gets mcstCurrentSlot
  awaitSlot s = modify' (\st -> st {mcstCurrentSlot = max s (mcstCurrentSlot st)}) >> currentSlot

instance (Monad m) => MonadBlockChain (MockChainT m) where
  validateTxSkel skelUnbal = do
    -- We retrieve the current parameters
    oldParams <- getParams
    -- We compute the optionally modified parameters
    let newParams = applyEmulatorParamsModification (txOptEmulatorParamsModification . txSkelOpts $ skelUnbal) oldParams
    -- We change the parameters for the duration of the validation process
    setParams newParams
    -- We ensure that the outputs have the required minimal amount of ada, when
    -- requested in the skeleton options
    minAdaSkelUnbal <-
      if txOptEnsureMinAda . txSkelOpts $ skelUnbal
        then toTxSkelWithMinAda skelUnbal
        else return skelUnbal
    -- We balance the skeleton and get the associated fees and collateral
    -- inputs, when requested in the skeleton options
    (skel, fees, collateralIns) <- balanceTxSkel minAdaSkelUnbal
    -- We retrieve data that will be used in the transaction generation process:
    -- datums, validators and various kinds of inputs. This idea is to provide a
    -- rich-enough context for the transaction generation to succeed.
    insData <- txSkelInputData skel
    insValidators <- txSkelInputValidators skel
    insMap <- txSkelInputUtxosPl skel
    refInsMap <- txSkelReferenceInputUtxosPl skel
    collateralInsMap <- lookupUtxosPl $ Set.toList collateralIns
    -- We attempt to generate the transaction associated with the balanced
    -- skeleton and the retrieved data. This is an internal generation, there is
    -- no validation involved yet.
    cardanoTx <- case generateTx fees collateralIns newParams insData (insMap <> refInsMap <> collateralInsMap) insValidators skel of
      Left err -> throwError . MCEGenerationError $ err
      -- We apply post-generation modification when applicable
      Right tx -> return $ Ledger.CardanoEmulatorEraTx $ applyRawModOnBalancedTx (txOptUnsafeModTx . txSkelOpts $ skelUnbal) tx
    -- To run transaction validation we need a minimal ledger state
    eLedgerState <- gets mcstToEmulatedLedgerState
    -- We finally run the emulated validation, and we only care about the
    -- validation result, as we update our own internal state
    let (_, mValidationResult) = Emulator.validateCardanoTx newParams eLedgerState cardanoTx
    -- We retrieve our current utxo index to perform modifications associated
    -- with the validated transaction.
    utxoIndex <- gets mcstIndex
    -- We create a new utxo index with an error when validation failed
    let (newUtxoIndex, valError) = case mValidationResult of
          -- In case of a phase 1 error, we give back the same index
          Ledger.FailPhase1 _ err -> (utxoIndex, Just (Ledger.Phase1, err))
          -- In case of a phase 2 error, we retrieve the collaterals (and yes,
          -- despite its name, 'insertCollateral' actually takes the collaterals
          -- away from the index)
          Ledger.FailPhase2 _ err _ -> (Ledger.insertCollateral cardanoTx utxoIndex, Just (Ledger.Phase2, err))
          -- In case of success, we update the index with all inputs and outputs
          -- contained in the transaction
          Ledger.Success {} -> (Ledger.insert cardanoTx utxoIndex, Nothing)
    -- Now that we have compute a new index, we can update it
    modify' (\st -> st {mcstIndex = newUtxoIndex})
    case valError of
      -- When validation failed for any reason, we throw an error. TODO: This
      -- behavior could be subject to change in the future.
      Just err -> throwError (uncurry MCEValidationError err)
      -- Otherwise, we update known validators and datums.
      Nothing -> do
        modify' (\st -> st {mcstDatums = (mcstDatums st `removeMcstDatums` insData) `addMcstDatums` txSkelOutputData skel})
        modify' (\st -> st {mcstValidators = mcstValidators st `Map.union` (txSkelOutValidators skel <> txSkelOutReferenceScripts skel)})
    -- We apply a change of slot when requested in the options
    when (txOptAutoSlotIncrease $ txSkelOpts skel) $
      modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
    -- We return the parameters to their original state
    setParams oldParams
    -- We return the validated transaction
    return cardanoTx
    where
      addMcstDatums stored new = Map.unionWith (\(d, n1) (_, n2) -> (d, n1 + n2)) stored (Map.map (,1) new)
      -- FIXME: is this correct? What happens if we remove several similar
      -- datums?
      removeMcstDatums = Map.differenceWith $ \(d, n) _ -> if n == 1 then Nothing else Just (d, n - 1)
