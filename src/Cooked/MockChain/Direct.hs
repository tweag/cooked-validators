{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Direct where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Ledger.Shelley.API as CardanoLedger
import qualified Cardano.Ledger.Shelley.LedgerState as Ledger
import qualified Cardano.Node.Emulator.Internal.Node.Params as Emulator
import qualified Cardano.Node.Emulator.Internal.Node.Validation as Emulator
import Control.Applicative
import Control.Arrow
import Control.Monad (when, (<=<))
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cooked.InitialDistribution
import Cooked.MockChain.Balancing
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.UtxoState
import Cooked.Output
import Cooked.Skeleton
import Data.Bifunctor (bimap)
import Data.Default
import Data.Either.Combinators (mapLeft)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Ledger.Index as Ledger
import Ledger.Orphans ()
import qualified Ledger.Slot as Ledger
import qualified Ledger.Tx as Ledger
import qualified Ledger.Tx.CardanoAPI as Ledger
import Optics.Core (view)
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified PlutusLedgerApi.V3 as Pl

-- * Direct Emulation

-- $mockchaindocstr
--
-- The MockChainT monad provides a direct emulator; that is, it gives us a
-- simple way to call validator scripts directly, without the need for all the
-- complexity the 'Contract' monad introduces.
--
-- Running a 'MockChain' produces a 'UtxoState', a simplified view on
-- 'Pl.UtxoIndex', which we also keep in our state.

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
    . C.unUTxO
    $ mcstIndex
  where
    extractPayload :: (Pl.TxOutRef, Pl.TxOut) -> Maybe (Pl.Address, UtxoPayloadSet)
    extractPayload (txOutRef, out@Pl.TxOut {Pl.txOutAddress, Pl.txOutValue, Pl.txOutDatum}) =
      do
        let mRefScript = outputReferenceScriptHash out
        txSkelOutDatum <-
          case txOutDatum of
            Pl.NoOutputDatum -> Just TxSkelOutNoDatum
            Pl.OutputDatum datum -> fst <$> Map.lookup (Pl.datumHash datum) mcstDatums
            Pl.OutputDatumHash hash -> fst <$> Map.lookup hash mcstDatums
        return
          ( txOutAddress,
            UtxoPayloadSet [UtxoPayload txOutRef txOutValue txSkelOutDatum mRefScript]
          )

-- | Slightly more concrete version of 'UtxoState', used to actually run the
-- simulation.
data MockChainSt = MockChainSt
  { mcstIndex :: Ledger.UtxoIndex,
    -- map from datum hash to (datum, count), where count is the number of
    -- UTxOs that currently have the datum. This map is used  to display the
    -- contents of the state to the user, and to recover datums for transaction
    -- generation.
    mcstDatums :: Map Pl.DatumHash (TxSkelOutDatum, Integer),
    mcstValidators :: Map Pl.ValidatorHash (Pl.Versioned Pl.Validator),
    mcstCurrentSlot :: Ledger.Slot
  }
  deriving (Show)

-- | Generating an emulated state for the emulator from a mockchain
-- state and some parameters, based on a standard initial state
mcstToEmulatedLedgerState :: Emulator.Params -> MockChainSt -> Emulator.EmulatedLedgerState
mcstToEmulatedLedgerState params MockChainSt {..} =
  let els@(Emulator.EmulatedLedgerState le mps) = Emulator.initialState params
   in els
        { Emulator._ledgerEnv = le {CardanoLedger.ledgerSlotNo = fromIntegral mcstCurrentSlot},
          Emulator._memPoolState =
            mps
              { CardanoLedger.lsUTxOState =
                  Ledger.smartUTxOState
                    (Emulator.emulatorPParams params)
                    (Ledger.fromPlutusIndex mcstIndex)
                    (Emulator.Coin 0)
                    (Emulator.Coin 0)
                    def
                    (Emulator.Coin 0)
              }
        }

instance Eq MockChainSt where
  (MockChainSt index1 datums1 validators1 currentSlot1)
    == (MockChainSt index2 datums2 validators2 currentSlot2) =
      and
        [ index1 == index2,
          datums1 == datums2,
          validators1 == validators2,
          currentSlot1 == currentSlot2
        ]

instance Default Ledger.Slot where
  def = Ledger.Slot 0

newtype MockChainEnv = MockChainEnv {mceParams :: Emulator.Params}
  deriving (Show)

instance Default MockChainEnv where
  def = MockChainEnv def

newtype MockChainT m a = MockChainT
  {unMockChain :: ReaderT MockChainEnv (StateT MockChainSt (ExceptT MockChainError m)) a}
  deriving newtype (Functor, Applicative, MonadState MockChainSt, MonadError MockChainError, MonadReader MockChainEnv)

type MockChain = MockChainT Identity

-- | Custom monad instance made to increase the slot count automatically
instance (Monad m) => Monad (MockChainT m) where
  return = pure
  MockChainT x >>= f = MockChainT $ x >>= unMockChain . f

instance (Monad m) => MonadFail (MockChainT m) where
  fail = throwError . FailWith

instance MonadTrans MockChainT where
  lift = MockChainT . lift . lift . lift

instance (Monad m, Alternative m) => Alternative (MockChainT m) where
  empty = MockChainT $ ReaderT $ const $ StateT $ const $ ExceptT empty
  (<|>) = combineMockChainT (<|>)

combineMockChainT ::
  (Monad m) =>
  (forall a. m a -> m a -> m a) ->
  MockChainT m x ->
  MockChainT m x ->
  MockChainT m x
combineMockChainT f ma mb = MockChainT $
  ReaderT $ \r ->
    StateT $ \s ->
      let resA = runExceptT $ runStateT (runReaderT (unMockChain ma) r) s
          resB = runExceptT $ runStateT (runReaderT (unMockChain mb) r) s
       in ExceptT $ f resA resB

mapMockChainT ::
  (m (Either MockChainError (a, MockChainSt)) -> n (Either MockChainError (b, MockChainSt))) ->
  MockChainT m a ->
  MockChainT n b
mapMockChainT f = MockChainT . mapReaderT (mapStateT (mapExceptT f)) . unMockChain

-- | Executes a 'MockChainT' from some initial state and environment; does /not/
-- convert the 'MockChainSt' into a 'UtxoState'.
runMockChainTRaw ::
  (Monad m) =>
  MockChainEnv ->
  MockChainSt ->
  MockChainT m a ->
  m (Either MockChainError (a, MockChainSt))
runMockChainTRaw e0 i0 =
  runExceptT
    . flip runStateT i0
    . flip runReaderT e0
    . unMockChain

-- | Executes a 'MockChainT' from an initial state set up with the given
-- initial value distribution. Similar to 'runMockChainT', uses the default
-- environment. Returns a 'UtxoState' instead of a 'MockChainSt'. If you need
-- the later, use 'runMockChainTRaw'
runMockChainTFrom ::
  (Monad m) =>
  InitialDistribution ->
  MockChainT m a ->
  m (Either MockChainError (a, UtxoState))
runMockChainTFrom i0 =
  fmap (fmap $ second mcstToUtxoState) . runMockChainTRaw def (mockChainSt0From i0)

-- | Executes a 'MockChainT' from the canonical initial state and environment.
-- The canonical environment uses the default 'SlotConfig' and
-- @Cooked.Wallet.wallet 1@ as the sole wallet signing transactions.
runMockChainT :: (Monad m) => MockChainT m a -> m (Either MockChainError (a, UtxoState))
runMockChainT = runMockChainTFrom def

-- | See 'runMockChainTRaw'
runMockChainRaw :: MockChainEnv -> MockChainSt -> MockChain a -> Either MockChainError (a, MockChainSt)
runMockChainRaw e0 i0 = runIdentity . runMockChainTRaw e0 i0

-- | See 'runMockChainTFrom'
runMockChainFrom ::
  InitialDistribution -> MockChain a -> Either MockChainError (a, UtxoState)
runMockChainFrom i0 = runIdentity . runMockChainTFrom i0

-- | See 'runMockChainT'
runMockChain :: MockChain a -> Either MockChainError (a, UtxoState)
runMockChain = runIdentity . runMockChainT

-- * Canonical initial values

utxoState0 :: UtxoState
utxoState0 = mcstToUtxoState mockChainSt0

mockChainSt0 :: MockChainSt
mockChainSt0 = MockChainSt utxoIndex0 Map.empty Map.empty def

-- * Initial `MockChainSt` from an initial distribution

mockChainSt0From :: InitialDistribution -> MockChainSt
mockChainSt0From i0 =
  MockChainSt
    (utxoIndex0From i0)
    (datumMap0From i0)
    (referenceScriptMap0From i0)
    def

instance Default MockChainSt where
  def = mockChainSt0

-- | Reference scripts from initial distributions should be accounted
-- for in the `MockChainSt` which is done using this function.
referenceScriptMap0From :: InitialDistribution -> Map Pl.ValidatorHash (Pl.Versioned Pl.Validator)
referenceScriptMap0From (InitialDistribution initDist) =
  -- This builds a map of entries from the reference scripts contained
  -- in the initial distribution
  Map.fromList $ mapMaybe unitMaybeFrom initDist
  where
    -- This takes a single output and returns a possible map entry
    -- when it contains a reference script
    unitMaybeFrom :: TxSkelOut -> Maybe (Pl.ValidatorHash, Pl.Versioned Pl.Validator)
    unitMaybeFrom (Pays output) = do
      refScript <- view outputReferenceScriptL output
      let vScript@(Pl.Versioned script version) = toScript refScript
          Pl.ScriptHash scriptHash = toScriptHash vScript
      return (Pl.ValidatorHash scriptHash, Pl.Versioned (Pl.Validator script) version)

-- | Datums from initial distributions should be accounted for in the
-- `MockChainSt` which is done using this function.
datumMap0From :: InitialDistribution -> Map Pl.DatumHash (TxSkelOutDatum, Integer)
datumMap0From (InitialDistribution initDist) =
  -- This concatenates singleton maps from inputs and accounts for the
  -- number of occurrences of similar datums
  foldl' (\m -> Map.unionWith (\(d, n1) (_, n2) -> (d, n1 + n2)) m . unitMapFrom) Map.empty initDist
  where
    -- This takes a single output and creates an empty map if it
    -- contains no datum, or a singleton map if it contains one
    unitMapFrom :: TxSkelOut -> Map Pl.DatumHash (TxSkelOutDatum, Integer)
    unitMapFrom txSkelOut =
      let datum = view txSkelOutDatumL txSkelOut
       in maybe Map.empty (flip Map.singleton (datum, 1) . Pl.datumHash) $ txSkelOutUntypedDatum datum

-- | This creates the initial UtxoIndex from an initial distribution
-- by submitting an initial transaction with the appropriate content:
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
  -- There may be better ways to generate this initial state, see createGenesisTransaction for instance
  Right body -> Ledger.initialise [[Emulator.unsafeMakeValid $ Ledger.CardanoEmulatorEraTx $ C.Tx body []]]
  where
    mkBody :: Either GenerateTxError (C.TxBody C.ConwayEra)
    mkBody = do
      value <- mapLeft (ToCardanoError "Value error") $ Ledger.toCardanoValue (foldl' (\v -> (v <>) . view txSkelOutValueL) mempty initDist)
      let mintValue = flip (C.TxMintValue C.MaryEraOnwardsConway) (C.BuildTxWith mempty) . C.filterValue (/= C.AdaAssetId) $ value
          theNetworkId = C.Testnet $ C.NetworkMagic 42
          genesisKeyHash = C.GenesisUTxOKeyHash $ CardanoLedger.KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194"
          inputs = [(C.genesisUTxOPseudoTxIn theNetworkId genesisKeyHash, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
      outputs <- mapM (generateTxOut theNetworkId) initDist
      left (TxBodyError "Body error") $
        C.createAndValidateTransactionBody C.ShelleyBasedEraConway $
          Ledger.emptyTxBodyContent {C.txMintValue = mintValue, C.txOuts = outputs, C.txIns = inputs}

utxoIndex0 :: Ledger.UtxoIndex
utxoIndex0 = utxoIndex0From def

-- * Direct Interpretation of Operations

getIndex :: Ledger.UtxoIndex -> Map Pl.TxOutRef Ledger.TxOut
getIndex =
  Map.fromList
    . map (bimap Ledger.fromCardanoTxIn (Ledger.TxOut . toCtxTxTxOut))
    . Map.toList
    . C.unUTxO
  where
    -- We need to convert a UTxO context TxOut to a Transaction context Tx out.
    -- One could be forgiven for thinking this exists somewhere, but I couldn't find
    -- it. It's this complicated because the datum type is indexed by the context.
    toCtxTxTxOut :: C.TxOut C.CtxUTxO era -> C.TxOut C.CtxTx era
    toCtxTxTxOut (C.TxOut addr val d refS) =
      let dat = case d of
            C.TxOutDatumNone -> C.TxOutDatumNone
            C.TxOutDatumHash s h -> C.TxOutDatumHash s h
            C.TxOutDatumInline s sd -> C.TxOutDatumInline s sd
       in C.TxOut addr val dat refS

instance (Monad m) => MonadBlockChainBalancing (MockChainT m) where
  getParams = asks mceParams
  validatorFromHash valHash = gets $ Map.lookup valHash . mcstValidators
  txOutByRefLedger outref = gets $ Map.lookup outref . getIndex . mcstIndex
  datumFromHash datumHash = (txSkelOutUntypedDatum <=< Just . fst <=< Map.lookup datumHash) <$> gets mcstDatums
  utxosAtLedger addr = filter ((addr ==) . outputAddress . txOutV2FromLedger . snd) <$> allUtxosLedger

instance (Monad m) => MonadBlockChainWithoutValidation (MockChainT m) where
  allUtxosLedger = gets $ Map.toList . getIndex . mcstIndex

  currentSlot = gets mcstCurrentSlot

  awaitSlot s = modify' (\st -> st {mcstCurrentSlot = max s (mcstCurrentSlot st)}) >> currentSlot

instance (Monad m) => MonadBlockChain (MockChainT m) where
  validateTxSkel skelUnbal = do
    -- We balance the skeleton (when requested in the options) and get
    -- the associated fees and collateral inputs
    (skel, fees, collateralIns) <- balanceTxSkel skelUnbal
    -- We apply the optional modifications of the emulator parameters
    params <- applyEmulatorParamsModification (txOptEmulatorParamsModification . txSkelOpts $ skel) <$> getParams
    -- We retrieve data that will be used in the transaction
    -- generation process: datums, validators and various kinds of
    -- inputs. This idea is to provide a rich-enough context for the
    -- transaction generation to succeed.
    insData <- txSkelInputData skel
    insValidators <- txSkelInputValidators skel
    insMap <- txSkelInputUtxosPl skel
    refInsMap <- txSkelReferenceInputUtxosPl skel
    collateralInsMap <- lookupUtxosPl $ Set.toList collateralIns
    -- We attempt to generate the transaction associated with the
    -- balanced skeleton and the retrieved data. This is an internal
    -- generation, there is no validation involved yet.
    cardanoTx <- case generateTx fees collateralIns params insData (insMap <> refInsMap <> collateralInsMap) insValidators skel of
      Left err -> throwError . MCEGenerationError $ err
      -- We apply post-generation modification when applicable
      Right tx -> return $ Ledger.CardanoEmulatorEraTx $ applyRawModOnBalancedTx (txOptUnsafeModTx . txSkelOpts $ skelUnbal) tx
    -- To run transaction validation we need a minimal ledger state
    eLedgerState <- gets (mcstToEmulatedLedgerState params)
    -- We finally run the emulated validation, and we only care about
    -- the validation result, as we update our own internal state
    let (_, mValidationResult) = Emulator.validateCardanoTx params eLedgerState cardanoTx
    -- We retrieve our current utxo index to perform modifications
    -- associated with the validated transaction.
    utxoIndex <- gets mcstIndex
    -- We create a new utxo index with an error when validation failed
    let (newUtxoIndex, valError) = case mValidationResult of
          -- In case of a phase 1 error, we give back the same index
          Ledger.FailPhase1 _ err -> (utxoIndex, Just (Ledger.Phase1, err))
          -- In case of a phase 2 error, we retrieve the collaterals
          -- (and yes, despite its name, 'insertCollateral' actually
          -- takes is away from the index
          Ledger.FailPhase2 _ err _ -> (Ledger.insertCollateral cardanoTx utxoIndex, Just (Ledger.Phase2, err))
          -- In case of success, we update the index with all inputs
          -- and outputs contained in the transaction
          Ledger.Success {} -> (Ledger.insert cardanoTx utxoIndex, Nothing)
    -- Now that we have compute a new index, we can update it
    modify' (\st -> st {mcstIndex = newUtxoIndex})
    case valError of
      -- When validation failed for any reason, we throw an error.
      -- This behavior could be subject to change in the future.
      Just err -> throwError (uncurry MCEValidationError err)
      -- Otherwise, we update known validators and datums.
      Nothing -> do
        modify' (\st -> st {mcstDatums = (mcstDatums st `removeMcstDatums` insData) `addMcstDatums` txSkelOutputData skel})
        modify' (\st -> st {mcstValidators = mcstValidators st `Map.union` (txSkelOutValidators skel <> txSkelOutReferenceScripts skel)})
    -- We apply a change of slot when requested in the options
    when (txOptAutoSlotIncrease $ txSkelOpts skel) $
      modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
    -- We return the validated transaction
    return cardanoTx
    where
      addMcstDatums stored new = Map.unionWith (\(d, n1) (_, n2) -> (d, n1 + n2)) stored (Map.map (,1) new)
      -- FIXME: is this correct? What happens if we remove several similar datums?
      removeMcstDatums = Map.differenceWith $ \(d, n) _ -> if n == 1 then Nothing else Just (d, n - 1)
