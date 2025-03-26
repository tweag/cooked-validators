{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Transaction validation using 'cardano-ledger-specs'
module Cardano.Node.Emulator.Internal.Node.Validation
  ( EmulatedLedgerState (..),
    Coin (..),
    SlotNo (..),
    EmulatorEra,
    CardanoLedgerError,
    initialState,
    hasValidationErrors,
    createAndValidateTransactionBody,
    validateCardanoTx,
    getTxExUnitsWithLogs,
    unsafeMakeValid,
    validateAndApplyTx,

    -- * Modifying the state
    updateSlot,
    nextSlot,
    getSlot,
    UTxO (..),
    setUtxo,

    -- * Lenses
    ledgerEnv,
    memPoolState,

    -- * Etc.
    emulatorGlobals,
  )
where

import Cardano.Api.Internal.Error qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Plutus.Evaluate
  ( collectPlutusScriptsWithContext,
    evalPlutusScripts,
  )
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoUtxoPredFailure (UtxosFailure),
    AlonzoUtxosPredFailure (CollectErrors),
  )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (AlonzoTx), IsValid (IsValid))
import Cardano.Ledger.Api.Transition (createInitialState)
import Cardano.Ledger.Api.Tx
  ( TransactionScriptFailure (ValidationFailure),
    evalTxExUnitsWithLogs,
  )
import Cardano.Ledger.BaseTypes (Globals (systemStart), epochInfo)
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure (ConwayUtxowFailure))
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Plutus.Evaluate (ScriptResult (Fails, Passes))
import Cardano.Ledger.Shelley.API
  ( ApplyTxError (ApplyTxError),
    Coin (Coin),
    LedgerEnv (LedgerEnv, ledgerSlotNo),
    LedgerState (lsUTxOState),
    MempoolEnv,
    UTxO (UTxO),
    Validated,
    unsafeMakeValidated,
  )
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Cardano.Ledger.Shelley.LedgerState (esLState, nesEs, smartUTxOState, utxosUtxo)
import Cardano.Node.Emulator.Internal.Node.Params
  ( EmulatorEra,
    Params (pConfig),
    emulatorGlobals,
    emulatorPParams,
    ledgerProtocolParameters,
  )
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Control.Lens (makeLenses, over, view, (&), (.~))
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Text qualified as Text
import Ledger.Blockchain (OnChainTx (OnChainTx))
import Ledger.Index (genesisTxIn, getCollateral)
import Ledger.Index.Internal qualified as P
import Ledger.Tx (CardanoTx (CardanoEmulatorEraTx))
import Ledger.Tx.CardanoAPI qualified as P
import PlutusLedgerApi.V1 qualified as V1 hiding (TxOut (..))
import PlutusLedgerApi.V1.Scripts qualified as P

type CardanoLedgerError = Either P.ValidationErrorInPhase P.ToCardanoError

{- Note [Emulated ledger]

In the real cardano node, there two types of validation: Transaction validation
(performed when a transaction is first added to the mempool) and block
validation (performed when a block is created by the local node or received
from a peer).

Transaction validation runs the Plutus scripts, checks cryptographic
signatures, balances, existence of transaction inputs and so forth. This is
where the ledger state is updated. Block validation performs other checks
related to the consensus algorithm.

Networking and consensus issues are not part of the emulator's scope. We only
care about transaction validation here, so we don't have to worry about block
validation.

The decision to leave out block validation and consensus-related concerns has
the following implications:

1. We can represent blocks as simple lists-of-transactions
2. We can modify time (the slot number) and ledger parameters as we wish,
   without having to post transactions that modify them.

There are also some limitations of the emulator's functionality that could be
addressed by extending the emulator, without having to bring in the full block
validating machinery.

\* We cannot represent different eras - everything is 'ConwayEra'.
\* There is no handling of epoch boundaries, rewards, etc.
\* The block size is unlimited - we simply take all transactions from the
  mempool when we make a block. There is however a limit on the size of
  individual transactions.
\* We use the standard ledger cryptography everywhere ('StandardCrypto').
  This could be replaced by "NoCrypto" for faster validation.

-}

-- | State of the ledger with configuration, mempool, and the blockchain.
data EmulatedLedgerState = EmulatedLedgerState
  { _ledgerEnv :: !(MempoolEnv EmulatorEra),
    _memPoolState :: !(LedgerState EmulatorEra)
  }
  deriving (Show)

makeLenses ''EmulatedLedgerState

-- | Increase the slot number by one
nextSlot :: EmulatedLedgerState -> EmulatedLedgerState
nextSlot = over ledgerEnv f
  where
    f l@LedgerEnv {ledgerSlotNo = oldSlot} = l {ledgerSlotNo = succ oldSlot}

-- | Set the slot number
updateSlot :: (SlotNo -> SlotNo) -> EmulatedLedgerState -> EmulatedLedgerState
updateSlot f = over ledgerEnv (\l -> l {ledgerSlotNo = f (ledgerSlotNo l)})

-- | Get the slot number
getSlot :: (Num a) => EmulatedLedgerState -> a
getSlot (EmulatedLedgerState LedgerEnv {ledgerSlotNo = SlotNo s} _) = fromIntegral s

-- | Set the utxo
setUtxo :: Params -> UTxO EmulatorEra -> EmulatedLedgerState -> EmulatedLedgerState
setUtxo params utxo els@EmulatedLedgerState {_memPoolState} = els {_memPoolState = newPoolState}
  where
    newPoolState =
      _memPoolState
        { lsUTxOState = smartUTxOState (emulatorPParams params) utxo (Coin 0) (Coin 0) def (Coin 0)
        }

-- | Get the utxo
getUtxo :: EmulatedLedgerState -> UTxO EmulatorEra
getUtxo = utxosUtxo . lsUTxOState . view memPoolState

-- | Initial ledger state for a distribution
initialState :: Params -> EmulatedLedgerState
initialState params =
  EmulatedLedgerState
    { _ledgerEnv =
        C.Ledger.LedgerEnv
          { C.Ledger.ledgerSlotNo = 0,
            C.Ledger.ledgerIx = minBound,
            C.Ledger.ledgerPp = emulatorPParams params,
            C.Ledger.ledgerAccount = C.Ledger.AccountState (Coin 0) (Coin 0),
            C.Ledger.ledgerMempool = True, -- TODO, what does it mean?
            C.Ledger.ledgerEpochNo = Nothing
          },
      _memPoolState = esLState (nesEs (createInitialState (pConfig params)))
    }

applyTx ::
  Params ->
  EmulatedLedgerState ->
  Core.Tx EmulatorEra ->
  Either (ApplyTxError EmulatorEra) (EmulatedLedgerState, Validated (Core.Tx EmulatorEra))
applyTx params oldState@EmulatedLedgerState {_ledgerEnv, _memPoolState} tx = do
  (newMempool, vtx) <- C.Ledger.applyTx (emulatorGlobals params) _ledgerEnv _memPoolState tx
  return (oldState & memPoolState .~ newMempool, vtx)

hasValidationErrors ::
  Params ->
  EmulatedLedgerState ->
  C.Tx C.ConwayEra ->
  (Maybe EmulatedLedgerState, P.ValidationResult)
hasValidationErrors params ls tx =
  case res of
    Left err -> (Nothing, P.FailPhase1 (CardanoEmulatorEraTx tx) err)
    Right (ls', vtx) -> case getTxExUnitsWithLogs params utxo tx of
      Left (P.Phase1, err) -> (Just ls', P.FailPhase1 (CardanoEmulatorEraTx tx) err)
      Left (P.Phase2, err) ->
        (Just ls', P.FailPhase2 vtx err $ getCollateral (P.toPlutusIndex utxo) (CardanoEmulatorEraTx tx))
      Right report -> (Just ls', P.Success vtx report)
  where
    utxo = getUtxo ls
    res =
      bimap
        (P.CardanoLedgerValidationError . Text.pack . show)
        (fmap OnChainTx)
        (validateAndApplyTx params ls tx)

validateAndApplyTx ::
  Params ->
  EmulatedLedgerState ->
  C.Tx C.ConwayEra ->
  Either (ApplyTxError EmulatorEra) (EmulatedLedgerState, Validated (Core.Tx EmulatorEra))
validateAndApplyTx params ledgerState (C.ShelleyTx _ tx) = res
  where
    memPool = _memPoolState ledgerState
    res = do
      vtx <-
        constructValidated
          (emulatorGlobals params)
          (C.Ledger.UtxoEnv (getSlot ledgerState) (emulatorPParams params) (C.Ledger.lsCertState memPool))
          (lsUTxOState memPool)
          tx
      applyTx params ledgerState vtx

-- | Construct a 'AlonzoTx' from a 'Core.Tx' by setting the `IsValid`
-- flag.
--
-- Note that this simply constructs the transaction; it does not validate
-- anything other than the scripts. Thus the resulting transaction may be
-- completely invalid.
--
-- Copied from cardano-ledger as it was removed there
-- in https://github.com/input-output-hk/cardano-ledger/commit/721adb55b39885847562437a6fe7e998f8e48c03
constructValidated ::
  forall m.
  (MonadError (ApplyTxError EmulatorEra) m) =>
  Globals ->
  C.Ledger.UtxoEnv EmulatorEra ->
  C.Ledger.UTxOState EmulatorEra ->
  Core.Tx EmulatorEra ->
  m (AlonzoTx EmulatorEra)
constructValidated globals (C.Ledger.UtxoEnv _ pp _) st tx =
  case collectPlutusScriptsWithContext ei sysS pp tx utxo of
    Left errs ->
      throwError
        ( ApplyTxError
            ( ConwayUtxowFailure
                (Core.injectFailure (UtxosFailure (Core.injectFailure $ CollectErrors errs)))
                :| []
            )
        )
    Right sLst ->
      let scriptEvalResult = evalPlutusScripts sLst
          vTx =
            AlonzoTx
              (view Core.bodyTxL tx)
              (view Core.witsTxL tx)
              (IsValid (lift scriptEvalResult))
              (view Core.auxDataTxL tx)
       in pure vTx
  where
    utxo = utxosUtxo st
    sysS = systemStart globals
    ei = epochInfo globals
    lift (Passes _) = True
    lift (Fails _ _) = False

unsafeMakeValid :: CardanoTx -> OnChainTx
unsafeMakeValid (CardanoEmulatorEraTx (C.Tx txBody _)) =
  let C.ShelleyTxBody _ txBody' _ _ _ _ = txBody
      vtx :: Core.Tx EmulatorEra = AlonzoTx txBody' mempty (IsValid True) C.Ledger.SNothing
   in OnChainTx $ unsafeMakeValidated vtx

validateCardanoTx ::
  Params ->
  EmulatedLedgerState ->
  CardanoTx ->
  (Maybe EmulatedLedgerState, P.ValidationResult)
validateCardanoTx params ls ctx@(CardanoEmulatorEraTx tx) =
  if map fst (C.txIns $ C.getTxBodyContent $ C.getTxBody tx) == [genesisTxIn]
    then (Just ls, P.Success (unsafeMakeValid ctx) Map.empty)
    else hasValidationErrors params ls tx

getTxExUnitsWithLogs ::
  Params -> UTxO EmulatorEra -> C.Tx C.ConwayEra -> Either P.ValidationErrorInPhase P.RedeemerReport
getTxExUnitsWithLogs params utxo (C.ShelleyTx _ tx) =
  traverse (either toCardanoLedgerError Right) result
  where
    eg = emulatorGlobals params
    ss = systemStart eg
    ei = epochInfo eg
    result = evalTxExUnitsWithLogs (emulatorPParams params) tx utxo ei ss
    toCardanoLedgerError (ValidationFailure _ (V1.CekError ce) logs _) =
      Left (P.Phase2, P.ScriptFailure (P.EvaluationError logs ("CekEvaluationFailure: " ++ show ce)))
    toCardanoLedgerError e = Left (P.Phase2, P.CardanoLedgerValidationError $ Text.pack $ show e)

createAndValidateTransactionBody ::
  Params ->
  P.CardanoBuildTx ->
  Either CardanoLedgerError (C.TxBody C.ConwayEra)
createAndValidateTransactionBody params (P.CardanoBuildTx bodyContent) =
  let bodyContent' = bodyContent {C.txProtocolParams = C.BuildTxWith $ Just $ ledgerProtocolParameters params}
   in first (Right . P.TxBodyError . C.displayError) $
        C.createTransactionBody C.shelleyBasedEra bodyContent'
