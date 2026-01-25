{-# LANGUAGE TemplateHaskell #-}

module Cooked.MockChain.Log
  ( -- * Logging events
    MockChainLogEntry (..),

    -- * Logging effect
    MockChainLog,
    runMockChainLog,

    -- * Logging primitive
    logEvent,
  )
where

import Cooked.MockChain.Common
import Cooked.Skeleton
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Writer

-- | Events logged when processing transaction skeletons
data MockChainLogEntry
  = -- | Logging a Skeleton as it is submitted by the user.
    MCLogSubmittedTxSkel TxSkel
  | -- | Logging a Skeleton as it has been adjusted by the balancing mechanism,
    -- alongside fee, and possible collateral utxos and return collateral user.
    MCLogAdjustedTxSkel TxSkel Fee Collaterals
  | -- | Logging the successful validation of a new transaction, with its id and
    -- number of produced outputs.
    MCLogNewTx Api.TxId Integer
  | -- | Logging the fact that utxos provided by the user for balancing have to be
    -- discarded for a specific reason.
    MCLogDiscardedUtxos Integer String
  | -- | Logging the fact that utxos provided as collaterals will not be used
    -- because the transaction does not involve scripts. There are 2 cases,
    -- depending on whether the user has provided an explicit user or a set of
    -- utxos to be used as collaterals.
    MCLogUnusedCollaterals (Either Peer CollateralIns)
  | -- | Logging the automatic addition of a reference script
    MCLogAddedReferenceScript TxSkelRedeemer Api.TxOutRef Script.ScriptHash
  | -- | Logging the automatic addition of a withdrawal amount
    MCLogAutoFilledWithdrawalAmount Api.Credential Api.Lovelace
  | -- | Logging the automatic addition of the constitution script
    MCLogAutoFilledConstitution Api.ScriptHash
  | -- | Logging the automatic adjustment of a min ada amount
    MCLogAdjustedTxSkelOut TxSkelOut Api.Lovelace
  deriving (Show)

-- | An effect to allow logging of mockchain events
data MockChainLog :: Effect where
  LogEvent :: MockChainLogEntry -> MockChainLog m ()

makeSem_ ''MockChainLog

-- | Interpreting a `MockChainLog` in terms of a writer of
-- @[MockChainLogEntry]@
runMockChainLog ::
  (Member (Writer j) effs) =>
  (MockChainLogEntry -> j) ->
  Sem (MockChainLog : effs) a ->
  Sem effs a
runMockChainLog inject = interpret $ \(LogEvent event) -> tell $ inject event

-- | Logs an internal event occurring while processing a transaction skeleton
logEvent :: (Member MockChainLog effs) => MockChainLogEntry -> Sem effs ()
