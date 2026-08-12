{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes primitives required to log internal pieces of
-- information during a mockchain run. This includes, in particular, all the
-- adjustment automatically done by \cooked-validators\ during the transaction
-- processing phase. This effect is typically not available to users, and should
-- solely be used to track internal events. To trace additional elements from a
-- user's perspective, use `Cooked.Effect.Misc.note` instead.
module Cooked.Effect.Log
  ( -- * Logging events
    TxValidity (..),
    MockChainLogEntry (..),

    -- * Logging effect
    MockChainLog,
    runMockChainLog,

    -- * Logging primitive
    logEvent,
  )
where

import Cooked.Common
import Cooked.Skeleton
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Writer

-- | The validity of a transaction
data TxValidity
  = -- | The transaction is valid, we store the number of inputs and outputs
    Valid Int Int
  | -- | The transaction is invalid in phase 1 (no ledger change)
    InvalidPhase1
  | -- | The transaction is invalid in phase 2, we store the number of collateral
    -- inputs and return collateral outputs
    InvalidPhase2 Int Int
  deriving (Show)

-- | Events logged when processing transaction skeletons
data MockChainLogEntry
  = -- | Logging a Skeleton as it is submitted by the user.
    MCLogSubmittedTxSkel TxSkel
  | -- | Logging a Skeleton as it has been adjusted by the balancing mechanism,
    -- alongside fee, and possible collateral utxos and return collateral user.
    MCLogAdjustedTxSkel TxSkel Fee (Maybe Collaterals)
  | -- | Logging the production of a new transaction, with its ID as well as its
    -- validity.
    MCLogNewTx Api.TxId TxValidity
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
  | -- | Logging the existence of failures uncovered during the computation of
    -- execution units, when they're not treated as fatal.
    MCELogExUnitsFailures ExUnitsFailures
  | -- | Logging the existence of failures uncovered during submission, when
    -- they're not treated as fatal.
    MCELogSubmissionFailures SubmissionFailures
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
