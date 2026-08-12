-- | This module exposes internal, configuration-level primitives to query the
-- fixed configuration of the chain, such as its protocol parameters, network
-- id, era history and system start. These primitives are not meant to be used
-- directly when writing traces: they are an implementation detail backing the
-- user-facing 'Cooked.Effect.Read.Chain.MockChainReadChain' effect,
-- and they are deliberately not meant to be used directly through the "Cooked"
-- umbrella module.
module Cooked.Effect.Read.Conf
  ( -- * The 'MockChainReadConf' effect
    MockChainReadConf,

    -- * 'MockChainReadConf' interpreters
    runMockChainReadConf,
    runBlockChainReadConf,

    -- * Queries related to protocol parameters
    getParams,
    getNetworkId,
    govActionDeposit,
    dRepDeposit,
    stakeAddressDeposit,
    stakePoolDeposit,

    -- * Queries related to time configuration
    getEraHistory,
    getSystemStart,

    -- * Queries related to `Cooked.Skeleton.TxSkel` deposits
    txSkelDepositedValueInCertificates,
    txSkelDepositedValueInProposals,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Conway.Core qualified as Conway
import Cardano.Ledger.Core qualified as C.Ledger
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Cardano.Slotting.Time qualified as Time
import Control.Lens qualified as Lens
import Cooked.Runtime.State
import Cooked.Skeleton
import Data.Functor
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State

-- | An effect that offers primitives to query the fixed configuration of the
-- chain (protocol parameters, network id, era history and system start). As its
-- name suggests, this effect is read-only and does not alter the state in any
-- way. It is internal to the library and backs the user-facing
-- 'Cooked.Effect.Read.Chain.MockChainReadChain' effect.
data MockChainReadConf :: Effect where
  GetParams :: MockChainReadConf m (C.Ledger.PParams Conway.ConwayEra)
  GetNetworkId :: MockChainReadConf m Cardano.NetworkId
  GetEraHistory :: MockChainReadConf m Cardano.EraHistory
  GetSystemStart :: MockChainReadConf m Time.SystemStart

makeSem_ ''MockChainReadConf

-- | The interpretation for the configuration effect with a stored
-- 'EmulatorState'
runMockChainReadConf ::
  (Member (State EmulatorState) effs) =>
  Sem (MockChainReadConf : effs) a ->
  Sem effs a
runMockChainReadConf = interpret $ \case
  GetParams -> gets $ Emulator.pEmulatorPParams . emulatorStateParams
  GetNetworkId -> gets $ Emulator.pNetworkId . emulatorStateParams
  GetEraHistory -> gets $ Emulator.emulatorEraHistory . emulatorStateParams
  GetSystemStart -> gets $ Shelley.systemStart . Emulator.emulatorGlobals . emulatorStateParams

-- | Interpret the `MockChainReadConf` effect by talking to a deployed node
-- through a `Cardano.LocalNodeConnectInfo` (socket path and network id) provided
-- via a `Reader`, running in a stack featuring @IO@ (via `Embed`).
runBlockChainReadConf ::
  ( Members
      '[ Embed IO,
         Error Cardano.UnsupportedNtcVersionError,
         Error Cardano.EraMismatch,
         Error Cardano.AcquiringFailure,
         Reader Cardano.LocalNodeConnectInfo
       ]
      effs
  ) =>
  Sem (MockChainReadConf : effs) a ->
  Sem effs a
runBlockChainReadConf = interpret $ \case
  GetParams -> queryAndHandleErrors $ Cardano.queryProtocolParameters Cardano.ShelleyBasedEraConway
  GetNetworkId -> asks Cardano.localNodeNetworkId
  GetEraHistory -> queryAndHandleError Cardano.queryEraHistory
  GetSystemStart -> queryAndHandleError Cardano.querySystemStart
  where
    -- Fetches the local node info, embeds a query in IO and handles errors
    query q = do
      conn <- ask
      response <- embed $ Cardano.executeLocalStateQueryExpr conn Cardano.VolatileTip q
      fromEither response
    -- Handles one more layer of errors from the response of a query
    queryAndHandleError q = query q >>= fromEither
    -- Handles a second layer of error from the response of a query
    queryAndHandleErrors q = queryAndHandleError q >>= fromEither

-- | Returns the emulator parameters, including protocol parameters
getParams ::
  (Member MockChainReadConf effs) =>
  Sem effs (C.Ledger.PParams Conway.ConwayEra)

-- | Returns the network id of the current chain
getNetworkId ::
  (Member MockChainReadConf effs) =>
  Sem effs Cardano.NetworkId

-- | Returns the era history of the chain, which notably allows converting slots
-- into epochs (see 'Cardano.slotToEpoch').
getEraHistory ::
  (Member MockChainReadConf effs) =>
  Sem effs Cardano.EraHistory

-- | Returns the system start time of the chain, that is the UTC time at which
-- the first slot begins.
getSystemStart ::
  (Member MockChainReadConf effs) =>
  Sem effs Time.SystemStart

-- | Retrieves the required governance action deposit amount
govActionDeposit ::
  (Member MockChainReadConf effs) =>
  Sem effs Api.Lovelace
govActionDeposit =
  getParams
    <&> Api.Lovelace
      . Cardano.unCoin
      . Lens.view Conway.ppGovActionDepositL

-- | Retrieves the required drep deposit amount
dRepDeposit ::
  (Member MockChainReadConf effs) =>
  Sem effs Api.Lovelace
dRepDeposit =
  getParams
    <&> Api.Lovelace
      . Cardano.unCoin
      . Lens.view Conway.ppDRepDepositL

-- | Retrieves the required stake address deposit amount
stakeAddressDeposit ::
  (Member MockChainReadConf effs) =>
  Sem effs Api.Lovelace
stakeAddressDeposit =
  getParams
    <&> Api.Lovelace
      . Cardano.unCoin
      . Lens.view Conway.ppKeyDepositL

-- | Retrieves the required stake pool deposit amount
stakePoolDeposit ::
  (Member MockChainReadConf effs) =>
  Sem effs Api.Lovelace
stakePoolDeposit =
  getParams
    <&> Api.Lovelace
      . Cardano.unCoin
      . Lens.view Conway.ppPoolDepositL

-- | Retrieves the total amount of lovelace deposited in certificates in this
-- skeleton. Note that unregistering a staking address or a dRep lead to a
-- negative deposit (a withdrawal, in fact) which means this function can return
-- a negative amount of lovelace, which is intended. The deposited amounts are
-- dictated by the current protocol parameters, and computed as such.
txSkelDepositedValueInCertificates ::
  (Member MockChainReadConf effs) =>
  TxSkel ->
  Sem effs Api.Lovelace
txSkelDepositedValueInCertificates txSkel = do
  sDep <- stakeAddressDeposit
  dDep <- dRepDeposit
  pDep <- stakePoolDeposit
  return $
    foldOf
      ( txSkelCertificatesL
          % traversed
          % to
            ( \case
                TxSkelCertificate _ StakingRegister {} -> sDep
                TxSkelCertificate _ StakingRegisterDelegate {} -> sDep
                TxSkelCertificate _ StakingUnRegister {} -> -sDep
                TxSkelCertificate _ DRepRegister {} -> dDep
                TxSkelCertificate _ DRepUnRegister {} -> -dDep
                TxSkelCertificate _ PoolRegister {} -> pDep
                -- There is no special case for 'PoolRetire' because the deposit
                -- is given back to the reward account.
                _ -> Api.Lovelace 0
            )
      )
      txSkel

-- | Retrieves the total amount of lovelace deposited in proposals in this
-- skeleton (equal to `govActionDeposit` times the number of proposals)
txSkelDepositedValueInProposals ::
  (Member MockChainReadConf effs) =>
  TxSkel ->
  Sem effs Api.Lovelace
txSkelDepositedValueInProposals TxSkel {txSkelProposals} =
  govActionDeposit
    <&> Api.Lovelace
      . (toInteger (length txSkelProposals) *)
      . Api.getLovelace
