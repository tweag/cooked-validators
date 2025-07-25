module Cooked.MockChain.GenerateTx.Certificate (toCertificates) where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Conway.TxCert qualified as Conway
import Cardano.Ledger.DRep qualified as Ledger
import Cardano.Ledger.PoolParams qualified as Ledger
import Cardano.Ledger.Shelley.TxCert qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton.Certificate
import Cooked.Skeleton.Redeemer
import Data.Coerce
import Data.Default
import Data.Maybe.Strict
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api

toDRep :: (MonadBlockChainBalancing m) => Api.DRep -> m Ledger.DRep
toDRep Api.DRepAlwaysAbstain = return Ledger.DRepAlwaysAbstain
toDRep Api.DRepAlwaysNoConfidence = return Ledger.DRepAlwaysNoConfidence
toDRep (Api.DRep (Api.DRepCredential cred)) = Ledger.DRepCredential <$> toDRepCredential cred

toDelegatee :: (MonadBlockChainBalancing m) => Api.Delegatee -> m Conway.Delegatee
toDelegatee (Api.DelegStake pkh) = Conway.DelegStake <$> toStakePoolKeyHash pkh
toDelegatee (Api.DelegVote dRep) = Conway.DelegVote <$> toDRep dRep
toDelegatee (Api.DelegStakeVote pkh dRep) = liftA2 Conway.DelegStakeVote (toStakePoolKeyHash pkh) (toDRep dRep)

toCertificate :: (MonadBlockChainBalancing m) => TxSkelCertificate -> m (Cardano.Certificate Cardano.ConwayEra)
toCertificate txSkelCert =
  Cardano.ConwayCertificate Cardano.ConwayEraOnwardsConway <$> case txSkelCert of
    TxSkelCertificate (Script.toCredential -> cred) (getSDeposit -> sDep) StakingRegister ->
      Conway.ConwayTxCertDeleg . (`Conway.ConwayRegCert` sDep) <$> toStakeCredential cred
    TxSkelCertificate (Script.toCredential -> cred) (getSDeposit -> sDep) StakingUnRegister ->
      Conway.ConwayTxCertDeleg . (`Conway.ConwayUnRegCert` sDep) <$> toStakeCredential cred
    TxSkelCertificate (Script.toCredential -> cred) EmptyDeposit (StakingDelegate delegatee) ->
      Conway.ConwayTxCertDeleg <$> liftA2 Conway.ConwayDelegCert (toStakeCredential cred) (toDelegatee delegatee)
    TxSkelCertificate (Script.toCredential -> cred) (SomeDeposit (getCDeposit -> cDep)) (StakingRegisterDelegate delegatee) ->
      Conway.ConwayTxCertDeleg . (cDep &) <$> liftA2 Conway.ConwayRegDelegCert (toStakeCredential cred) (toDelegatee delegatee)
    TxSkelCertificate (Script.toCredential -> cred) (SomeDeposit (getCDeposit -> cDep)) DRepRegister ->
      Conway.ConwayTxCertGov . (\c -> Conway.ConwayRegDRep c cDep SNothing) <$> toDRepCredential cred
    TxSkelCertificate (Script.toCredential -> cred) EmptyDeposit DRepUpdate ->
      Conway.ConwayTxCertGov . (`Conway.ConwayUpdateDRep` SNothing) <$> toDRepCredential cred
    TxSkelCertificate (Script.toCredential -> cred) (SomeDeposit (getCDeposit -> cDep)) DRepUnRegister ->
      Conway.ConwayTxCertGov . (`Conway.ConwayUnRegDRep` cDep) <$> toDRepCredential cred
    -- TODO: For now, when registering a new pool we use the default parameters
    -- excepct for the pool id and pool vrf. We could change it later on.
    TxSkelCertificate (PubKeyOwner poolHash) EmptyDeposit (PoolRegister poolVrf) ->
      Conway.ConwayTxCertPool . Shelley.RegPool
        <$> liftA2
          (\pId pVrf -> def {Ledger.ppId = pId, Ledger.ppVrf = pVrf})
          (toStakePoolKeyHash poolHash)
          (toVRFVerKeyHash poolVrf)
    TxSkelCertificate (PubKeyOwner poolHash) EmptyDeposit (PoolRetire slot) ->
      Conway.ConwayTxCertPool
        <$> liftA2
          Shelley.RetirePool
          (toStakePoolKeyHash poolHash)
          ( do
              eeh <- Emulator.emulatorEraHistory <$> getParams
              case Cardano.slotToEpoch (fromIntegral slot) eeh of
                -- TODO: we could have a dedicated error for this case if the
                -- can occur at several places in the codebase
                Left _ -> fail "Too far away in the future"
                Right (epoch, _, _) -> return epoch
          )
    TxSkelCertificate (Script.toCredential -> coldCred) EmptyDeposit (CommitteeRegisterHot hotCred) ->
      Conway.ConwayTxCertGov <$> liftA2 Conway.ConwayAuthCommitteeHotKey (toColdCredential coldCred) (toHotCredential hotCred)
    TxSkelCertificate (Script.toCredential -> cred) EmptyDeposit CommitteeResign ->
      Conway.ConwayTxCertGov . (`Conway.ConwayResignCommitteeColdKey` SNothing) <$> toColdCredential cred
  where
    getSDeposit :: Deposit a -> StrictMaybe Cardano.Coin
    getSDeposit EmptyDeposit = SNothing
    getSDeposit (SomeDeposit lv) = SJust $ getCDeposit lv

    getCDeposit :: Api.Lovelace -> Cardano.Coin
    getCDeposit = coerce

toCertificateWitness :: (MonadBlockChainBalancing m) => TxSkelCertificate -> m (Maybe (Cardano.ScriptWitness Cardano.WitCtxStake Cardano.ConwayEra))
toCertificateWitness =
  maybe
    (return Nothing)
    (\(RedeemedScript s red) -> Just <$> toScriptWitness s red Cardano.NoScriptDatumForStake)
    . preview txSkelCertificateRedeemedScriptAT

-- | Builds a 'Cardano.TxCertificates' from a list of 'TxSkelCertificate'
toCertificates :: (MonadBlockChainBalancing m) => [TxSkelCertificate] -> m (Cardano.TxCertificates Cardano.BuildTx Cardano.ConwayEra)
toCertificates txSkelCerts = do
  Cardano.mkTxCertificates Cardano.ShelleyBasedEraConway
    <$> forM
      txSkelCerts
      ( \txSkelCert -> do
          cCert <- toCertificate txSkelCert
          mWit <- toCertificateWitness txSkelCert
          return (cCert, mWit)
      )
