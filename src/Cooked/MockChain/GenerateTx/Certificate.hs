-- | This module provide primitives to transform certificates from our skeleton
-- to certificate in Cardano transaction bodies.
module Cooked.MockChain.GenerateTx.Certificate (toCertificates) where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Conway.TxCert qualified as Conway
import Cardano.Ledger.DRep qualified as Ledger
import Cardano.Ledger.PoolParams qualified as Ledger
import Cardano.Ledger.Shelley.TxCert qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton.Certificate
import Cooked.Skeleton.Scripts
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
  do
    depositStake <- Cardano.Coin . Api.getLovelace <$> stakeAddressDeposit
    depositDRep <- Cardano.Coin . Api.getLovelace <$> dRepDeposit
    Cardano.ConwayCertificate Cardano.ConwayEraOnwardsConway <$> case txSkelCert of
      TxSkelCertificate (Script.toCredential -> cred) StakingRegister ->
        Conway.ConwayTxCertDeleg . (`Conway.ConwayRegCert` SJust depositStake) <$> toStakeCredential cred
      TxSkelCertificate (Script.toCredential -> cred) StakingUnRegister ->
        Conway.ConwayTxCertDeleg . (`Conway.ConwayUnRegCert` SJust depositStake) <$> toStakeCredential cred
      TxSkelCertificate (Script.toCredential -> cred) (StakingDelegate delegatee) ->
        Conway.ConwayTxCertDeleg <$> liftA2 Conway.ConwayDelegCert (toStakeCredential cred) (toDelegatee delegatee)
      TxSkelCertificate (Script.toCredential -> cred) (StakingRegisterDelegate delegatee) ->
        Conway.ConwayTxCertDeleg . (depositStake &) <$> liftA2 Conway.ConwayRegDelegCert (toStakeCredential cred) (toDelegatee delegatee)
      TxSkelCertificate (Script.toCredential -> cred) DRepRegister ->
        Conway.ConwayTxCertGov . (\c -> Conway.ConwayRegDRep c depositDRep SNothing) <$> toDRepCredential cred
      TxSkelCertificate (Script.toCredential -> cred) DRepUpdate ->
        Conway.ConwayTxCertGov . (`Conway.ConwayUpdateDRep` SNothing) <$> toDRepCredential cred
      TxSkelCertificate (Script.toCredential -> cred) DRepUnRegister ->
        Conway.ConwayTxCertGov . (`Conway.ConwayUnRegDRep` depositDRep) <$> toDRepCredential cred
      -- TODO: For now, when registering a new pool we use the default parameters
      -- excepct for the pool id and pool vrf. We could change it later on.
      TxSkelCertificate (UserPubKeyHash (Script.toPubKeyHash -> poolHash)) (PoolRegister poolVrf) ->
        Conway.ConwayTxCertPool . Shelley.RegPool
          <$> liftA2
            (\pId pVrf -> def {Ledger.ppId = pId, Ledger.ppVrf = pVrf})
            (toStakePoolKeyHash poolHash)
            (toVRFVerKeyHash poolVrf)
      TxSkelCertificate (UserPubKeyHash (Script.toPubKeyHash -> poolHash)) (PoolRetire slot) ->
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
      TxSkelCertificate (Script.toCredential -> coldCred) (CommitteeRegisterHot hotCred) ->
        Conway.ConwayTxCertGov <$> liftA2 Conway.ConwayAuthCommitteeHotKey (toColdCredential coldCred) (toHotCredential hotCred)
      TxSkelCertificate (Script.toCredential -> cred) CommitteeResign ->
        Conway.ConwayTxCertGov . (`Conway.ConwayResignCommitteeColdKey` SNothing) <$> toColdCredential cred

toCertificateWitness :: (MonadBlockChainBalancing m) => TxSkelCertificate -> m (Maybe (Cardano.ScriptWitness Cardano.WitCtxStake Cardano.ConwayEra))
toCertificateWitness =
  maybe
    (return Nothing)
    (\(UserRedeemedScript s red) -> Just <$> toScriptWitness s red Cardano.NoScriptDatumForStake)
    . preview (txSkelCertificateOwnerAT @IsScript)

-- | Builds a 'Cardano.TxCertificates' from a list of 'TxSkelCertificate'
toCertificates :: (MonadBlockChainBalancing m) => [TxSkelCertificate] -> m (Cardano.TxCertificates Cardano.BuildTx Cardano.ConwayEra)
toCertificates =
  fmap (Cardano.mkTxCertificates Cardano.ShelleyBasedEraConway)
    . mapM (\txSkelCert -> liftA2 (,) (toCertificate txSkelCert) (toCertificateWitness txSkelCert))
