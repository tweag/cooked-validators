-- | This modules exposes the generation of withdrawals
module Cooked.MockChain.GenerateTx.Withdrawals (toWithdrawals) where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Data.Coerce
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api

-- | Takes a 'TxSkelWithdrawals' and transforms it into a 'Cardano.TxWithdrawals'
toWithdrawals :: (MonadBlockChainBalancing m) => TxSkelWithdrawals -> m (Cardano.TxWithdrawals Cardano.BuildTx Cardano.ConwayEra)
toWithdrawals withdrawals | withdrawals == mempty = return Cardano.TxWithdrawalsNone
toWithdrawals (view txSkelWithdrawalsListI -> withdrawals) = do
  networkId <- Emulator.pNetworkId <$> getParams
  cardanoWithdrawals <- forM withdrawals $ \case
    Withdrawal (UserPubKey (Script.toPubKeyHash -> pkh)) amount -> do
      sCred <-
        throwOnToCardanoError "toWithdrawals: unable to translate pkh stake credential" $
          Cardano.StakeCredentialByKey <$> Ledger.toCardanoStakeKeyHash pkh
      return (Cardano.makeStakeAddress networkId sCred, coerce amount, Cardano.BuildTxWith $ Cardano.KeyWitness Cardano.KeyWitnessForStakeAddr)
    Withdrawal (UserRedeemedScript (toVScript -> vScript) red) amount -> do
      witness <-
        Cardano.ScriptWitness Cardano.ScriptWitnessForStakeAddr
          <$> toScriptWitness vScript red Cardano.NoScriptDatumForStake
      sCred <-
        throwOnToCardanoError "toWithdrawals: unable to translate script stake credential" $
          Cardano.StakeCredentialByScript <$> Ledger.toCardanoScriptHash (Script.toScriptHash vScript)
      return (Cardano.makeStakeAddress networkId sCred, coerce amount, Cardano.BuildTxWith witness)
  return $ Cardano.TxWithdrawals Cardano.ShelleyBasedEraConway cardanoWithdrawals
