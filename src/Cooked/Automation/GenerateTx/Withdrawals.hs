-- | This modules exposes the generation of withdrawals
module Cooked.Automation.GenerateTx.Withdrawals (toWithdrawals) where

import Cardano.Api qualified as Cardano
import Control.Monad
import Cooked.Automation.GenerateTx.Witness
import Cooked.Effect.Read.Chain
import Cooked.Effect.Read.Conf
import Cooked.Runtime.Error
import Cooked.Skeleton.User
import Cooked.Skeleton.Withdrawal
import Data.Coerce
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import Polysemy
import Polysemy.Error

-- | Takes a 'TxSkelWithdrawals' and transforms it into a 'Cardano.TxWithdrawals'
toWithdrawals ::
  (Members '[MockChainReadChain, MockChainReadConf, Error MockChainError, Error P.Ledger.ToCardanoError] effs) =>
  TxSkelWithdrawals ->
  Sem effs (Cardano.TxWithdrawals Cardano.BuildTx Cardano.ConwayEra)
toWithdrawals withdrawals | withdrawals == mempty = return Cardano.TxWithdrawalsNone
toWithdrawals (view txSkelWithdrawalsListI -> withdrawals) = do
  networkId <- getNetworkId
  cardanoWithdrawals <- forM withdrawals $ \(Withdrawal user amount) -> do
    let coinAmount = maybe (Cardano.Coin 0) coerce amount
    (sCred, witness) <- case user of
      UserPubKey (Script.toPubKeyHash -> pkh) -> do
        sCred <- fromEither $ Cardano.StakeCredentialByKey <$> P.Ledger.toCardanoStakeKeyHash pkh
        return (sCred, Cardano.KeyWitness Cardano.KeyWitnessForStakeAddr)
      UserRedeemedScript (toVScript -> vScript) red -> do
        witness <-
          Cardano.ScriptWitness Cardano.ScriptWitnessForStakeAddr
            <$> toScriptWitness vScript red Cardano.NoScriptDatumForStake
        sCred <- fromEither $ Cardano.StakeCredentialByScript <$> P.Ledger.toCardanoScriptHash (Script.toScriptHash vScript)
        return (sCred, witness)
    return (Cardano.makeStakeAddress networkId sCred, coinAmount, Cardano.BuildTxWith witness)
  return $ Cardano.TxWithdrawals Cardano.ShelleyBasedEraConway cardanoWithdrawals
