-- | This modules exposes the generation of withdrawals
module Cooked.MockChain.GenerateTx.Withdrawals (toWithdrawals) where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Data.Map qualified as Map
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api

-- | Takes a 'TxSkelWithdrawals' and transforms it into a 'Cardano.TxWithdrawals'
toWithdrawals :: (MonadBlockChainBalancing m) => TxSkelWithdrawals -> m (Cardano.TxWithdrawals Cardano.BuildTx Cardano.ConwayEra)
toWithdrawals (Map.toList -> []) = return Cardano.TxWithdrawalsNone
toWithdrawals (Map.toList -> withdrawals) =
  fmap
    (Cardano.TxWithdrawals Cardano.ShelleyBasedEraConway)
    $ forM withdrawals
    $ \(staker, (red, Api.Lovelace n)) ->
      do
        (witness, sCred) <-
          case staker of
            Right pkh -> do
              sCred <-
                throwOnToCardanoError "toWithdrawals: unable to translate pkh stake credential" $
                  Cardano.StakeCredentialByKey <$> Ledger.toCardanoStakeKeyHash pkh
              return (Cardano.KeyWitness Cardano.KeyWitnessForStakeAddr, sCred)
            Left script -> do
              witness <-
                Cardano.ScriptWitness Cardano.ScriptWitnessForStakeAddr
                  <$> toScriptWitness script red Cardano.NoScriptDatumForStake
              sCred <-
                throwOnToCardanoError "toWithdrawals: unable to translate script stake credential" $
                  Cardano.StakeCredentialByScript <$> Ledger.toCardanoScriptHash (Script.toScriptHash script)
              return (witness, sCred)
        networkId <- Emulator.pNetworkId <$> getParams
        return (Cardano.makeStakeAddress networkId sCred, Cardano.Coin n, Cardano.BuildTxWith witness)
