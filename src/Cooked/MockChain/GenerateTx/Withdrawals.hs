module Cooked.MockChain.GenerateTx.Withdrawals
  ( WithdrawalsContext (..),
    toWithdrawals,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Control.Monad
import Control.Monad.Reader
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api

data WithdrawalsContext where
  WithdrawalsContext ::
    { managedTxOuts :: Map Api.TxOutRef Api.TxOut,
      networkId :: Cardano.NetworkId
    } ->
    WithdrawalsContext

instance Transform WithdrawalsContext (Map Api.TxOutRef Api.TxOut) where
  transform = managedTxOuts

type WithdrawalsGen a = TxGen WithdrawalsContext a

toWithdrawals :: TxSkelWithdrawals -> WithdrawalsGen (Cardano.TxWithdrawals Cardano.BuildTx Cardano.ConwayEra)
toWithdrawals (Map.toList -> []) = return Cardano.TxWithdrawalsNone
toWithdrawals (Map.toList -> withdrawals) =
  fmap
    (Cardano.TxWithdrawals Cardano.ShelleyBasedEraConway)
    $ forM withdrawals
    $ \(staker, (red, Script.Lovelace n)) ->
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
                  <$> liftTxGen (toScriptWitness script red Cardano.NoScriptDatumForStake)
              sCred <-
                throwOnToCardanoError "toWithdrawals: unable to translate script stake credential" $
                  Cardano.StakeCredentialByScript <$> Ledger.toCardanoScriptHash (Script.toScriptHash script)
              return (witness, sCred)
        networkId <- asks networkId
        return (Cardano.makeStakeAddress networkId sCred, Cardano.Coin n, Cardano.BuildTxWith witness)
