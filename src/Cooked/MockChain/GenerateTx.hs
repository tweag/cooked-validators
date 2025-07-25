-- | This module exposes translation functions to transform a 'TxSkel' into a
-- signed transaction
module Cooked.MockChain.GenerateTx
  ( txSignersAndBodyToCardanoTx,
    txSkelToCardanoTx,
  )
where

import Cardano.Api qualified as Cardano
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Body
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Cooked.Wallet
import Data.Set (Set)
import PlutusLedgerApi.V3 qualified as Api

-- | Generates a Cardano transaction and signs it
txSignersAndBodyToCardanoTx :: [Wallet] -> Cardano.TxBody Cardano.ConwayEra -> Cardano.Tx Cardano.ConwayEra
txSignersAndBodyToCardanoTx signers txBody = Cardano.Tx txBody (toKeyWitness txBody <$> signers)

-- | Generates a full Cardano transaction from a skeleton, fees and collaterals
txSkelToCardanoTx :: (MonadBlockChainBalancing m) => TxSkel -> Integer -> Maybe (Set Api.TxOutRef, Wallet) -> m (Cardano.Tx Cardano.ConwayEra)
txSkelToCardanoTx txSkel fee mCollaterals =
  txSignersAndBodyToCardanoTx (txSkelSigners txSkel) <$> txSkelToTxBody txSkel fee mCollaterals
