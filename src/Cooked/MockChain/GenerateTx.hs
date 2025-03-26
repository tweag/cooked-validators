module Cooked.MockChain.GenerateTx
  ( txSignersAndBodyToCardanoTx,
    txSkelToCardanoTx,
  )
where

import Cardano.Api.Shelley qualified as Cardano
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Body
import Cooked.Skeleton
import Cooked.Wallet
import Data.Set (Set)
import Ledger.Address qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api

-- | Generates a Cardano transaction and signs it
txSignersAndBodyToCardanoTx :: [Wallet] -> Cardano.TxBody Cardano.ConwayEra -> Cardano.Tx Cardano.ConwayEra
txSignersAndBodyToCardanoTx signers txBody =
  Ledger.getEmulatorEraTx $
    foldl
      (flip Ledger.addCardanoTxWitness)
      (Ledger.CardanoEmulatorEraTx $ txBody `Cardano.Tx` [])
      (Ledger.toWitness . Ledger.PaymentPrivateKey . walletSK <$> signers)

-- | Generates a full Cardano transaction for a skeleton, fees and collaterals
txSkelToCardanoTx :: (MonadBlockChainBalancing m) => TxSkel -> Integer -> Maybe (Set Api.TxOutRef, Wallet) -> m (Cardano.Tx Cardano.ConwayEra)
txSkelToCardanoTx txSkel fee mCollaterals =
  txSignersAndBodyToCardanoTx (txSkelSigners txSkel) <$> txSkelToTxBody txSkel fee mCollaterals
