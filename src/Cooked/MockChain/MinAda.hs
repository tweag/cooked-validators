module Cooked.MockChain.MinAda
  ( toTxSkelOutMinAda,
    toTxSkelMinAda,
    ensureTxSkelMinAda,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Shelley.Core qualified as Shelley
import Cardano.Node.Emulator qualified as Emulator
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Control.Monad.Except
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.Output
import Cooked.Skeleton
import Cooked.ValueUtils
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script

ensureTxSkelMinAda :: (MonadBlockChainBalancing m) => TxSkel -> m TxSkel
ensureTxSkelMinAda skel =
  if txOptEnsureMinAda . txSkelOpts $ skel
    then toTxSkelMinAda skel
    else return skel

-- | To that the transaction outputs have the necessary minimum amount of
-- Ada on them. This will only be applied if the 'txOptToMinAda' is set to
-- @True@.
toTxSkelMinAda :: (MonadBlockChainBalancing m) => TxSkel -> m TxSkel
toTxSkelMinAda skel = do
  theParams <- getParams
  case mapM (toTxSkelOutMinAda theParams) $ skel ^. txSkelOutsL of
    Left err -> throwError $ MCEGenerationError err
    Right newTxSkelOuts -> return $ skel & txSkelOutsL .~ newTxSkelOuts

toTxSkelOutMinAda :: Emulator.Params -> TxSkelOut -> Either GenerateTxError TxSkelOut
toTxSkelOutMinAda theParams txSkelOut@(Pays output) = do
  cardanoTxOut <- generateTxOut (Emulator.pNetworkId theParams) txSkelOut
  let Script.Lovelace oldAda = output ^. outputValueL % adaL
      Cardano.Coin requiredAda =
        Shelley.getMinCoinTxOut
          (Emulator.emulatorPParams theParams)
          . Cardano.toShelleyTxOut Cardano.ShelleyBasedEraConway
          . Cardano.toCtxUTxOTxOut
          $ cardanoTxOut
      updatedTxSkelOut = Pays $ output & outputValueL % adaL .~ Script.Lovelace (max oldAda requiredAda)
  -- The following iterative approach to calculate the minimum Ada amount of
  -- a TxOut is necessary, because the additional value might make the TxOut
  -- heavier.
  --
  -- It is inspired by
  -- https://github.com/input-output-hk/plutus-apps/blob/8706e6c7c525b4973a7b6d2ed7c9d0ef9cd4ef46/plutus-ledger/src/Ledger/Index.hs#L124
  if oldAda < requiredAda
    then toTxSkelOutMinAda theParams updatedTxSkelOut
    else return txSkelOut
