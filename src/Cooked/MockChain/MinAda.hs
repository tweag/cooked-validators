-- | This module provides functions to ensure skeleton outputs contain enough
-- ada to satisfy the minimum ada constraint.
module Cooked.MockChain.MinAda
  ( toTxSkelOutWithMinAda,
    toTxSkelWithMinAda,
    getTxSkelOutMinAda,
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
import Cooked.Skeleton
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Value qualified as Script

-- | This provides the minimum amount of ada required in a given `TxSkelOut`. As
-- we need to transform our output into a Cardano output to compute this value,
-- this function can fail.
getTxSkelOutMinAda :: Emulator.Params -> TxSkelOut -> Either GenerateTxError Integer
getTxSkelOutMinAda Emulator.Params {..} txSkelOut =
  Cardano.unCoin
    . Shelley.getMinCoinTxOut pEmulatorPParams
    . Cardano.toShelleyTxOut Cardano.ShelleyBasedEraConway
    . Cardano.toCtxUTxOTxOut
    <$> generateTxOut pNetworkId txSkelOut

-- | This transforms an output into another output which necessarily contains at
-- least the minimal required ada. If the previous quantity of ada was
-- sufficient, it remains unchanged. This requires an iterative process, as
-- adding ada into an output can potentially increase its size and thus make it
-- require more minimal ada (although this remains to be witnessed in practice).
-- This approach was inspired by
-- https://github.com/input-output-hk/plutus-apps/blob/8706e6c7c525b4973a7b6d2ed7c9d0ef9cd4ef46/plutus-ledger/src/Ledger/Index.hs#L124
-- This transformation is option and piloted by the 'txSkelOutValueAutoAdjust'
-- flag from the `TxSkelOutValue`.
toTxSkelOutWithMinAda :: Emulator.Params -> TxSkelOut -> Either GenerateTxError TxSkelOut
toTxSkelOutWithMinAda _ txSkelOut | TxSkelOutValue _ False <- txSkelOut ^. txSkelOutValueL = return txSkelOut
toTxSkelOutWithMinAda params txSkelOut | TxSkelOutValue ((^. Script.adaL) -> Script.Lovelace oldAda) _ <- txSkelOut ^. txSkelOutValueL = do
  requiredAda <- getTxSkelOutMinAda params txSkelOut
  if oldAda < requiredAda
    then toTxSkelOutWithMinAda params $ txSkelOut & txSkelOutValueL % txSkelOutValueContentL % Script.adaL .~ Script.Lovelace requiredAda
    else return txSkelOut

-- | This goes through all the `TxSkelOut`s of the given skeleton and update
-- their ada value when requested by the user and required by the protocol
-- parameters. This requires to transform the outputs into their Cardano
-- counterpart, which can cause some errors that are transformed into
-- `MCEGenerationError`. 
toTxSkelWithMinAda :: (MonadBlockChainBalancing m) => TxSkel -> m TxSkel
toTxSkelWithMinAda skel = do
  theParams <- getParams
  case mapM (toTxSkelOutWithMinAda theParams) $ skel ^. txSkelOutsL of
    Left err -> throwError $ MCEGenerationError err
    Right newTxSkelOuts -> return $ skel & txSkelOutsL .~ newTxSkelOuts
