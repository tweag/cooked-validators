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
import Control.Monad
import Control.Monad.Except
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.Skeleton
import Data.Maybe
import Optics.Core
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
toTxSkelOutWithMinAda ::
  -- | Current blockchain parameters
  Emulator.Params ->
  -- | The output to potential adjust
  TxSkelOut ->
  -- | Returns @Nothing@ when no ajustment was required/done, and
  -- @Just(newOutput,newAdaAmount)@ otherwise.
  Either GenerateTxError (Maybe (TxSkelOut, Integer))
-- The auto adjustment is disabled so nothing is done here
toTxSkelOutWithMinAda _ ((^. txSkelOutValueL % txSkelOutValueAutoAdjustL) -> False) = return Nothing
-- The auto adjustment is enabled
toTxSkelOutWithMinAda params txSkelOut = do
  -- Computing the required minimal amount of ADA in this output
  requiredAda <- getTxSkelOutMinAda params txSkelOut
  -- If this amount is sufficient, we return Nothing, otherwise, we adjust the
  -- output and possibly iterate
  if Script.getLovelace (txSkelOut ^. txSkelOutValueL % txSkelOutValueContentL % Script.adaL) >= requiredAda
    then return Nothing
    else
      let newTxSkelOut = txSkelOut & txSkelOutValueL % txSkelOutValueContentL % Script.adaL .~ Script.Lovelace requiredAda
       in Just . fromMaybe (newTxSkelOut, requiredAda) <$> toTxSkelOutWithMinAda params newTxSkelOut

-- | This goes through all the `TxSkelOut`s of the given skeleton and updates
-- their ada value when requested by the user and required by the protocol
-- parameters.
toTxSkelWithMinAda :: (MonadBlockChainBalancing m) => TxSkel -> m TxSkel
toTxSkelWithMinAda skel = do
  theParams <- getParams
  (\x -> skel & txSkelOutsL .~ x)
    <$> forM
      (skel ^. txSkelOutsL)
      ( \txSkelOut -> case toTxSkelOutWithMinAda theParams txSkelOut of
          -- A generation error was raised, which we propagate
          Left err -> throwError $ MCEGenerationError err
          -- No adjustment is required/necessary
          Right Nothing -> return txSkelOut
          -- Adjustment is logged and performed
          Right (Just (newTxSkelOut, newAdaAmount)) -> do
            logEvent $ MCLogAdjustedTxSkelOut txSkelOut (Script.Lovelace newAdaAmount)
            return newTxSkelOut
      )
