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
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Output
import Cooked.Skeleton
import Data.Maybe
import Optics.Core
import Plutus.Script.Utils.Value qualified as Script

-- | Compute the required minimal ADA for a given output
getTxSkelOutMinAda :: (MonadBlockChainBalancing m) => TxSkelOut -> m Integer
getTxSkelOutMinAda txSkelOut = do
  params <- Emulator.pEmulatorPParams <$> getParams
  Cardano.unCoin
    . Shelley.getMinCoinTxOut params
    . Cardano.toShelleyTxOut Cardano.ShelleyBasedEraConway
    . Cardano.toCtxUTxOTxOut
    <$> toCardanoTxOut txSkelOut

-- | This transforms an output into another output which necessarily contains at
-- least the minimal required ada. If the previous quantity of ada was
-- sufficient, it remains unchanged.
toTxSkelOutWithMinAda ::
  (MonadBlockChainBalancing m) =>
  -- | The output to potential adjust
  TxSkelOut ->
  -- | Returns @Nothing@ when no ajustment was required/done, and
  -- @Just(newOutput,newAdaAmount)@ otherwise.
  m TxSkelOut
-- The auto adjustment is disabled so nothing is done here
toTxSkelOutWithMinAda txSkelOut@((^. txSkelOutValueL % txSkelOutValueAutoAdjustL) -> False) = return txSkelOut
-- The auto adjustment is enabled
toTxSkelOutWithMinAda txSkelOut = do
  txSkelOut' <- go txSkelOut
  let originalAda = txSkelOutValue txSkelOut ^. Script.adaL
      updatedAda = txSkelOutValue txSkelOut' ^. Script.adaL
  when (originalAda /= updatedAda) $ logEvent $ MCLogAdjustedTxSkelOut txSkelOut updatedAda
  return txSkelOut'
  where
    go :: (MonadBlockChainBalancing m) => TxSkelOut -> m TxSkelOut
    go skelOut = do
      -- Computing the required minimal amount of ADA in this output
      requiredAda <- getTxSkelOutMinAda skelOut
      -- If this amount is sufficient, we return Nothing, otherwise, we adjust the
      -- output and possibly iterate
      if Script.getLovelace (skelOut ^. txSkelOutValueL % txSkelOutValueContentL % Script.adaL) >= requiredAda
        then return skelOut
        else go $ skelOut & txSkelOutValueL % txSkelOutValueContentL % Script.adaL .~ Script.Lovelace requiredAda

-- | This goes through all the `TxSkelOut`s of the given skeleton and updates
-- their ada value when requested by the user and required by the protocol
-- parameters.
toTxSkelWithMinAda :: (MonadBlockChainBalancing m) => TxSkel -> m TxSkel
toTxSkelWithMinAda skel = (\x -> skel & txSkelOutsL .~ x) <$> forM (skel ^. txSkelOutsL) toTxSkelOutWithMinAda
