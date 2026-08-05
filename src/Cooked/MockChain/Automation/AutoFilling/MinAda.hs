-- | This module exposes functions to automatically adjust the ADA contained in
-- the outputs of a 'Cooked.Skeleton.TxSkel' to satisfy the minimal amount
-- required by the protocol parameters.
module Cooked.MockChain.Automation.AutoFilling.MinAda
  ( getTxSkelOutMinAda,
    toTxSkelOutWithMinAda,
    autoFillMinAda,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Shelley.Core qualified as Shelley
import Control.Monad
import Cooked.MockChain.Automation.GenerateTx.Output
import Cooked.MockChain.Effect.Log
import Cooked.MockChain.Effect.Read
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Update
import Ledger.Tx qualified as P.Ledger
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error

-- * Auto filling min ada amounts

-- | Compute the required minimal ADA for a given output
getTxSkelOutMinAda ::
  (Members '[MockChainRead, Error P.Ledger.ToCardanoError] effs) =>
  TxSkelOut ->
  Sem effs Integer
getTxSkelOutMinAda txSkelOut = do
  params <- getParams
  Cardano.unCoin
    . Shelley.getMinCoinTxOut params
    . Cardano.toShelleyTxOut Cardano.ShelleyBasedEraConway
    . Cardano.toCtxUTxOTxOut
    <$> toCardanoTxOut txSkelOut

-- | This transforms an output into another output which contains the minimal
-- required ada. If the previous quantity of ADA was sufficient, it remains
-- unchanged. This can require a few iterations to converge, as the added ADA
-- will increase the size of the UTXO which in turn might need more ADA.
toTxSkelOutWithMinAda ::
  forall effs.
  (Members '[MockChainRead, MockChainLog, Error P.Ledger.ToCardanoError] effs) =>
  TxSkelOut ->
  Sem effs TxSkelOut
-- The auto adjustment is disabled so nothing is done here
toTxSkelOutWithMinAda txSkelOut@(view txSkelOutValueAutoAdjustL -> False) = return txSkelOut
-- The auto adjustment is enabled
toTxSkelOutWithMinAda txSkelOut = do
  txSkelOut' <- go txSkelOut
  let originalAda = view (txSkelOutValueL % valueLovelaceL) txSkelOut
      updatedAda = view (txSkelOutValueL % valueLovelaceL) txSkelOut'
  when (originalAda /= updatedAda) $ logEvent $ MCLogAdjustedTxSkelOut txSkelOut updatedAda
  return txSkelOut'
  where
    go :: TxSkelOut -> Sem effs TxSkelOut
    go skelOut = do
      -- Computing the required minimal amount of ADA in this output
      requiredAda <- getTxSkelOutMinAda skelOut
      -- If this amount is sufficient, we return Nothing, otherwise, we adjust the
      -- output and possibly iterate
      if Api.getLovelace (view (txSkelOutValueL % valueLovelaceL) skelOut) >= requiredAda
        then return skelOut
        else go $ set (txSkelOutValueL % valueLovelaceL) (Api.Lovelace requiredAda) skelOut

-- | This goes through all the `TxSkelOut`s of the given skeleton and updates
-- their ada value when requested by the user and required by the protocol
-- parameters. Logs an event whenever such a change occurs.
autoFillMinAda ::
  (Members '[Tweak, MockChainRead, MockChainLog, Error P.Ledger.ToCardanoError] effs) =>
  Sem effs ()
autoFillMinAda = traverseTweak (txSkelOutputsL % traversed) toTxSkelOutWithMinAda
