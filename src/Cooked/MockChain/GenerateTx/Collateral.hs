-- | This module exposes the generation of transaction collaterals, which
-- consist of a collateral amount, collateral inputs and return collateral
module Cooked.MockChain.GenerateTx.Collateral where

import Cardano.Api qualified as Cardano
import Cooked.MockChain.Common
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.Read
import Cooked.Skeleton.Output
import Cooked.Skeleton.Value
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error

-- | Computes the collateral triplet from the potential collaterals. What we
-- call a collateral triplet is composed of:
--
-- * The set of collateral inputs
--
-- * The total collateral paid by the transaction in case of phase 2 failure
--
-- * An output returning excess collateral value when collaterals are used
--
-- These quantity should satisfy the equation (in terms of their values):
-- collateral inputs = total collateral + return collateral
toCollateralTriplet ::
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  Maybe Collaterals ->
  Sem
    effs
    ( Cardano.TxInsCollateral Cardano.ConwayEra,
      Cardano.TxTotalCollateral Cardano.ConwayEra,
      Cardano.TxReturnCollateral Cardano.CtxTx Cardano.ConwayEra
    )
toCollateralTriplet Nothing = return (Cardano.TxInsCollateralNone, Cardano.TxTotalCollateralNone, Cardano.TxReturnCollateralNone)
toCollateralTriplet (Just (Set.toList -> collateralInsList, mReturnCollateral)) = do
  -- We build the collateral inputs from this list
  txInsCollateral <-
    case collateralInsList of
      [] -> return Cardano.TxInsCollateralNone
      l -> fromEither $ Cardano.TxInsCollateral Cardano.AlonzoEraOnwardsConway <$> mapM Ledger.toCardanoTxIn l
  -- We collect the amount of lovelace in the collateral inputs
  Api.Lovelace collateralInsLovelace <- foldOf (folded % txSkelOutValueL % valueLovelaceL) . Map.elems <$> lookupUtxos collateralInsList
  -- We collect the amount of lovelace in the return collateral output
  let Api.Lovelace returnCollateralLovelace = maybe 0 (view (txSkelOutValueL % valueLovelaceL)) mReturnCollateral
  -- The total collateral is the difference between the two
  let txTotalCollateral = Cardano.TxTotalCollateral Cardano.BabbageEraOnwardsConway $ Cardano.Coin $ collateralInsLovelace - returnCollateralLovelace
  txReturnCollateral <-
    case mReturnCollateral of
      Nothing -> return Cardano.TxReturnCollateralNone
      Just collateralOut -> Cardano.TxReturnCollateral Cardano.BabbageEraOnwardsConway <$> toCardanoTxOut collateralOut
  return (txInsCollateral, txTotalCollateral, txReturnCollateral)
