module Cooked.MockChain.GenerateTx.Collateral where

import Cardano.Api qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano hiding (Testnet)
import Cardano.Ledger.Conway.Core qualified as Conway
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.Wallet
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Tx.CardanoAPI qualified as Ledger
import Lens.Micro.Extras qualified as MicroLens
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Numeric qualified as PlutusTx

-- | Computes the collateral triplet from the fees and the collateral inputs in
-- the context. What we call a collateral triplet is composed of:
-- * The set of collateral inputs
-- * The total collateral paid by the transaction in case of phase 2 failure
-- * An output returning excess collateral value when collaterals are used
-- These quantity should satisfy the equation (in terms of their values):
-- collateral inputs = total collateral + return collateral
toCollateralTriplet ::
  (MonadBlockChainBalancing m) =>
  Integer ->
  Maybe (Set Api.TxOutRef, Wallet) ->
  m
    ( Cardano.TxInsCollateral Cardano.ConwayEra,
      Cardano.TxTotalCollateral Cardano.ConwayEra,
      Cardano.TxReturnCollateral Cardano.CtxTx Cardano.ConwayEra
    )
toCollateralTriplet _ Nothing = return (Cardano.TxInsCollateralNone, Cardano.TxTotalCollateralNone, Cardano.TxReturnCollateralNone)
toCollateralTriplet fee (Just (Set.toList -> collateralInsList, returnCollateralWallet)) = do
  -- We build the collateral inputs from this list
  txInsCollateral <-
    case collateralInsList of
      [] -> return Cardano.TxInsCollateralNone
      l -> throwOnToCardanoError "toCollateralTriplet" $ Cardano.TxInsCollateral Cardano.AlonzoEraOnwardsConway <$> mapM Ledger.toCardanoTxIn l
  -- Retrieving the total value in collateral inputs. This fails if one of the
  -- collateral inputs has not been successfully resolved.
  collateralInsValue <-
    foldM (\val -> ((val <>) <$>) . (throwOnMaybe "toCollateralTriplet: unresolved txOutRefs" <=< valueFromTxOutRef)) mempty collateralInsList
  -- We retrieve the collateral percentage compared to fees. By default, we use
  -- 150% which is the current value in the parameters, although the default
  -- value should never be used here, as the call is supposed to always succeed.
  collateralPercentage <- toInteger . MicroLens.view Conway.ppCollateralPercentageL . Emulator.pEmulatorPParams <$> getParams
  -- The total collateral corresponds to the fees multiplied by the collateral
  -- percentage. We add 1 because the ledger apparently rounds up this value.
  let coinTotalCollateral = 1 + (fee * collateralPercentage) `div` 100
  -- We create the total collateral based on the computed value
  let txTotalCollateral = Cardano.TxTotalCollateral Cardano.BabbageEraOnwardsConway $ Emulator.Coin coinTotalCollateral
  -- We compute a return collateral value by subtracting the total collateral to
  -- the value in collateral inputs
  let returnCollateralValue = collateralInsValue <> PlutusTx.negate (Script.lovelace coinTotalCollateral)
  -- This should never happen, as we always compute the collaterals for the
  -- user, but we guard against having some negative elements in the value in
  -- case we give more freedom to the users in the future
  when (fst (Api.split returnCollateralValue) /= mempty) $ throwOnString "toCollateralTriplet: negative parts in return collateral value"
  -- The return collateral is then computed
  txReturnCollateral <-
    -- If the total collateral equal what the inputs provide, we return
    -- `TxReturnCollateralNone`, otherwise, we compute the new output
    if returnCollateralValue == mempty
      then return Cardano.TxReturnCollateralNone
      else do
        -- The value is a translation of the remaining value
        txReturnCollateralValue <-
          Ledger.toCardanoTxOutValue
            <$> throwOnToCardanoError
              "toCollateralTriplet: cannot build return collateral value"
              (Ledger.toCardanoValue returnCollateralValue)
        -- The address is the one from the return collateral wallet, which is
        -- required to exist here.
        networkId <- Emulator.pNetworkId <$> getParams
        address <-
          throwOnToCardanoError "toCollateralTriplet: cannot build return collateral address" $
            Ledger.toCardanoAddressInEra networkId (walletAddress returnCollateralWallet)
        -- The return collateral is built up from those elements
        return $
          Cardano.TxReturnCollateral Cardano.BabbageEraOnwardsConway $
            Cardano.TxOut address txReturnCollateralValue Cardano.TxOutDatumNone Cardano.ReferenceScriptNone
  return (txInsCollateral, txTotalCollateral, txReturnCollateral)
