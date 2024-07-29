module Cooked.MockChain.GenerateTx.Collateral where

import Cardano.Api qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano hiding (Testnet)
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Control.Monad.Reader
import Cooked.Conversion
import Cooked.MockChain.GenerateTx.Common
import Cooked.Wallet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Tx.CardanoAPI qualified as Ledger
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Numeric qualified as PlutusTx

data CollateralContext where
  CollateralContext ::
    { managedTxOuts :: Map Api.TxOutRef Api.TxOut,
      fee :: Integer,
      mCollaterals :: Maybe (Set Api.TxOutRef, Wallet),
      params :: Emulator.Params
    } ->
    CollateralContext

type CollateralGen a = TxGen CollateralContext a

-- | Computes the collateral triplet from the fees and the collateral inputs in
-- the context. What we call a collateral triplet is composed of:
-- * The set of collateral inputs
-- * The total collateral paid by the transaction in case of phase 2 failure
-- * An output returning excess collateral value when collaterals are used
-- These quantity should satisfy the equation (in terms of their values):
-- collateral inputs = total collateral + return collateral
toCollateralTriplet ::
  CollateralGen
    ( Cardano.TxInsCollateral Cardano.ConwayEra,
      Cardano.TxTotalCollateral Cardano.ConwayEra,
      Cardano.TxReturnCollateral Cardano.CtxTx Cardano.ConwayEra
    )
toCollateralTriplet = do
  -- Retrieving the optional collaterals and associated wallet
  mCollaterals <- asks mCollaterals
  case mCollaterals of
    -- If this is nothing, it means no collateral is needed (no script involved)
    Nothing -> return (Cardano.TxInsCollateralNone, Cardano.TxTotalCollateralNone, Cardano.TxReturnCollateralNone)
    Just (Set.toList -> collateralInsList, returnCollateralWallet) -> do
      -- Retrieving know outputs
      knownTxOuts <- asks managedTxOuts
      -- We build the collateral inputs from this list
      txInsCollateral <-
        case collateralInsList of
          [] -> return Cardano.TxInsCollateralNone
          l -> throwOnToCardanoError "txOutRefsToTxInCollateral" (Cardano.TxInsCollateral Cardano.AlonzoEraOnwardsConway <$> mapM Ledger.toCardanoTxIn l)
      -- Retrieving the total value in collateral inputs. This fails if one of the
      -- collaterals has been been successfully resolved.
      collateralInsValue <- do
        let collateralInsResolved = mapMaybe (`Map.lookup` knownTxOuts) collateralInsList
        when (length collateralInsResolved /= length collateralInsList) $ throwOnString "toCollateralTriplet: unresolved txOutRefs"
        return $ mconcat (Api.txOutValue <$> collateralInsResolved)
      -- We retrieve the collateral percentage compared to fees. By default, we use
      -- 150% which is the current value in the parameters, although the default
      -- value should never be used here, as the call is supposed to always succeed.
      collateralPercentage <- asks (toInteger . fromMaybe 150 . Cardano.protocolParamCollateralPercent . Emulator.pProtocolParams . params)
      -- The total collateral corresponds to the fees multiplied by the collateral
      -- percentage. We add 1 because the ledger apparently rounds up this value.
      coinTotalCollateral <- asks (Emulator.Coin . (+ 1) . (`div` 100) . (* collateralPercentage) . fee)
      -- We create the total collateral based on the computed value
      let txTotalCollateral = Cardano.TxTotalCollateral Cardano.BabbageEraOnwardsConway coinTotalCollateral
      -- We compute a return collateral value by subtracting the total collateral to
      -- the value in collateral inputs
      let returnCollateralValue = collateralInsValue <> PlutusTx.negate (toValue coinTotalCollateral)
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
            address <- do
              networkId <- asks (Emulator.pNetworkId . params)
              throwOnToCardanoError "toCollateralTriplet: cannot build return collateral address" $
                Ledger.toCardanoAddressInEra networkId (walletAddress returnCollateralWallet)
            -- The return collateral is built up from those elements
            return $
              Cardano.TxReturnCollateral Cardano.BabbageEraOnwardsConway $
                Cardano.TxOut address txReturnCollateralValue Cardano.TxOutDatumNone Cardano.ReferenceScriptNone
      return (txInsCollateral, txTotalCollateral, txReturnCollateral)
