{-# OPTIONS_GHC -Wno-orphans #-}

-- | This modules exposes optics around 'Api.Value' and 'Api.Lovelace' that we
-- use in our 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Value
  ( valueAssetClassAmountL,
    lovelaceIntegerI,
    valueLovelaceL,
    valueAssetClassAmountP,
    valueLovelaceP,
  )
where

import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusTx.AssocMap qualified as PMap

-- | A lens to get or set the amount of tokens of a certain 'Api.AssetClass'
-- from a given 'Api.Value'. This removes the entry if the new amount is 0.
valueAssetClassAmountL :: (Script.ToMintingPolicyHash mp) => mp -> Api.TokenName -> Lens' Api.Value Integer
valueAssetClassAmountL (Script.toCurrencySymbol -> cs) tk =
  lens
    (`Api.assetClassValueOf` Api.assetClass cs tk)
    ( \v@(Api.Value val) i -> case PMap.lookup cs val of
        -- No previous cs entry and nothing to add.
        Nothing | i == 0 -> v
        -- No previous cs entry, and something to add.
        Nothing -> Api.Value $ PMap.insert cs (PMap.singleton tk i) val
        -- A previous cs and tk entry, which needs to be removed and the whole
        -- cs entry as well because it only containes this tk.
        Just (PMap.toList -> [(tk', _)]) | i == 0, tk == tk' -> Api.Value $ PMap.delete cs val
        -- A previous cs and tk entry, which needs to be removed, but the whole
        -- cs entry has other tokens and thus is kept.
        Just tokenMap | i == 0 -> Api.Value $ PMap.insert cs (PMap.delete tk tokenMap) val
        -- A previous cs entry, in which we insert the new tk (regarless of
        -- whether the tk was already present).
        Just tokenMap -> Api.Value $ PMap.insert cs (PMap.insert tk i tokenMap) val
    )

-- | Isomorphism between 'Api.Lovelace' and integers
lovelaceIntegerI :: Iso' Api.Lovelace Integer
lovelaceIntegerI = iso Api.getLovelace Api.Lovelace

-- | Focus the Lovelace part in a value.
valueLovelaceL :: Lens' Api.Value Api.Lovelace
valueLovelaceL = valueAssetClassAmountL Api.adaSymbol Api.adaToken % re lovelaceIntegerI

-- | A prism to build a value from an asset class and amount, or retrieves the
-- amount from this asset class if it is not zero
valueAssetClassAmountP :: (Script.ToMintingPolicyHash mp) => mp -> Api.TokenName -> Prism' Api.Value Integer
valueAssetClassAmountP (Script.toCurrencySymbol -> cs) tk
  | ac <- Api.assetClass cs tk =
      prism
        ( \case
            i | i == 0 -> mempty
            i -> Api.assetClassValue ac i
        )
        ( \val -> case val `Api.assetClassValueOf` ac of
            i | i == 0 -> Left val
            i -> Right i
        )

-- | An instance of 'valueAssetClassAmountP' for 'Api.Lovelace'
valueLovelaceP :: Prism' Api.Value Api.Lovelace
valueLovelaceP = valueAssetClassAmountP Api.adaSymbol Api.adaToken % re lovelaceIntegerI

instance Ord Api.Value where
  compare v1 v2 = compare (PMap.toList $ Api.getValue v1) (PMap.toList $ Api.getValue v2)
