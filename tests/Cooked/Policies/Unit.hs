{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Minting policies with unit redeemer
module Cooked.Policies.Unit (yes, careful) where

import qualified Plutus.Script.Utils.Typed as Scripts
import qualified Plutus.Script.Utils.Value as Pl
import Plutus.V2.Ledger.Api as PV2
import Plutus.V2.Ledger.Contexts as PV2
import PlutusTx
import PlutusTx.Builtins ()
import PlutusTx.Lift ()
import PlutusTx.Prelude

-- | The policy that always succeeds.
yes :: Scripts.Versioned MintingPolicy
yes =
  flip Scripts.Versioned Scripts.PlutusV2 $
    mkMintingPolicyScript
      $$(compile [||Scripts.mkUntypedMintingPolicy mkCarelessPolicy||])
  where
    mkCarelessPolicy :: () -> ScriptContext -> Bool
    mkCarelessPolicy _ _ = True

mkCarefulPolicy :: TokenName -> Integer -> () -> ScriptContext -> Bool
mkCarefulPolicy tName allowedAmount _ ctx
  | amnt == Just allowedAmount = True
  | otherwise = trace "tried to mint wrong amount" False
  where
    amnt :: Maybe Integer
    amnt = case Pl.flattenValue (txInfoMint $ scriptContextTxInfo ctx) of
      [(cs, tn, a)] | cs == ownCurrencySymbol ctx && tn == tName -> Just a
      _ -> Nothing

-- | The minting policy that succeeds when a given amount of a given token is
-- minted.
careful ::
  -- | The token to mint
  TokenName ->
  -- | How much to mint
  Integer ->
  Scripts.Versioned MintingPolicy
careful tName allowedAmount =
  flip Scripts.Versioned Scripts.PlutusV2 . mkMintingPolicyScript $
    $$(compile [||\n x -> Scripts.mkUntypedMintingPolicy (mkCarefulPolicy n x)||])
      `applyCode` liftCode tName
      `applyCode` liftCode allowedAmount
