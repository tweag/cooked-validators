{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies
  ( mkForwardingMintingPolicy,
    forwardToValidator,
  )
where

import Plutus.Script.Utils.Scripts
  ( MintingPolicy,
    ValidatorHash (ValidatorHash),
    toMintingPolicy,
  )
import Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1
  ( Address (Address, addressCredential),
    Credential (ScriptCredential),
    ScriptHash (ScriptHash),
  )
import PlutusLedgerApi.V1.Contexts
  ( ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
    ScriptPurpose (Minting),
    TxInfo (TxInfo, txInfoInputs),
  )
import PlutusLedgerApi.V1.Contexts qualified as PV1
import PlutusLedgerApi.V1.Tx (TxOut (TxOut, txOutAddress))
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False), any, ($), (.), (==))

-- | A minting policy that checks whether the validator script was run
--  in the minting transaction.
mkForwardingMintingPolicy :: ValidatorHash -> MintingPolicy
mkForwardingMintingPolicy vshsh =
  toMintingPolicy
    $ $$(PlutusTx.compile [||\(hsh :: ValidatorHash) -> mkUntypedMintingPolicy (forwardToValidator hsh)||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 vshsh

{-# INLINEABLE forwardToValidator #-}
forwardToValidator :: ValidatorHash -> () -> PV1.ScriptContext -> Bool
forwardToValidator (ValidatorHash h) _ ScriptContext {scriptContextTxInfo = TxInfo {txInfoInputs}, scriptContextPurpose = Minting _} =
  let checkHash TxOut {txOutAddress = Address {addressCredential = ScriptCredential (ScriptHash vh)}} = vh == h
      checkHash _ = False
   in any (checkHash . PV1.txInInfoResolved) txInfoInputs
forwardToValidator _ _ _ = False
