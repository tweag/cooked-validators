{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Some validators to be used in test transactions.
module Cooked.Behaviour.Validators
  ( Unit,
    yes,
    no,
    DatumKind (OnlyHash, ResolvedHash, Inline),
    continuingDatum,
    inputDatum,
    requireSigner,
  )
where

import qualified Plutus.Script.Utils.Typed as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.V2.Ledger.Api as PV2
import Plutus.V2.Ledger.Contexts as PV2
import PlutusTx
import PlutusTx.Builtins ()
import PlutusTx.Lift ()
import PlutusTx.Prelude
import qualified Prelude as Haskell

data Unit

instance Scripts.ValidatorTypes Unit where
  type RedeemerType Unit = ()
  type DatumType Unit = ()

-- | The validator that always succeeds
yes :: Scripts.TypedValidator Unit
yes =
  Scripts.mkTypedValidator @Unit
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    val _ _ _ = True
    wrap = Scripts.mkUntypedValidator

-- | The validator that always fails
no :: Scripts.TypedValidator Unit
no =
  Scripts.mkTypedValidator @Unit
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    val _ _ _ = False
    wrap = Scripts.mkUntypedValidator

-- | Three kinds of output datum.
data DatumKind = OnlyHash | ResolvedHash | Inline
  deriving (Haskell.Show)

makeLift ''DatumKind

-- | This defines three validators that check some properties on their
-- continuing output. The parameter affects them the following way:
-- - @continuingDatum OnlyHash@ is a validator that only returns true if there's
--   a continuing transaction output that has a datum hash that's not included in
--   the 'txInfoData',
-- - @continuingDatum ResolvedHash@ requires an output datum with a hash that's in
--   the 'txInfoData', and
-- - @continuingDatum Inline@ only returns true if the output has an inline
--   datum.
continuingDatum :: DatumKind -> Scripts.TypedValidator Unit
continuingDatum =
  Scripts.mkTypedValidatorParam @Unit
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    val :: DatumKind -> () -> () -> PV2.ScriptContext -> Bool
    val requiredOutputKind _ _ ctx =
      let [PV2.TxOut {PV2.txOutDatum = outDatum}] = PV2.getContinuingOutputs ctx
          txi = PV2.scriptContextTxInfo ctx
       in case (requiredOutputKind, outDatum) of
            (OnlyHash, PV2.OutputDatumHash h) -> isNothing (PV2.findDatum h txi)
            (ResolvedHash, PV2.OutputDatumHash h) -> isJust (PV2.findDatum h txi)
            (Inline, PV2.OutputDatum _) -> True
            _ -> False

    wrap = Scripts.mkUntypedValidator

-- | This defines two validators:
-- - @inputDatum True@ is true if the output it is asked to spend has an inline
--   datum,
-- - @inputDatum False@ is true if the output has a datum hash.
inputDatum :: Bool -> Scripts.TypedValidator Unit
inputDatum =
  Scripts.mkTypedValidatorParam @Unit
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    val :: Bool -> () -> () -> PV2.ScriptContext -> Bool
    val requireInlineDatum _ _ ctx =
      let Just (PV2.TxInInfo _ PV2.TxOut {PV2.txOutDatum = inDatum}) = PV2.findOwnInput ctx
       in if requireInlineDatum
            then case inDatum of
              PV2.OutputDatum _ -> True
              PV2.OutputDatumHash _ -> trace "I want an inline datum, but I got a hash" False
              PV2.NoOutputDatum -> trace "I want an inline datum, but I got neither a datum nor a hash" False
            else case inDatum of
              PV2.OutputDatumHash _ -> True
              PV2.OutputDatum _ -> trace "I want a datum hash, but I got an inline datum" False
              PV2.NoOutputDatum -> trace "I want a datum hash, but I got neither a datum nor a hash" False

    wrap = Scripts.mkUntypedValidator

-- | This validator ensures that the given public key signs the transaction.
requireSigner :: PV2.PubKeyHash -> Scripts.TypedValidator Unit
requireSigner =
  Scripts.mkTypedValidatorParam @Unit
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    val :: PV2.PubKeyHash -> () -> () -> PV2.ScriptContext -> Bool
    val pkh _ _ (PV2.ScriptContext txInfo _) =
      traceIfFalse "the required signer is missing" $
        elem pkh (txInfoSignatories txInfo)

    wrap = Scripts.mkUntypedValidator
