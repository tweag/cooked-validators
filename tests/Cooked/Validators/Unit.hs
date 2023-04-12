{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Validators to be used in tests. All validators defined there use () for
-- datum and redeemer.
module Cooked.Validators.Unit
  ( Unit,
    yes,
    no,
    DatumKind (OnlyHash, ResolvedHash, Inline),
    continuingDatum,
    inputDatum,
    requireSigner,
    requireRefScript,
    validRangeSubsetOf,
    checkFeeBetween,
    nonEmptyWithdrawal,
    otherInputDatum,
  )
where

import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.Script.Utils.Typed as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V2.Ledger.Api as PV2
import Plutus.V2.Ledger.Contexts as PV2
import PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
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

-- | This validator ensures that there is a transaction input that has a
-- reference script with the given hash.
requireRefScript :: PV2.ScriptHash -> Scripts.TypedValidator Unit
requireRefScript =
  Scripts.mkTypedValidatorParam @Unit
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    val :: ScriptHash -> () -> () -> ScriptContext -> Bool
    val expectedScriptHash _ _ (ScriptContext txInfo _) =
      traceIfFalse "there is no reference input with the correct script hash" $
        any
          ( \(TxInInfo _ (TxOut _ _ _ mRefScriptHash)) ->
              Just expectedScriptHash == mRefScriptHash
          )
          (txInfoReferenceInputs txInfo)

    wrap = Scripts.mkUntypedValidator

-- | A validator that succeeds if the valid range is a subset of the argument.
-- Have this in mind when using this validator:
-- https://github.com/input-output-hk/plutus/issues/5185
validRangeSubsetOf :: (Maybe Integer, Maybe Integer) -> Scripts.TypedValidator Unit
validRangeSubsetOf =
  Scripts.mkTypedValidatorParam @Unit
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    timeRangeOfPair Nothing Nothing = always
    timeRangeOfPair Nothing (Just a) = to (POSIXTime a)
    timeRangeOfPair (Just a) Nothing = from (POSIXTime a)
    timeRangeOfPair (Just a) (Just b) = Interval.interval (POSIXTime a) (POSIXTime b)
    val :: (Maybe Integer, Maybe Integer) -> () -> () -> ScriptContext -> Bool
    val range _ _ (ScriptContext txInfo _) =
      uncurry timeRangeOfPair range `contains` txInfoValidRange txInfo
    wrap = Scripts.mkUntypedValidator

-- | Succeeds if the fee is in the given range.
checkFeeBetween ::
  -- | Range @(x, Nothing)@ encodes @[max x 0, +∞)@ while @(x, Just y)@ encodes
  -- @[max x 0, max y 0]@.
  (Integer, Maybe Integer) ->
  Scripts.TypedValidator Unit
checkFeeBetween =
  Scripts.mkTypedValidatorParam @Unit
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator
    rangeOfPair :: (AdditiveMonoid a, Ord a) => a -> Maybe a -> Interval a
    rangeOfPair a Nothing = from (max zero a)
    rangeOfPair a (Just b) = Interval.interval (max zero a) (max zero b)
    val :: (Integer, Maybe Integer) -> () -> () -> ScriptContext -> Bool
    val (lower, upper) _ _ (ScriptContext txInfo _) =
      Ada.fromValue (txInfoFee txInfo)
        `member` rangeOfPair (Ada.Lovelace lower) (fmap Ada.Lovelace upper)

-- | Succeeds if some withdrawal is performed.
nonEmptyWithdrawal :: Scripts.TypedValidator Unit
nonEmptyWithdrawal =
  Scripts.mkTypedValidator @Unit
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator
    val :: () -> () -> ScriptContext -> Bool
    val _ _ (ScriptContext (TxInfo {txInfoWdrl = wdrl}) _) =
      wdrl /= AssocMap.empty

-- * Coupled validators

-- 'otherInputDatum' and 'Cooked.Validators.Other.pkNotInDatum' are two scripts
-- to test reference inputs. They serve no purpose and make no real sense.

-- | Outputs can only be spent if in the transaction, there's an input whose
-- datum contains the pubkey hash of a signer of the transaction. The datum is
-- expected inline.
otherInputDatum :: Scripts.TypedValidator Unit
otherInputDatum =
  Scripts.mkTypedValidator @Unit
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator
    val :: () -> () -> ScriptContext -> Bool
    val _ _ (ScriptContext txInfo _) =
      any (f txInfo) (txInfoReferenceInputs txInfo)
    f :: TxInfo -> TxInInfo -> Bool
    f
      txInfo
      ( TxInInfo
          _
          (TxOut _ _ (OutputDatum (Datum datum)) _)
        ) =
        case fromBuiltinData @PubKeyHash datum of
          Nothing -> False
          Just pkh -> pkh `elem` txInfoSignatories txInfo
    f _ _ = False
