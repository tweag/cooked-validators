{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module Plutus.Script.Utils.V2.Contexts
  ( module Contexts,
    findTxInByTxOutRef,
    findTxRefInByTxOutRef,
    outputsAt,
    scriptOutputsAt,
    valuePaidTo,
    ownHash,
    ownHashes,
  )
where

import Plutus.Script.Utils.Scripts (ValidatorHash (..))
import PlutusLedgerApi.V1 (Address, Value)
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V2.Contexts as Contexts hiding (findTxInByTxOutRef, valuePaidTo)
import PlutusTx.Prelude (Maybe (Just, Nothing), find, foldMap, fst, mapMaybe, snd, traceError, (==))

{-# INLINEABLE findTxInByTxOutRef #-}
findTxInByTxOutRef :: TxOutRef -> PV2.TxInfo -> Maybe PV2.TxInInfo
findTxInByTxOutRef outRef PV2.TxInfo {PV2.txInfoInputs} =
  find (\PV2.TxInInfo {PV2.txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoInputs

{-# INLINEABLE findTxRefInByTxOutRef #-}
findTxRefInByTxOutRef :: TxOutRef -> PV2.TxInfo -> Maybe PV2.TxInInfo
findTxRefInByTxOutRef outRef PV2.TxInfo {PV2.txInfoReferenceInputs} =
  find (\PV2.TxInInfo {PV2.txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoReferenceInputs

{-# INLINEABLE outputsAt #-}

-- | Get the datums and values paid to an address by a pending transaction.
outputsAt :: Address -> TxInfo -> [(PV2.OutputDatum, Value)]
outputsAt addr p =
  let flt TxOut {txOutAddress, txOutValue, txOutDatum} | txOutAddress == addr = Just (txOutDatum, txOutValue)
      flt _ = Nothing
   in mapMaybe flt (txInfoOutputs p)

{-# INLINEABLE scriptOutputsAt #-}

-- | Get the datums and values paid to an address of a validator by a pending transaction.
scriptOutputsAt :: ValidatorHash -> TxInfo -> [(PV2.OutputDatum, Value)]
scriptOutputsAt (ValidatorHash s) = outputsAt (PV2.Address (PV2.ScriptCredential (PV2.ScriptHash s)) Nothing)

{-# INLINEABLE valuePaidTo #-}

-- | Get the total value paid to a public key address by a pending transaction.
valuePaidTo :: TxInfo -> Address -> Value
valuePaidTo ptx addr = foldMap snd (outputsAt addr ptx)

{-# INLINEABLE ownHashes #-}

-- | Get the validator and datum hashes of the output that is curently being validated
ownHashes :: ScriptContext -> (ValidatorHash, PV2.OutputDatum)
ownHashes
  ( findOwnInput ->
      Just
        TxInInfo
          { txInInfoResolved =
              TxOut {txOutAddress = PV2.Address (PV2.ScriptCredential (PV2.ScriptHash s)) _, txOutDatum = d}
          }
    ) = (ValidatorHash s, d)
ownHashes _ = traceError "Lg" -- "Can't get validator and datum hashes"

{-# INLINEABLE ownHash #-}

-- | Get the hash of the validator script that is currently being validated.
ownHash :: ScriptContext -> ValidatorHash
ownHash p = fst (ownHashes p)
