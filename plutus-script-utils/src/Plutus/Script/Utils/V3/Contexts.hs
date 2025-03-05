{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module Plutus.Script.Utils.V3.Contexts
  ( module Contexts,
    findTxRefInByTxOutRef,
    outputsAt,
    scriptOutputsAt,
    ownHash,
    ownHashes,
  )
where

import Plutus.Script.Utils.Scripts (ValidatorHash (..))
import PlutusLedgerApi.V1 (Address, Value)
import PlutusLedgerApi.V3 qualified as PV3
import PlutusLedgerApi.V3.Contexts as Contexts
import PlutusTx.Prelude (Maybe (Just, Nothing), find, fst, mapMaybe, traceError, (==))

{-# INLINEABLE findTxRefInByTxOutRef #-}
findTxRefInByTxOutRef :: PV3.TxOutRef -> PV3.TxInfo -> Maybe PV3.TxInInfo
findTxRefInByTxOutRef outRef PV3.TxInfo {PV3.txInfoReferenceInputs} =
  find (\PV3.TxInInfo {PV3.txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoReferenceInputs

{-# INLINEABLE outputsAt #-}

-- | Get the datums and values paid to an address by a pending transaction.
outputsAt :: Address -> TxInfo -> [(PV3.OutputDatum, Value)]
outputsAt addr p =
  let flt PV3.TxOut {PV3.txOutAddress, PV3.txOutValue, PV3.txOutDatum} | txOutAddress == addr = Just (txOutDatum, txOutValue)
      flt _ = Nothing
   in mapMaybe flt (txInfoOutputs p)

{-# INLINEABLE scriptOutputsAt #-}

-- | Get the datums and values paid to an address of a validator by a pending transaction.
scriptOutputsAt :: ValidatorHash -> TxInfo -> [(PV3.OutputDatum, Value)]
scriptOutputsAt (ValidatorHash s) = outputsAt (PV3.Address (PV3.ScriptCredential (PV3.ScriptHash s)) Nothing)

{-# INLINEABLE ownHashes #-}

-- | Get the validator and datum hashes of the output that is curently being validated
ownHashes :: ScriptContext -> (ValidatorHash, PV3.OutputDatum)
ownHashes
  ( findOwnInput ->
      Just
        PV3.TxInInfo
          { PV3.txInInfoResolved =
              PV3.TxOut
                { PV3.txOutAddress = PV3.Address (PV3.ScriptCredential (PV3.ScriptHash s)) _,
                  PV3.txOutDatum = d
                }
          }
    ) = (ValidatorHash s, d)
ownHashes _ = traceError "Lg" -- "Can't get validator and datum hashes"

{-# INLINEABLE ownHash #-}

-- | Get the hash of the validator script that is currently being validated.
ownHash :: ScriptContext -> ValidatorHash
ownHash p = fst (ownHashes p)
