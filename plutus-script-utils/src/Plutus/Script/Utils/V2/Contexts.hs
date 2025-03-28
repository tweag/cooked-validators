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
    ownHashValueAndDatum,
    findInput,
  )
where

import Plutus.Script.Utils.Address (ToAddress (toAddress))
import Plutus.Script.Utils.Scripts (ToValidatorHash, ValidatorHash (..), toScriptHash, toValidatorHash)
import PlutusLedgerApi.V2 (Address (Address), Credential (ScriptCredential), OutputDatum, ScriptHash (ScriptHash), Value)
import PlutusLedgerApi.V2.Contexts as Contexts hiding (findTxInByTxOutRef, valuePaidTo)
import PlutusTx.Prelude (Maybe (Just), find, foldMap, fst, traceError, (.), (<$>), (==))

{-# INLINEABLE findInput #-}
findInput :: TxOutRef -> [TxInInfo] -> Maybe TxOut
findInput outRef = (txInInfoResolved <$>) . find ((== outRef) . txInInfoOutRef)

{-# INLINEABLE findTxInByTxOutRef #-}
findTxInByTxOutRef :: TxOutRef -> TxInfo -> Maybe TxOut
findTxInByTxOutRef outRef = findInput outRef . txInfoInputs

{-# INLINEABLE findTxRefInByTxOutRef #-}
findTxRefInByTxOutRef :: TxOutRef -> TxInfo -> Maybe TxOut
findTxRefInByTxOutRef outRef = findInput outRef . txInfoReferenceInputs

-- | Get the datums and values paid to an address by a pending transaction.
{-# INLINEABLE outputsAt #-}
outputsAt :: (ToAddress a) => TxInfo -> a -> [(Value, OutputDatum)]
outputsAt txInfo (toAddress -> addr) = [(val, dat) | TxOut addr' val dat _ <- txInfoOutputs txInfo, addr == addr']

-- | Get the datums and values paid to an address of a validator by a pending transaction.
{-# INLINEABLE scriptOutputsAt #-}
scriptOutputsAt :: (ToValidatorHash a) => TxInfo -> a -> [(Value, OutputDatum)]
scriptOutputsAt txInfo = outputsAt txInfo . toScriptHash . toValidatorHash

-- | Get the total value paid to a public key address by a pending transaction.
{-# INLINEABLE valuePaidTo #-}
valuePaidTo :: (ToAddress a) => TxInfo -> a -> Value
valuePaidTo txInfo = foldMap fst . outputsAt txInfo

-- | Get the validator hash, datum and value of the output that is curently
-- being validated
{-# INLINEABLE ownHashValueAndDatum #-}
ownHashValueAndDatum :: ScriptContext -> (ValidatorHash, Value, OutputDatum)
ownHashValueAndDatum (findOwnInput -> Just (TxInInfo _ (TxOut (Address (ScriptCredential (ScriptHash s)) _) val dat _))) = (ValidatorHash s, val, dat)
ownHashValueAndDatum _ = traceError "Unable to find the own script's input"

-- | Get the hash of the validator script that is currently being validated.
{-# INLINEABLE ownHash #-}
ownHash :: ScriptContext -> ValidatorHash
ownHash (ownHashValueAndDatum -> (vHash, _, _)) = vHash
