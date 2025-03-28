module Plutus.Script.Utils.Data
  ( ToOutputDatum (toOutputDatum),
    datumHash,
    redeemerHash,
    dataHash,
    toCardanoAPIData,
    toMaybeDatumHash,
  )
where

import Cardano.Api.Shelley qualified as C.Api
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V2 qualified as PV2
import PlutusTx.Builtins qualified as Builtins

-- * Converting to Datum

class ToOutputDatum a where
  toOutputDatum :: a -> PV2.OutputDatum

instance ToOutputDatum PV2.OutputDatum where
  toOutputDatum = id

instance ToOutputDatum PV2.Datum where
  toOutputDatum = PV2.OutputDatum

instance ToOutputDatum () where
  toOutputDatum () = PV2.NoOutputDatum

instance ToOutputDatum PV2.DatumHash where
  toOutputDatum = PV2.OutputDatumHash

instance ToOutputDatum PV1.BuiltinData where
  toOutputDatum = toOutputDatum . PV1.Datum

{- Note [Hash computation of datums, redeemers and scripts]

We have three options for computing the hash (each with advantages and drawbacks):

1- Depend on `cardano-api` and use it's `Scripts.hashScriptData` and `Scripts.hashScript`
functions.
The good: most simplest way to compute the hashes.
The bad: this package has an additional pretty large dependency.

2- Depend on `cardano-ledger` instead and use their `hashScriptData` and `hashScript`.
The good: smaller footprint than `cardano-api`.
The bad: a lower-lever library than `cardano-api`.

3- Depend on `cardano-crypto-class`, and reimplement ourselves the hashing functions
from `cardano-ledger`.
The good: the lowest dependency footprint.
The bad: code duplication.

However, we expect that most Plutus script devs depending on this package will
also probably depend on `cardano-api`, so the dependency on `cardano-api` should
(probably) be an non-issue.

If this becomes an issue, we'll change the implementation.
-}

-- * Hashing data

-- | Hash a 'PV1.Datum builtin data.
datumHash :: PV1.Datum -> PV1.DatumHash
datumHash = PV1.DatumHash . dataHash . PV1.getDatum

-- | Hash a 'PV1.Redeemer' builtin data.
redeemerHash :: PV1.Redeemer -> PV1.RedeemerHash
redeemerHash = PV1.RedeemerHash . dataHash . PV1.getRedeemer

-- | Hash a 'Builtins.BuiltinData'
dataHash :: Builtins.BuiltinData -> Builtins.BuiltinByteString
dataHash =
  Builtins.toBuiltin
    . C.Api.serialiseToRawBytes
    . C.Api.hashScriptDataBytes
    . toCardanoAPIData

-- | Convert a 'Builtins.BuiltinsData' value to a 'cardano-api' script
--  data value.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
toCardanoAPIData :: Builtins.BuiltinData -> C.Api.HashableScriptData
toCardanoAPIData =
  C.Api.unsafeHashableScriptData
    . C.Api.fromPlutusData
    . Builtins.builtinDataToData

toMaybeDatumHash :: PV2.OutputDatum -> Maybe PV1.DatumHash
toMaybeDatumHash PV2.NoOutputDatum = Nothing
toMaybeDatumHash (PV2.OutputDatumHash dh) = Just dh
toMaybeDatumHash (PV2.OutputDatum dat) = Just $ datumHash dat
