module Cooked.Skeleton.Datum
  ( TxSkelOutDatumConstrs,
    TxSkelOutDatum (..),
    txSkelOutTypedDatum,
    txSkelOutUntypedDatum,
  )
where

import Cooked.Conversion
import Cooked.Pretty.Class
import Data.Typeable (cast)
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx
import Type.Reflection

type TxSkelOutDatumConstrs a = (Show a, PrettyCooked a, Api.ToData a, PlutusTx.Eq a, Typeable a)

-- | On transaction outputs, we have the options to use
--
-- 1. no datum
-- 2. only a datum hash
-- 3. a "normal" datum
-- 4. an inline datum
--
-- These four options are also what the type 'TxSkelOutDatum' records. The
-- following table explains their differences.
--
-- +------------------------+------------------+---------------------+-----------------------+
-- |                        | datum stored in  |                     | 'Api.OutputDatum'     |
-- |                        | in the simulated | datum resolved      | constructor           |
-- |                        | chain state      | on the 'txInfoData' | seen by the validator |
-- +========================+==================+=====================+=======================+
-- | 'TxSkelOutNoDatum'     | no               | no                  | 'Api.NoOutputDatum'   |
-- +------------------------+------------------+---------------------+-----------------------+
-- | 'TxSkelOutDatumHash'   | yes              | no                  | 'Api.OutputDatumHash' |
-- +------------------------+------------------+---------------------+-----------------------+
-- | 'TxSkelOutDatum'       | yes              | yes                 | 'Api.OutputDatumHash' |
-- +------------------------+------------------+---------------------+-----------------------+
-- | 'TxSkelOutInlineDatum' | yes              | no                  | 'Api.OutputDatum'     |
-- +------------------------+------------------+---------------------+-----------------------+
--
-- That is:
--
-- - Whenever there is a datum, we'll store it in the state of our simulated
--   chain. This will make it possible to retrieve it later, using functions
--   such as 'datumFromHash'.
--
-- - Both of the 'TxSkelOutDatumHash' and 'TxSkelOutDatum' constructors will
--   create an output that scripts see on the 'txInfo' as having a datum
--   hash. The difference is whether that hash will be resolvable using
--   validator functions like 'findDatum'.
data TxSkelOutDatum where
  -- | use no datum
  TxSkelOutNoDatum :: TxSkelOutDatum
  -- | only include the hash on the transaction
  TxSkelOutDatumHash :: (TxSkelOutDatumConstrs a) => a -> TxSkelOutDatum
  -- | use a 'Api.OutputDatumHash' on the transaction output, but generate the
  -- transaction in such a way that the complete datum is included in the
  -- 'txInfoData' seen by validators
  TxSkelOutDatum :: (TxSkelOutDatumConstrs a) => a -> TxSkelOutDatum
  -- | use an inline datum
  TxSkelOutInlineDatum :: (TxSkelOutDatumConstrs a) => a -> TxSkelOutDatum

deriving instance Show TxSkelOutDatum

instance Eq TxSkelOutDatum where
  x == y = compare x y == EQ

instance Ord TxSkelOutDatum where
  compare = curry $ \case
    (TxSkelOutNoDatum, TxSkelOutNoDatum) -> EQ
    (TxSkelOutDatumHash d1, TxSkelOutDatumHash d2) -> compareDats d1 d2
    (TxSkelOutDatum d1, TxSkelOutDatum d2) -> compareDats d1 d2
    (TxSkelOutInlineDatum d1, TxSkelOutInlineDatum d2) -> compareDats d1 d2
    (TxSkelOutDatumHash {}, TxSkelOutNoDatum) -> GT
    (TxSkelOutDatum {}, TxSkelOutNoDatum) -> GT
    (TxSkelOutDatum {}, TxSkelOutDatumHash {}) -> GT
    (TxSkelOutInlineDatum {}, _) -> GT
    (_, _) -> LT
    where
      compareDats d1 d2 = case compare (SomeTypeRep (typeOf d1)) (SomeTypeRep (typeOf d2)) of
        EQ -> compare (Api.toBuiltinData d1) (Api.toBuiltinData d2)
        a -> a

instance ToOutputDatum TxSkelOutDatum where
  toOutputDatum TxSkelOutNoDatum = Api.NoOutputDatum
  toOutputDatum (TxSkelOutDatumHash datum) = Api.OutputDatumHash . Script.datumHash . Api.Datum . Api.toBuiltinData $ datum
  toOutputDatum (TxSkelOutDatum datum) = Api.OutputDatumHash . Script.datumHash . Api.Datum . Api.toBuiltinData $ datum
  toOutputDatum (TxSkelOutInlineDatum datum) = Api.OutputDatum . Api.Datum . Api.toBuiltinData $ datum

txSkelOutUntypedDatum :: TxSkelOutDatum -> Maybe Api.Datum
txSkelOutUntypedDatum = \case
  TxSkelOutNoDatum -> Nothing
  TxSkelOutDatumHash x -> Just (Api.Datum $ Api.toBuiltinData x)
  TxSkelOutDatum x -> Just (Api.Datum $ Api.toBuiltinData x)
  TxSkelOutInlineDatum x -> Just (Api.Datum $ Api.toBuiltinData x)

txSkelOutTypedDatum :: (Typeable a) => TxSkelOutDatum -> Maybe a
txSkelOutTypedDatum = \case
  TxSkelOutNoDatum -> Nothing
  TxSkelOutDatumHash x -> cast x
  TxSkelOutDatum x -> cast x
  TxSkelOutInlineDatum x -> cast x
