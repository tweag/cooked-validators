-- | Objects from which an output datum can be extracted
module Cooked.Conversion.ToOutputDatum where

import PlutusLedgerApi.V3 qualified as Api

class ToOutputDatum a where
  toOutputDatum :: a -> Api.OutputDatum

instance ToOutputDatum Api.OutputDatum where
  toOutputDatum = id

instance ToOutputDatum Api.Datum where
  toOutputDatum = Api.OutputDatum

instance ToOutputDatum () where
  toOutputDatum = const Api.NoOutputDatum

instance ToOutputDatum Api.DatumHash where
  toOutputDatum = Api.OutputDatumHash

instance ToOutputDatum Api.BuiltinData where
  toOutputDatum = toOutputDatum . Api.Datum
