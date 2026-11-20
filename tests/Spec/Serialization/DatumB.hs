{-# LANGUAGE DeriveAnyClass #-}

-- | A datum type whose name collides with 'Spec.Serialization.DatumA.Datum'
module Spec.Serialization.DatumB where

import Cooked
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter qualified as PP

newtype Datum = Datum Api.BuiltinByteString
  deriving newtype (Show, Eq, Api.ToData, Api.FromData)
  deriving anyclass (HasTypeKey)

instance PrettyCooked Datum where
  prettyCooked = PP.viaShow
