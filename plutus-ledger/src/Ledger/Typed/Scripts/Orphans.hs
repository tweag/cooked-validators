{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Typed.Scripts.Orphans where

import Data.Aeson (FromJSON, ToJSON)
import Ledger.Tx.Orphans ()
import Plutus.Script.Utils.V1 qualified as V1

deriving instance ToJSON (V1.TypedValidator a)

deriving instance FromJSON (V1.TypedValidator a)

deriving instance ToJSON V1.ConnectionError

deriving instance FromJSON V1.ConnectionError
