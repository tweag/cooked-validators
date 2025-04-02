{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Typed.Scripts.Orphans where

import Data.Aeson (FromJSON, ToJSON)
import Ledger.Tx.Orphans ()
import Plutus.Script.Utils.Typed
import Plutus.Script.Utils.V1 qualified as V1
import Plutus.Script.Utils.V2 qualified as V2

deriving instance ToJSON (TypedValidator a)

deriving instance FromJSON (TypedValidator a)

deriving instance ToJSON V1.ConnectionError

deriving instance FromJSON V1.ConnectionError

deriving instance ToJSON V2.ConnectionError

deriving instance FromJSON V2.ConnectionError
