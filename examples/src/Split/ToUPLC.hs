{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Split.ToUPLC where

import Data.ByteString
import Flat (flat)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import Split

-- | Compiles the split contract down to a bytestring, that can later be loaded
--  as a arbitrary UPLC contract. To see how to handle scripts with parameters,
--  please check "PMultiSigStateful.ToUPLC".
splitBS :: ByteString
splitBS =
  flat $
    PlutusTx.getPlc $
      $$(PlutusTx.compile [||wrap||])
        `PlutusTx.applyCode` $$(PlutusTx.compile [||validateSplit||])
  where
    wrap = Scripts.mkUntypedValidator @SplitDatum @SplitRedeemer
