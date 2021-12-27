{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module PMultiSigStateful.ToUPLC where

import Data.ByteString
import Flat (flat)
import qualified Ledger.Typed.Scripts as Scripts
import PMultiSigStateful
import qualified PlutusTx

-- | Compiles the pmultisig contract down to a bytestring, that can later be loaded
--  as a arbitrary UPLC contract.
pmultisigBS :: Params -> ByteString
pmultisigBS parms =
  flat $
    PlutusTx.getPlc $
      $$(PlutusTx.compile [||wrap||])
        `PlutusTx.applyCode` ( $$(PlutusTx.compile [||validatePayment||])
                                 `PlutusTx.applyCode` PlutusTx.liftCode parms
                             )
  where
    wrap = Scripts.wrapValidator @Datum @Redeemer
