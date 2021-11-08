{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stealer where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Contexts as Contexts
import qualified PlutusTx
import qualified Ledger.Typed.Scripts as Scripts
import Schema (ToSchema)
import PlutusTx.Prelude hiding (Applicative (..))
import qualified Prelude as Haskell

newtype StealerParams = StealerParams
  { stealerPKH :: Ledger.PubKeyHash
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''StealerParams

newtype StealerDatum = StealerDatum
  { fake :: Integer}
  deriving stock (Haskell.Show)

type StealerRedeemer = ()

validateSteal :: StealerParams -> StealerDatum -> StealerRedeemer -> Contexts.ScriptContext -> Bool
validateSteal (StealerParams pkh) _ _ context =
  let info = Contexts.scriptContextTxInfo context
  in
    elem pkh (Contexts.txInfoSignatories info)

data Stealer

PlutusTx.makeLift ''StealerDatum
PlutusTx.unstableMakeIsData ''StealerDatum

instance Scripts.ValidatorTypes Stealer where
  type RedeemerType Stealer = StealerRedeemer
  type DatumType Stealer = StealerDatum

stealerValidator :: StealerParams -> Scripts.TypedValidator Stealer
stealerValidator =
  Scripts.mkTypedValidatorParam @Stealer
    $$(PlutusTx.compile [||validateSteal||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @StealerDatum @StealerRedeemer
