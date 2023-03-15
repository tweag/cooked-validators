{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cooked.Validators.Other
  ( pkNotInDatum,
    PubKey,
    BoolR,
    yesBoolR,
  )
where

import Cooked
import Optics.Core (Iso', iso)
import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.Script.Utils.Typed as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V2.Ledger.Api as PV2
import Plutus.V2.Ledger.Contexts as PV2
import PlutusTx
import PlutusTx.Builtins ()
import PlutusTx.Lift ()
import PlutusTx.Prelude
import Prettyprinter (viaShow)
import qualified Prelude as Haskell

-- * Boolean redeemer

data BoolR

instance Scripts.ValidatorTypes BoolR where
  type RedeemerType BoolR = Bool
  type DatumType BoolR = ()

-- | The validator that accepts a boolean redeemer but always succeeds.
yesBoolR :: Scripts.TypedValidator BoolR
yesBoolR =
  Scripts.mkTypedValidator @BoolR
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator
    val _ _ _ = True

-- * A datum made of a public key

data PubKey

instance Scripts.ValidatorTypes PubKey where
  type RedeemerType PubKey = ()
  type DatumType PubKey = PubKeyHash

-- | Outputs can only be spent by pubkeys whose hash is not the one in the datum.
pkNotInDatum :: Scripts.TypedValidator PubKey
pkNotInDatum =
  Scripts.mkTypedValidator @PubKey
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    val :: PubKeyHash -> () -> ScriptContext -> Bool
    val pkh _ (ScriptContext txInfo _) =
      pkh `notElem` txInfoSignatories txInfo
    wrap = Scripts.mkUntypedValidator
