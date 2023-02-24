{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

-- | Some validators to be used in test transactions.
module Cooked.Behaviour.Validators (yes, no) where

import qualified Plutus.Script.Utils.Typed as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import PlutusTx
import PlutusTx.Builtins ()
import PlutusTx.Lift ()
import PlutusTx.Prelude


data UnitValidator

instance Scripts.ValidatorTypes UnitValidator where
  type RedeemerType UnitValidator = ()
  type DatumType UnitValidator = ()

-- | The validator that always succeeds
yes :: Scripts.TypedValidator UnitValidator
yes =
  Scripts.mkTypedValidator @UnitValidator
       $$(compile [|| const . const . const $ True ||])
       $$(compile [|| wrap ||])
  where wrap = Scripts.mkUntypedValidator

-- | The validator that always fails
no :: Scripts.TypedValidator UnitValidator
no =
  Scripts.mkTypedValidator @UnitValidator
       $$(compile [|| const . const . const $ False ||])
       $$(compile [|| wrap ||])
  where wrap = Scripts.mkUntypedValidator
