{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | This module introduces standard dummy validators to be used in
-- attacks, traces or tests. More precisely, it introduces the always
-- True and always False validators, which will respectively always
-- succeed or always fail.
module Cooked.Validators
  ( alwaysTrueValidator,
    alwaysFalseValidator,
  )
where

import Cooked.RawUPLC
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Pl
import qualified PlutusTx as Pl

-- | The trivial validator that always succeds; this is in particular
-- a sufficient target for the datum hijacking attack since we only
-- want to show feasibility of the attack.
alwaysTrueValidator :: Pl.TypedValidator a
alwaysTrueValidator = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||tgt||]))
  where
    tgt :: Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()
    tgt _ _ _ = ()

-- | The trivial validator that always fails
alwaysFalseValidator :: Pl.TypedValidator a
alwaysFalseValidator = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||tgt||]))
  where
    tgt :: Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()
    tgt _ _ _ = error "This validator always fails."
