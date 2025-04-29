{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.ProposingScript where

import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude

{-# INLINEABLE checkParameterChangeProposingPurpose #-}
checkParameterChangeProposingPurpose :: Script.ProposingPurposeType ()
checkParameterChangeProposingPurpose _ (Api.ProposalProcedure _ _ (Api.ParameterChange _ (Api.ChangedParameters dat) _)) _ _ =
  let innerMap = unsafeFromBuiltinData @(Map.Map Integer Integer) dat
   in ((Map.toList innerMap == [(0, 100)]) || traceError "wrong map")
checkParameterChangeProposingPurpose _ _ _ _ = traceError "Wrong proposal procedure"

checkProposingScript :: Script.Versioned Script.Script
checkProposingScript =
  Script.toVersioned
    $ Script.MultiPurposeScript @()
    $ Script.toScript $$(compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript
        $ Script.falseTypedMultiPurposeScript
        `Script.withProposingPurpose` checkParameterChangeProposingPurpose

-- | A dummy false proposing validator
alwaysFalseProposingValidator :: Script.Versioned Script.Script
alwaysFalseProposingValidator =
  Script.toVersioned
    $ Script.MultiPurposeScript @()
    $ Script.toScript $$(compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript
        $ Script.falseTypedMultiPurposeScript
        `Script.withProposingPurpose` (\_ _ () () -> False)

-- | A dummy true proposing validator
alwaysTrueProposingValidator :: Script.Versioned Script.Script
alwaysTrueProposingValidator =
  Script.toVersioned
    $ Script.MultiPurposeScript @()
    $ Script.toScript $$(compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript
        $ Script.falseTypedMultiPurposeScript
        `Script.withProposingPurpose` (\_ _ () () -> True)
