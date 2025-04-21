{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.ProposingScript where

import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Builtins qualified as PlutusTx hiding (head)
import PlutusTx.Eq qualified as PlutusTx
import PlutusTx.IsData qualified as PlutusTx
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.TH qualified as PlutusTx
import Prelude qualified as HS

{-# INLINEABLE checkParameterChangeProposingPurpose #-}
checkParameterChangeProposingPurpose :: Script.ProposingPurposeType ()
checkParameterChangeProposingPurpose _ (Api.ProposalProcedure _ _ (Api.ParameterChange _ (Api.ChangedParameters dat) _)) _ _ =
  let innerMap = PlutusTx.unsafeFromBuiltinData @(PMap.Map PlutusTx.Integer PlutusTx.Integer) dat
   in ((PMap.toList innerMap PlutusTx.== [(0, 100)]) PlutusTx.|| PlutusTx.traceError "wrong map")
checkParameterChangeProposingPurpose _ _ _ _ = PlutusTx.traceError "Wrong proposal procedure"

checkProposingScript :: Script.Versioned Script.Script
checkProposingScript =
  Script.toVersioned HS.$
    Script.MultiPurposeScript @() HS.$
      Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript HS.$
        Script.falseTypedMultiPurposeScript
          `Script.withProposingPurpose` checkParameterChangeProposingPurpose

-- | A dummy false proposing validator
alwaysFalseProposingValidator :: Script.Versioned Script.Script
alwaysFalseProposingValidator =
  Script.toVersioned HS.$
    Script.MultiPurposeScript @() HS.$
      Script.toScript $$(PlutusTx.compile [||script||])
  where
    script = Script.mkMultiPurposeScript Script.falseTypedMultiPurposeScript

-- | A dummy true proposing validator
alwaysTrueProposingValidator :: Script.Versioned Script.Script
alwaysTrueProposingValidator =
  Script.toVersioned HS.$
    Script.MultiPurposeScript @() HS.$
      Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript HS.$
        Script.falseTypedMultiPurposeScript
          `Script.withProposingPurpose` (\_ _ () () -> PlutusTx.True)
