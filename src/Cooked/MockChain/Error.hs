-- | This module exposes the errors that can be raised during a mockchain run
module Cooked.MockChain.Error
  ( -- * Mockchain errors
    MockChainError (..),

    -- * Interpretating effects into `Error MockChainError`
    runToCardanoErrorInMockChainError,
    runFailInMockChainError,
  )
where

import Cooked.Skeleton.User
import Ledger.Index qualified as Ledger
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail

-- | Errors that can be produced by the blockchain
data MockChainError
  = -- | Validation errors, either in Phase 1 or Phase 2
    MCEValidationError Ledger.ValidationPhase Ledger.ValidationError
  | -- | The balancing user does not have enough funds
    MCEUnbalanceable Peer Api.Value
  | -- | The balancing user is required but missing
    MCEMissingBalancingUser String
  | -- | No suitable collateral could be associated with a skeleton
    MCENoSuitableCollateral Integer Integer Api.Value
  | -- | Translating a skeleton element to its Cardano counterpart failed
    MCEToCardanoError Ledger.ToCardanoError
  | -- | The required reference script is missing from a witness utxo
    MCEWrongReferenceScriptError Api.TxOutRef Api.ScriptHash (Maybe Api.ScriptHash)
  | -- | A UTxO is missing from the mockchain state
    MCEUnknownOutRef Api.TxOutRef
  | -- | A jump in time would result in a past slot
    MCEPastSlot Ledger.Slot Ledger.Slot
  | -- | An attempt to invoke an unsupported feature has been made
    MCEUnsupportedFeature String
  | -- | Used to provide 'MonadFail' instances.
    MCEFailure String
  deriving (Show, Eq)

-- | Interpreting `Ledger.ToCardanoError` in terms of `MockChainError`
runToCardanoErrorInMockChainError ::
  forall effs a.
  (Member (Error MockChainError) effs) =>
  Sem (Error Ledger.ToCardanoError : effs) a ->
  Sem effs a
runToCardanoErrorInMockChainError = mapError MCEToCardanoError

-- | Interpreting failures in terms of `MockChainError`
runFailInMockChainError ::
  forall effs a.
  (Member (Error MockChainError) effs) =>
  Sem (Fail : effs) a ->
  Sem effs a
runFailInMockChainError = interpret $
  \(Fail s) -> throw $ MCEFailure s
