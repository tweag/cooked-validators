-- | This module exposes the errors that can be raised during a mockchain run
module Cooked.MockChain.Error
  ( -- * Mockchain errors
    BalancingError (..),
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

-- | Errors that can be produced during balancing
data BalancingError
  = -- | The balancing user theoretically has enough funds to balancing the
    -- trasaction, but this balancing results in a surplus payment which they
    -- cannot afford ADA-wise.
    NotEnoughFundForExtraMinAda Peer
  | -- | The balancing does not have enough funds to sustain the fee required to
    -- balance the transaction.
    NotEnoughFundForProperFee Peer
  | -- | The balancing wallet does not have enough funds to balance the
    -- transaction
    NotEnoughFund Peer Api.Value
  | -- | The provided of collateral UTxOs does not have enough funds to cover
    -- the potential collateral cost
    NoSuitableCollateral Integer Integer Api.Value
  | -- | The balancing user has not be provided, but the balancing requires it
    MissingBalancingUser
  deriving (Show, Eq)

-- | Errors that can be produced by the blockchain
data MockChainError
  = -- | Validation errors, either in Phase 1 or Phase 2
    MCEValidationError Ledger.ValidationPhase Ledger.ValidationError
  | -- | Balancing errors
    MCEBalancingError BalancingError
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
