-- | This module exposes the errors that can be raised during a mockchain run
module Cooked.Runtime.Error
  ( -- * Mockchain errors
    BalancingError (..),
    ChainError (..),

    -- * Interpreting Fail into @Error ChainError@
    runFailInChainError,
  )
where

import Cooked.Skeleton.User
import Cooked.Utilities.Aliases
import Ledger.Tx qualified as P.Ledger
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail

-- | Errors that can be produced during balancing
data BalancingError
  = -- | The balancing user theoretically has enough funds to balancing the
    -- transaction, but this balancing results in a surplus payment which they
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
data ChainError
  = -- | Failures occurring while computing execution units
    CEExUnitsFailures ExUnitsFailures
  | -- | Failures occurring while submitting the transaction for validation
    CESubmissionFailures SubmissionFailures
  | -- | Balancing errors
    CEBalancingError BalancingError
  | -- | Translating a skeleton element to its Cardano counterpart failed
    CEToCardanoError P.Ledger.ToCardanoError
  | -- | The required reference script is missing from a witness utxo
    CEWrongReferenceScriptError Api.TxOutRef Api.ScriptHash (Maybe Api.ScriptHash)
  | -- | A UTxO is missing from the mockchain state
    CEUnknownOutRef Api.TxOutRef
  | -- | An attempt to invoke an unsupported feature has been made
    CEUnsupportedFeature String
  | -- | An attempt to spend a script output whose datum is only known by its
    -- hash, which does not provide the datum content required by the witness
    CESpendingHashOnlyDatum Api.TxOutRef Api.DatumHash
  | -- | An attempt to spend a script output whose script is only known by its
    -- hash, without providing the full script through a matching reference input
    CESpendingHashOnlyScript Api.TxOutRef Api.ScriptHash
  | -- | Used to provide 'MonadFail' instances.
    CEFailure String
  deriving (Show, Eq)

-- | Interpreting failures in terms of `ChainError`
runFailInChainError ::
  forall effs a.
  (Member (Error ChainError) effs) =>
  Sem (Fail : effs) a ->
  Sem effs a
runFailInChainError = interpret $
  \(Fail s) -> throw $ CEFailure s
