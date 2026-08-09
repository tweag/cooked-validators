-- | This modules provides a variety of options associated with a
-- 'Cooked.Skeleton.TxSkel'. These options mostly revolves around customizing
-- the default behavior of cooked-validators's transaction generation mechanism.
module Cooked.Skeleton.Option
  ( -- * Data types
    UserConstraints,
    BalanceOutputPolicy (..),
    FeePolicy (..),
    BalancingPolicy (..),
    BalancingUtxos (..),
    CollateralUtxos (..),
    TxSkelOpts (..),

    -- * Optics
    txSkelOptModTxL,
    txSkelOptBalancingPolicyL,
    txSkelOptBalanceOutputPolicyL,
    txSkelOptFeePolicyL,
    txSkelOptBalancingUtxosL,
    txSkelOptCollateralUtxosL,
    txSkelOptOptimizeFeeInCaseOfScriptFailuresL,
    txSkelOptMaxNbOfBalancingUtxosL,

    -- * Utilities
    txSkelOptAddModTx,
  )
where

import Cardano.Api qualified as Cardano
import Data.Default
import Data.Set (Set)
import Data.Typeable
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Set of constraints that need to be satisfied by users in options
type UserConstraints pkh =
  ( Script.ToPubKeyHash pkh,
    Show pkh,
    Eq pkh,
    Typeable pkh
  )

-- | What fee policy to use in the transaction.
data FeePolicy
  = -- | Use automatic fee computation. If balancing is activated, an optimal
    -- fee will be computed based on the transaction and existing utxos in the
    -- balancing user. Otherwise, the maximum transaction fee will be applied.
    AutoFeeComputation
  | -- | Provide a fee to the transaction. If the autobalancing is activated, it
    -- will be attempted around this fee, which might lead to failure if it is
    -- too low, otherwise, this fee will be given to transaction generation.
    ManualFee Integer
  deriving (Eq, Ord, Show)

instance Default FeePolicy where
  def = AutoFeeComputation

-- | Whether to adjust a potentially existing output to the balancing user
-- with the change during transaction balancing.
data BalanceOutputPolicy
  = -- | Try to adjust an existing public key output with the change. If no
    -- suitable output can be found, create a new change output.
    AdjustExistingOutput
  | -- | Do not change the existing outputs, always create a new change output.
    DontAdjustExistingOutput
  deriving (Eq, Ord, Show)

instance Default BalanceOutputPolicy where
  def = AdjustExistingOutput

-- | Which UTxOs to use when balancing. Note that utxos that are already known
-- by the skeleton being balanced (in the sense of
-- `Cooked.Skeleton.txSkelKnownTxOutRefs`, i.e. inputs and reference inputs)
-- will be filtered out during balancing.
data BalancingUtxos
  = -- | Use all UTxOs containing only a Value (no datum, no staking credential,
    -- and no reference script) belonging to the balancing user.
    BalancingUtxosFromBalancingUser
  | -- | Use the provided UTxOs. UTxOs belonging to scripts will be filtered out.
    BalancingUtxosFromSet (Set Api.TxOutRef)
  deriving (Eq, Ord, Show)

instance Default BalancingUtxos where
  def = BalancingUtxosFromBalancingUser

-- | Whether to balance the transaction or not, and which user to use to
-- provide outputs for balancing.
data BalancingPolicy where
  -- | Balance with the first signatory of the list of signatories
  BalanceWithFirstSignatory :: BalancingPolicy
  -- | Balance using a given user
  BalanceWith :: (UserConstraints pkh) => pkh -> BalancingPolicy
  -- | Do not perform balancing at all
  DoNotBalance :: BalancingPolicy

instance Eq BalancingPolicy where
  DoNotBalance == DoNotBalance = True
  BalanceWithFirstSignatory == BalanceWithFirstSignatory = True
  BalanceWith pkh == BalanceWith pkh1 = Script.toPubKeyHash pkh == Script.toPubKeyHash pkh1
  _ == _ = False

deriving instance Show BalancingPolicy

instance Default BalancingPolicy where
  def = BalanceWithFirstSignatory

-- | Describe which UTxOs to use as collaterals
data CollateralUtxos where
  -- | Rely on automated computation with only-value UTxOs from the balancing
  -- user. Return collaterals will be sent to this user.
  CollateralUtxosFromBalancingUser :: CollateralUtxos
  -- | Rely on automated computation with only-value UTxOs from a given
  -- user. Return collaterals will be sent to this user.
  CollateralUtxosFromUser :: (UserConstraints pkh) => pkh -> CollateralUtxos
  -- | Manually provide a set of candidate UTxOs to be used as collaterals
  -- alongside a user to send return collaterals back to.
  CollateralUtxosFromSet :: (UserConstraints pkh) => Set Api.TxOutRef -> pkh -> CollateralUtxos

instance Eq CollateralUtxos where
  CollateralUtxosFromSet set0 pkh == CollateralUtxosFromSet set1 pkh1 =
    Script.toPubKeyHash pkh == Script.toPubKeyHash pkh1 && set0 == set1
  CollateralUtxosFromUser pkh == CollateralUtxosFromUser pkh1 = Script.toPubKeyHash pkh == Script.toPubKeyHash pkh1
  CollateralUtxosFromBalancingUser == CollateralUtxosFromBalancingUser = True
  _ == _ = False

deriving instance Show CollateralUtxos

instance Default CollateralUtxos where
  def = CollateralUtxosFromBalancingUser

-- | Set of options to modify the behavior of generating and validating some
-- transaction.
data TxSkelOpts = TxSkelOpts
  { -- | Applies an arbitrary modification to a transaction after it has been
    -- adjusted, balanced and generated. This offers a hook for being able to
    -- alter a transaction in unforeseen ways.
    --
    -- One interesting use of this function is to observe a transaction just
    -- before it is being sent for validation, with
    --
    -- > txSkelOptModTx = Debug.Trace.traceShowId
    --
    -- Default is @id@.
    txSkelOptModTx :: Cardano.Tx Cardano.ConwayEra -> Cardano.Tx Cardano.ConwayEra,
    -- | Whether to balance the transaction or not, and which user should
    -- provide/reclaim the missing and surplus value.
    --
    -- If you decide to set @txSkelOptBalance = DoNotBalance@ you will have trouble
    -- satisfying the balancing equation by hand unless you use @ManualFee@.
    --
    -- Default is 'BalanceWithFirstSignatory'
    txSkelOptBalancingPolicy :: BalancingPolicy,
    -- | The fee to use when balancing the transaction
    --
    -- Default is 'AutoFeeComputation'
    txSkelOptFeePolicy :: FeePolicy,
    -- | The 'BalanceOutputPolicy' to apply when balancing the transaction.
    --
    -- Default is 'AdjustExistingOutput'.
    txSkelOptBalanceOutputPolicy :: BalanceOutputPolicy,
    -- | Which UTxOs to use during balancing. This can either be a precise list,
    -- or rely on automatic searches for utxos with values only belonging to the
    -- balancing user.
    --
    -- Default is 'BalancingUtxosFromBalancingUser'.
    txSkelOptBalancingUtxos :: BalancingUtxos,
    -- | Which utxos to use as collaterals. They can be given manually, or
    -- computed automatically from a given, or the balancing, user.
    --
    -- Default is 'CollateralUtxosFromBalancingUser'
    txSkelOptCollateralUtxos :: CollateralUtxos,
    -- | Whether to defer validation failures occurring during balancing
    -- (specifically during the computation of execution units) to the actual
    -- later submission of the transaction.
    --
    -- When set to @False@: the phase 2 validation failures will be caught as
    -- early as possible, typically during the first successful balancing
    -- attempt when the execution units are computed. This will shortcut the
    -- dychotomic search and return a balanced, non-optimized, skeleton, which
    -- is not going to pass phase 2 validation (only relevant when
    -- @txOptFeePolicy == AutoFeeComputation@).
    --
    -- When set to @True@: the phase 2 validation errors will be ignored during
    -- the balancing process. This will result in a worst performance (40%), but
    -- will allow the log to display an optimial balanced version of the failing
    -- `Cooked.Skeleton.TxSkel`, which would not be computed otherwise.
    --
    -- Default is `False`
    txSkelOptOptimizeFeeInCaseOfScriptFailures :: Bool,
    -- | The optional maximum number of Utxos that can be used during
    -- balancing. The algorithm which selects Utxos when permorming balancing is
    -- greedy. In the default use case where the are only a few wallets and
    -- Utxos (the most common case for testing purpose), this is fine. However,
    -- if the amount of candidate Utxos is big (let's say, bigger than 15), this
    -- is problematic. Use this option to limit the number of Utxos that can be
    -- used during the balancing process.
    --
    -- Alternatively, this can also be used to pilot balancing in some way. For
    -- instance, setting this option to @Just 1@ will result in a single Utxo
    -- added in the inputs of the transaction, if such a Utxo exist.
    --
    -- Default is @Nothing@
    txSkelOptMaxNbOfBalancingUtxos :: Maybe Integer
  }

-- | Comparing 'TxSkelOpts' is possible as long as we ignore modifications to the
-- generated transaction and the parameters.
instance Eq TxSkelOpts where
  (TxSkelOpts _ balancingPol feePol balOutputPol balUtxos colUtxos deferFailures maxNbBalUtxos)
    == (TxSkelOpts _ balancingPol' feePol' balOutputPol' balUtxos' colUtxos' deferFailures' maxNbBalUtxos') =
      balancingPol == balancingPol'
        && feePol == feePol'
        && balOutputPol == balOutputPol'
        && balUtxos == balUtxos'
        && colUtxos == colUtxos'
        && deferFailures == deferFailures'
        && maxNbBalUtxos == maxNbBalUtxos'

-- | Showing 'TxSkelOpts' is possible as long as we ignore modifications to the
-- generated transaction and the parameters.
instance Show TxSkelOpts where
  show (TxSkelOpts _ balancingPol feePol balOutputPol balUtxos colUtxos deferFailures maxNbBalUtxos) =
    show [show balancingPol, show feePol, show balOutputPol, show balUtxos, show colUtxos, show deferFailures, show maxNbBalUtxos]

-- | Focuses on the Cardano transaction modifications option of a 'TxSkelOpts'
makeLensesFor [("txSkelOptModTx", "txSkelOptModTxL")] ''TxSkelOpts

-- | Focuses on the balancing policy option of a 'TxSkelOpts'
makeLensesFor [("txSkelOptBalancingPolicy", "txSkelOptBalancingPolicyL")] ''TxSkelOpts

-- | Focuses on the fee policy option of a 'TxSkelOpts'
makeLensesFor [("txSkelOptFeePolicy", "txSkelOptFeePolicyL")] ''TxSkelOpts

-- | Focuses on the handling of balancing outputs option of a 'TxSkelOpts'
makeLensesFor [("txSkelOptBalanceOutputPolicy", "txSkelOptBalanceOutputPolicyL")] ''TxSkelOpts

-- | Focuses on the balancing utxos option of a 'TxSkelOpts'
makeLensesFor [("txSkelOptBalancingUtxos", "txSkelOptBalancingUtxosL")] ''TxSkelOpts

-- | Focuses on the collateral utxos option of a 'TxSkelOpts'
makeLensesFor [("txSkelOptCollateralUtxos", "txSkelOptCollateralUtxosL")] ''TxSkelOpts

-- | Focuses on the deferring of the failures option of a 'TxSkelOpts'
makeLensesFor [("txSkelOptOptimizeFeeInCaseOfScriptFailures", "txSkelOptOptimizeFeeInCaseOfScriptFailuresL")] ''TxSkelOpts

-- | Focuses on the max nb of balancing Utxos option of a 'TxSkelOpts'
makeLensesFor [("txSkelOptMaxNbOfBalancingUtxos", "txSkelOptMaxNbOfBalancingUtxosL")] ''TxSkelOpts

instance Default TxSkelOpts where
  def =
    TxSkelOpts
      { txSkelOptModTx = id,
        txSkelOptBalancingPolicy = def,
        txSkelOptBalanceOutputPolicy = def,
        txSkelOptFeePolicy = def,
        txSkelOptBalancingUtxos = def,
        txSkelOptCollateralUtxos = def,
        txSkelOptOptimizeFeeInCaseOfScriptFailures = False,
        txSkelOptMaxNbOfBalancingUtxos = Nothing
      }

-- | Appends a transaction modification to the given 'TxSkelOpts'
txSkelOptAddModTx :: (Cardano.Tx Cardano.ConwayEra -> Cardano.Tx Cardano.ConwayEra) -> TxSkelOpts -> TxSkelOpts
txSkelOptAddModTx modTx = over txSkelOptModTxL (modTx .)
