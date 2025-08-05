-- | This modules provides a variety of options associated with a
-- 'Cooked.Skeleton.TxSkel'. These options mostly revolves around customizing
-- the default behavior of cooked-validators's transaction generation mechanism.
module Cooked.Skeleton.Option
  ( -- * Data types
    BalanceOutputPolicy (..),
    FeePolicy (..),
    BalancingPolicy (..),
    BalancingUtxos (..),
    CollateralUtxos (..),
    TxSkelOpts (..),

    -- * Optics
    txSkelOptModTxL,
    txSkelOptAutoSlotIncreaseL,
    txSkelOptBalancingPolicyL,
    txSkelOptBalanceOutputPolicyL,
    txSkelOptFeePolicyL,
    txSkelOptBalancingUtxosL,
    txSkelOptModParamsL,
    txSkelOptCollateralUtxosL,

    -- * Utilities
    txSkelOptAddModTx,
    txSkelOptAddModParams,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator qualified as Emulator
import Cooked.Wallet
import Data.Default
import Data.Set (Set)
import Optics.Core
import Optics.TH
import PlutusLedgerApi.V3 qualified as Api

-- | What fee policy to use in the transaction.
data FeePolicy
  = -- | Use automatic fee computation. If balancing is activated, an optimal
    -- fee will be computed based on the transaction and existing utxos in the
    -- balancing wallet. Otherwise, the maximum transaction fee will be applied.
    AutoFeeComputation
  | -- | Provide a fee to the transaction. If the autobalancing is activated, it
    -- will be attempted around this fee, which might lead to failure if it is
    -- too low, otherwise, this fee will be given to transaction generation.
    ManualFee Integer
  deriving (Eq, Ord, Show)

instance Default FeePolicy where
  def = AutoFeeComputation

-- | Whether to adjust a potentially existing output to the balancing wallet
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
    -- and no reference script) belonging to the balancing wallet.
    BalancingUtxosFromBalancingWallet
  | -- | Use the provided UTxOs. UTxOs belonging to scripts will be filtered out
    BalancingUtxosFromSet (Set Api.TxOutRef)
  deriving (Eq, Ord, Show)

instance Default BalancingUtxos where
  def = BalancingUtxosFromBalancingWallet

-- | Whether to balance the transaction or not, and which wallet to use to
-- provide outputs for balancing.
data BalancingPolicy
  = -- | Balance with the first signer of the list of signers
    BalanceWithFirstSigner
  | -- | Balance using a given wallet
    BalanceWith Wallet
  | -- | Do not perform balancing at all
    DoNotBalance
  deriving (Eq, Ord, Show)

instance Default BalancingPolicy where
  def = BalanceWithFirstSigner

-- | Describe which UTxOs to use as collaterals
data CollateralUtxos
  = -- | Rely on automated computation with only-value UTxOs from the balancing
    -- wallet. Return collaterals will be sent to this wallet.
    CollateralUtxosFromBalancingWallet
  | -- | Rely on automated computation with only-value UTxOs from a given
    -- wallet. Return collaterals will be sent to this wallet.
    CollateralUtxosFromWallet Wallet
  | -- | Manually provide a set of candidate UTxOs to be used as collaterals
    -- alongside a wallet to send return collaterals back to.
    CollateralUtxosFromSet (Set Api.TxOutRef) Wallet
  deriving (Eq, Show)

instance Default CollateralUtxos where
  def = CollateralUtxosFromBalancingWallet

-- | Set of options to modify the behavior of generating and validating some
-- transaction.
data TxSkelOpts = TxSkelOpts
  { -- | Whether to increase the slot counter automatically on transaction
    -- submission.  This is useful for modelling transactions that could be
    -- submitted in parallel in reality, so there should be no explicit ordering
    -- of what comes first.
    --
    -- Default is @True@.
    txSkelOptAutoSlotIncrease :: Bool,
    -- | Applies an arbitrary modification to a transaction after it has been
    -- potentially adjusted and balanced. The name of this option contains
    -- /unsafe/ to draw attention to the fact that modifying a transaction at
    -- that stage might make it invalid. Still, this offers a hook for being
    -- able to alter a transaction in unforeseen ways. It is mostly used to test
    -- contracts that have been written for custom PABs.
    --
    -- One interesting use of this function is to observe a transaction just
    -- before it is being sent for validation, with
    --
    -- > txSkelOptModTx = [RawModTx Debug.Trace.traceShowId]
    --
    -- The leftmost function in the list is applied first.
    --
    -- Default is @[]@.
    txSkelOptModTx :: Cardano.Tx Cardano.ConwayEra -> Cardano.Tx Cardano.ConwayEra,
    -- | Whether to balance the transaction or not, and which wallet should
    -- provide/reclaim the missing and surplus value. Balancing ensures that
    --
    -- > input + mints == output + fees + burns
    --
    -- If you decide to set @txSkelOptBalance = DoNotBalance@ you will have trouble
    -- satisfying that equation by hand unless you use @ManualFee@. You will
    -- likely see a error about value preservation.
    --
    -- Default is 'BalanceWithFirstSigner'
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
    -- balancing wallet.
    --
    -- Default is 'BalancingUtxosFromBalancingWallet'.
    txSkelOptBalancingUtxos :: BalancingUtxos,
    -- | Apply an arbitrary modification to the protocol parameters that are
    -- used to balance and submit the transaction. This is obviously a very
    -- unsafe thing to do if you want to preserve compatibility with the actual
    -- chain. It is useful mainly for testing purposes, when you might want to
    -- use extremely big transactions or transactions that exhaust the maximum
    -- execution budget. Such a thing could be accomplished with
    --
    -- > txSkelOptModParams = Just $ ModParams increaseTransactionLimits
    --
    -- for example.
    --
    -- Default is 'Nothing'.
    txSkelOptModParams :: Emulator.Params -> Emulator.Params,
    -- | Which utxos to use as collaterals. They can be given manually, or
    -- computed automatically from a given, or the balancing, wallet.
    --
    -- Default is 'CollateralUtxosFromBalancingWallet'
    txSkelOptCollateralUtxos :: CollateralUtxos
  }

-- | Comparing 'TxSkelOpts' is possible as long as we ignore modifications to the
-- generated transaction and the parameters.
instance Eq TxSkelOpts where
  (TxSkelOpts slotIncrease _ balancingPol feePol balOutputPol balUtxos _ colUtxos)
    == (TxSkelOpts slotIncrease' _ balancingPol' feePol' balOutputPol' balUtxos' _ colUtxos') =
      slotIncrease == slotIncrease'
        && balancingPol == balancingPol'
        && feePol == feePol'
        && balOutputPol == balOutputPol'
        && balUtxos == balUtxos'
        && colUtxos == colUtxos'

-- | Showing 'TxSkelOpts' is possible as long as we ignore modifications to the
-- generated transaction and the parameters.
instance Show TxSkelOpts where
  show (TxSkelOpts slotIncrease _ balancingPol feePol balOutputPol balUtxos _ colUtxos) =
    show [show slotIncrease, show balancingPol, show feePol, show balOutputPol, show balUtxos, show colUtxos]

-- | A lens to get or set the automatic slot increase option
makeLensesFor [("txSkelOptAutoSlotIncrease", "txSkelOptAutoSlotIncreaseL")] ''TxSkelOpts

-- | A lens to get or set the Cardano transaction modifications option
makeLensesFor [("txSkelOptModTx", "txSkelOptModTxL")] ''TxSkelOpts

-- | A lens to get or set the balancing policy option
makeLensesFor [("txSkelOptBalancingPolicy", "txSkelOptBalancingPolicyL")] ''TxSkelOpts

-- | A lens to get or set the fee policy option
makeLensesFor [("txSkelOptFeePolicy", "txSkelOptFeePolicyL")] ''TxSkelOpts

-- | A lens to get or set the handling of balancing outputs option
makeLensesFor [("txSkelOptBalanceOutputPolicy", "txSkelOptBalanceOutputPolicyL")] ''TxSkelOpts

-- | A lens to get or set the balancing utxos option
makeLensesFor [("txSkelOptBalancingUtxos", "txSkelOptBalancingUtxosL")] ''TxSkelOpts

-- | A lens to get or set the changes to protocol parameters option
makeLensesFor [("txSkelOptModParams", "txSkelOptModParamsL")] ''TxSkelOpts

-- | A lens to get or set the collateral utxos option
makeLensesFor [("txSkelOptCollateralUtxos", "txSkelOptCollateralUtxosL")] ''TxSkelOpts

-- | A lens to get or set the anchor resolution option
makeLensesFor [("txSkelOptAnchorResolution", "txSkelOptAnchorResolutionL")] ''TxSkelOpts

instance Default TxSkelOpts where
  def =
    TxSkelOpts
      { txSkelOptAutoSlotIncrease = True,
        txSkelOptModTx = id,
        txSkelOptBalancingPolicy = def,
        txSkelOptBalanceOutputPolicy = def,
        txSkelOptFeePolicy = def,
        txSkelOptBalancingUtxos = def,
        txSkelOptModParams = id,
        txSkelOptCollateralUtxos = def
      }

-- | Appends a transaction modification to the given 'TxSkelOpts'
txSkelOptAddModTx :: (Cardano.Tx Cardano.ConwayEra -> Cardano.Tx Cardano.ConwayEra) -> TxSkelOpts -> TxSkelOpts
txSkelOptAddModTx modTx = over txSkelOptModTxL (modTx .)

-- | Appends a parameters modification to the given 'TxSkelOpts'
txSkelOptAddModParams :: (Emulator.Params -> Emulator.Params) -> TxSkelOpts -> TxSkelOpts
txSkelOptAddModParams modParams = over txSkelOptModParamsL (modParams .)
