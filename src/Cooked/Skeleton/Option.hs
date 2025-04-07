-- | This modules provides a variety of options associated with a
-- 'Cooked.Skeleton.TxSkel'. These options mostly revolves around customizing
-- the default behavior of cooked-validators's transaction generation mechanism.
module Cooked.Skeleton.Option
  ( BalanceOutputPolicy (..),
    FeePolicy (..),
    BalancingPolicy (..),
    BalancingUtxos (..),
    RawModTx (..),
    EmulatorParamsModification (..),
    CollateralUtxos (..),
    AnchorResolution (..),
    applyEmulatorParamsModification,
    applyRawModOnBalancedTx,
    TxOpts (..),
    txOptUnsafeModTxL,
    txOptAutoSlotIncreaseL,
    txOptBalancingPolicyL,
    txOptBalanceOutputPolicyL,
    txOptFeePolicyL,
    txOptBalancingUtxosL,
    txOptEmulatorParamsModificationL,
    txOptCollateralUtxosL,
    txOptAnchorResolutionL,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator qualified as Emulator
import Cooked.Wallet
import Data.ByteString (ByteString)
import Data.Default
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
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

-- | Wraps a function that will be applied to a Cardano transaction after it has
-- been generated from this skeleton (and thus, after balancing has been
-- performed since it operates on skeletons).
newtype RawModTx
  = RawModTx (Cardano.Tx Cardano.ConwayEra -> Cardano.Tx Cardano.ConwayEra)

-- This instance always returns @False@, which is no problem, because 'Eq
-- TxSkel' is only used for tests that never depend on this comparison
instance Eq RawModTx where
  _ == _ = False

instance Show RawModTx where
  show (RawModTx _) = "RawModTxAfterBalancing"

-- | Applies a list of modifications right before the transaction is
-- submitted. The leftmost function in the argument list is applied first.
applyRawModOnBalancedTx :: [RawModTx] -> Cardano.Tx Cardano.ConwayEra -> Cardano.Tx Cardano.ConwayEra
applyRawModOnBalancedTx = foldl' (\acc (RawModTx f) -> acc . f) id

-- | Wraps a function that will temporarily change the emulator parameters for
-- the transaction's balancing and submission.
newtype EmulatorParamsModification = EmulatorParamsModification (Emulator.Params -> Emulator.Params)

-- | This instance always returns @False@, which is no problem, because 'Eq
-- TxSkel' is only used for tests that never depend on this comparison
instance Eq EmulatorParamsModification where
  _ == _ = False

instance Show EmulatorParamsModification where
  show EmulatorParamsModification {} = "EmulatorParamsModification <function>"

-- | Performs an 'EmulatorParamsModification' over an 'Emulator.Params'
applyEmulatorParamsModification :: Maybe EmulatorParamsModification -> Emulator.Params -> Emulator.Params
applyEmulatorParamsModification (Just (EmulatorParamsModification f)) = f
applyEmulatorParamsModification Nothing = id

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

-- | Describes how to resolve anchors in proposal procedures
data AnchorResolution
  = -- | Provide a map between urls and page content as Bytestring
    AnchorResolutionLocal (Map String ByteString)
  | -- | Allow online fetch of pages from a given URL. Important note: using
    -- this option is unsafe, as it requires a web connection and inherently
    -- prevents guarantees of reproducibily. Use at your own discretion.
    AnchorResolutionHttp
  deriving (Eq, Show)

instance Default AnchorResolution where
  def = AnchorResolutionLocal Map.empty

-- | Set of options to modify the behavior of generating and validating some
-- transaction.
data TxOpts = TxOpts
  { -- | Whether to increase the slot counter automatically on transaction
    -- submission.  This is useful for modelling transactions that could be
    -- submitted in parallel in reality, so there should be no explicit ordering
    -- of what comes first.
    --
    -- Default is @True@.
    txOptAutoSlotIncrease :: Bool,
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
    -- > txOptUnsafeModTx = [RawModTx Debug.Trace.traceShowId]
    --
    -- The leftmost function in the list is applied first.
    --
    -- Default is @[]@.
    txOptUnsafeModTx :: [RawModTx],
    -- | Whether to balance the transaction or not, and which wallet should
    -- provide/reclaim the missing and surplus value. Balancing ensures that
    --
    -- > input + mints == output + fees + burns
    --
    -- If you decide to set @txOptBalance = DoNotBalance@ you will have trouble
    -- satisfying that equation by hand unless you use @ManualFee@. You will
    -- likely see a error about value preservation.
    --
    -- Default is 'BalanceWithFirstSigner'
    txOptBalancingPolicy :: BalancingPolicy,
    -- | The fee to use when balancing the transaction
    --
    -- Default is 'AutoFeeComputation'
    txOptFeePolicy :: FeePolicy,
    -- | The 'BalanceOutputPolicy' to apply when balancing the transaction.
    --
    -- Default is 'AdjustExistingOutput'.
    txOptBalanceOutputPolicy :: BalanceOutputPolicy,
    -- | Which UTxOs to use during balancing. This can either be a precise list,
    -- or rely on automatic searches for utxos with values only belonging to the
    -- balancing wallet.
    --
    -- Default is 'BalancingUtxosFromBalancingWallet'.
    txOptBalancingUtxos :: BalancingUtxos,
    -- | Apply an arbitrary modification to the protocol parameters that are
    -- used to balance and submit the transaction. This is obviously a very
    -- unsafe thing to do if you want to preserve compatibility with the actual
    -- chain. It is useful mainly for testing purposes, when you might want to
    -- use extremely big transactions or transactions that exhaust the maximum
    -- execution budget. Such a thing could be accomplished with
    --
    -- > txOptEmulatorParamsModification = Just $ EmulatorParamsModification increaseTransactionLimits
    --
    -- for example.
    --
    -- Default is 'Nothing'.
    txOptEmulatorParamsModification :: Maybe EmulatorParamsModification,
    -- | Which utxos to use as collaterals. They can be given manually, or
    -- computed automatically from a given, or the balancing, wallet.
    --
    -- Default is 'CollateralUtxosFromBalancingWallet'
    txOptCollateralUtxos :: CollateralUtxos,
    -- | How to resolve anchor in proposal procedures
    --
    -- Default is 'AnchorResolutionLocal Map.Empty'
    txOptAnchorResolution :: AnchorResolution
  }
  deriving (Eq, Show)

-- | A lens to get or set the automatic slot increase option
makeLensesFor [("txOptAutoSlotIncrease", "txOptAutoSlotIncreaseL")] ''TxOpts

-- | A lens to get or set the Cardano transaction modifications option
makeLensesFor [("txOptUnsafeModTx", "txOptUnsafeModTxL")] ''TxOpts

-- | A lens to get or set the balancing policy option
makeLensesFor [("txOptBalancingPolicy", "txOptBalancingPolicyL")] ''TxOpts

-- | A lens to get or set the fee policy option
makeLensesFor [("txOptFeePolicy", "txOptFeePolicyL")] ''TxOpts

-- | A lens to get or set the handling of balancing outputs option
makeLensesFor [("txOptBalanceOutputPolicy", "txOptBalanceOutputPolicyL")] ''TxOpts

-- | A lens to get or set the balancing utxos option
makeLensesFor [("txOptBalancingUtxos", "txOptBalancingUtxosL")] ''TxOpts

-- | A lens to get or set the changes to protocol parameters option
makeLensesFor [("txOptEmulatorParamsModification", "txOptEmulatorParamsModificationL")] ''TxOpts

-- | A lens to get or set the collateral utxos option
makeLensesFor [("txOptCollateralUtxos", "txOptCollateralUtxosL")] ''TxOpts

-- | A lens to get or set the anchor resolution option
makeLensesFor [("txOptAnchorResolution", "txOptAnchorResolutionL")] ''TxOpts

instance Default TxOpts where
  def =
    TxOpts
      { txOptAutoSlotIncrease = True,
        txOptUnsafeModTx = [],
        txOptBalancingPolicy = def,
        txOptBalanceOutputPolicy = def,
        txOptFeePolicy = def,
        txOptBalancingUtxos = def,
        txOptEmulatorParamsModification = Nothing,
        txOptCollateralUtxos = def,
        txOptAnchorResolution = def
      }
