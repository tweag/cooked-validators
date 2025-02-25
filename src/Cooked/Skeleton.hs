{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides the description of a transaction skeleton. We have our
-- own representation of a transaction for three main reasons:
--
-- - our transaction skeletons are typed (datums, validators, outputs...)
--
-- - with our own wrapper, we are less affected by plutus updates
--
-- - we can have default or automated behavior for the parts of the transactions
-- that are less relevant to testing, such as collaterals or fees
module Cooked.Skeleton
  ( LabelConstrs,
    TxLabel (..),
    BalanceOutputPolicy (..),
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
    txOptEnsureMinAdaL,
    txOptUnsafeModTxL,
    txOptAutoSlotIncreaseL,
    txOptBalancingPolicyL,
    txOptBalanceOutputPolicyL,
    txOptFeePolicyL,
    txOptBalancingUtxosL,
    txOptEmulatorParamsModificationL,
    txOptCollateralUtxosL,
    txOptAnchorResolutionL,
    txOptAutoReferenceScriptsL,
    TxSkelMints,
    addToTxSkelMints,
    txSkelMintsToList,
    txSkelMintsFromList,
    txSkelMintsFromList',
    txSkelMintsValue,
    txSkelOutValueL,
    txSkelOutDatumL,
    txSkelOutValue,
    txSkelOutValidator,
    TxSkelOutDatumConstrs,
    TxSkelOutDatum (..),
    TxSkelOut (..),
    txSkelOutTypedDatum,
    txSkelOutUntypedDatum,
    paysPK,
    paysScript,
    paysScriptInlineDatum,
    paysScriptUnresolvedDatumHash,
    paysScriptNoDatum,
    receives,
    (&>),
    withDatum,
    withInlineDatum,
    withUnresolvedDatumHash,
    withReferenceScript,
    withStakingCredential,
    TxSkelRedeemer (..),
    Redeemer (..),
    RedeemerConstrs,
    withReferenceInput,
    TxParameterChange (..),
    TxGovAction (..),
    TxSkelProposal (..),
    txSkelProposalsL,
    txSkelProposalAddressL,
    txSkelProposalActionL,
    txSkelProposalWitnessL,
    txSkelProposalAnchorL,
    TxSkelWithdrawals,
    txSkelWithdrawnValue,
    txSkelWithdrawalsScripts,
    pkWithdrawal,
    scriptWithdrawal,
    TxSkel (..),
    txSkelLabelL,
    txSkelOptsL,
    txSkelMintsL,
    txSkelValidityRangeL,
    txSkelSignersL,
    txSkelInsL,
    txSkelInsReferenceL,
    txSkelOutsL,
    txSkelWithdrawalsL,
    txSkelTemplate,
    txSkelDataInOutputs,
    txSkelValidatorsInOutputs,
    txSkelOutOwnerTypeP,
    txSkelOutputDatumTypeAT,
    SkelContext (..),
    txSkelKnownTxOutRefs,
    simpleTxSkelProposal,
    withWitness,
    withAnchor,
    txSkelValueInOutputs,
    txSkelReferenceScripts,
    txSkelReferenceTxOutRefs,
    someTxSkelRedeemer,
    emptyTxSkelRedeemer,
    toTypedRedeemer,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator qualified as Emulator
import Control.Applicative
import Control.Monad
import Cooked.Conversion
import Cooked.Output
import Cooked.Pretty.Class
import Cooked.Wallet
import Data.ByteString (ByteString)
import Data.Default
import Data.Either
import Data.Either.Combinators
import Data.Function
import Data.List (foldl')
import Data.List.NonEmpty qualified as NEList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (cast)
import Ledger.Slot qualified as Ledger
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script hiding (validatorHash)
import Plutus.Script.Utils.Value qualified as Script hiding (adaSymbol, adaToken)
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (NonZero (..))
import Type.Reflection

-- * Transaction labels

type LabelConstrs x = (PrettyCooked x, Show x, Typeable x, Eq x, Ord x)

data TxLabel where
  TxLabel :: (LabelConstrs x) => x -> TxLabel

instance Eq TxLabel where
  a == x = compare a x == EQ

instance Show TxLabel where
  show (TxLabel x) = show x

instance PrettyCooked TxLabel where
  prettyCookedOpt opts (TxLabel x) = prettyCookedOpt opts x

instance Ord TxLabel where
  compare (TxLabel a) (TxLabel x) =
    case compare (SomeTypeRep (typeOf a)) (SomeTypeRep (typeOf x)) of
      LT -> LT
      GT -> GT
      EQ -> case typeOf a `eqTypeRep` typeOf x of
        Just HRefl -> compare a x
        -- This can never happen, since 'eqTypeRep' is implemented in terms of
        -- '==' on the type representation:
        Nothing -> error "Type representations compare as EQ, but are not eqTypeRep"

-- * Transaction options

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
-- by the skeleton being balanced (in the sense of `txSkelKnownTxOutRefs`,
-- i.e. inputs and reference inputs) will be filtered out during balancing.
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
-- provide outputs for balancing. Either the first signer or an explicit
-- wallet. In the second case, this wallet must be a signer of the transaction.
data BalancingPolicy
  = BalanceWithFirstSigner
  | BalanceWith Wallet
  | DoNotBalance
  deriving (Eq, Ord, Show)

instance Default BalancingPolicy where
  def = BalanceWithFirstSigner

-- | Wraps a function that will be applied to a transaction right before
-- submission, and after balancing.
newtype RawModTx
  = RawModTxAfterBalancing (Cardano.Tx Cardano.ConwayEra -> Cardano.Tx Cardano.ConwayEra)

-- This instance always returns @False@, which is no problem, because 'Eq
-- TxSkel' is only used for tests that never depend on this comparison
instance Eq RawModTx where
  _ == _ = False

instance Show RawModTx where
  show (RawModTxAfterBalancing _) = "RawModTxAfterBalancing"

-- | Applies a list of modifications right before the transaction is
-- submitted. The leftmost function in the argument list is applied first.
applyRawModOnBalancedTx :: [RawModTx] -> Cardano.Tx Cardano.ConwayEra -> Cardano.Tx Cardano.ConwayEra
applyRawModOnBalancedTx = foldl' (\acc (RawModTxAfterBalancing f) -> acc . f) id

-- | Wraps a function that will temporarily change the emulator parameters for
-- the transaction's balancing and submission.
newtype EmulatorParamsModification = EmulatorParamsModification (Emulator.Params -> Emulator.Params)

-- This instance always returns @False@, which is no problem, because 'Eq
-- TxSkel' is only used for tests that never depend on this comparison
instance Eq EmulatorParamsModification where
  _ == _ = False

instance Show EmulatorParamsModification where
  show EmulatorParamsModification {} = "EmulatorParamsModification <function>"

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
  { -- | Performs an adjustment to unbalanced transactions, making sure every
    -- UTxO that is produced has the necessary minimum amount of Ada.
    --
    -- Default is @False@.
    txOptEnsureMinAda :: Bool,
    -- | Whether to increase the slot counter automatically on transaction
    -- submission.  This is useful for modelling transactions that could be
    -- submitted in parallel in reality, so there should be no explicit ordering
    -- of what comes first.
    --
    -- Default is @True@.
    txOptAutoSlotIncrease :: Bool,
    -- | Applies an arbitrary modification to a transaction after it has been
    -- potentially adjusted ('txOptEnsureMinAda') and balanced. The name of this
    -- option contains /unsafe/ to draw attention to the fact that modifying a
    -- transaction at that stage might make it invalid. Still, this offers a
    -- hook for being able to alter a transaction in unforeseen ways. It is
    -- mostly used to test contracts that have been written for custom PABs.
    --
    -- One interesting use of this function is to observe a transaction just
    -- before it is being sent for validation, with
    --
    -- > txOptUnsafeModTx = [RawModTxAfterBalancing Debug.Trace.traceShowId]
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
    -- Default is 'AutomaticFeeComputation'
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
    txOptAnchorResolution :: AnchorResolution,
    -- | Whether to automatically fill up reference inputs in redeemers when
    -- they contain the right reference script. This will imply going through
    -- all the known utxos with reference scripts and compare their hashes, thus
    -- will slightly reduce performance.
    --
    -- Defaut is 'False'.
    txOptAutoReferenceScripts :: Bool
  }
  deriving (Eq, Show)

makeLensesFor
  [ ("txOptEnsureMinAda", "txOptEnsureMinAdaL"),
    ("txOptAutoSlotIncrease", "txOptAutoSlotIncreaseL"),
    ("txOptUnsafeModTx", "txOptUnsafeModTxL"),
    ("txOptBalancingPolicy", "txOptBalancingPolicyL"),
    ("txOptFeePolicy", "txOptFeePolicyL"),
    ("txOptBalanceOutputPolicy", "txOptBalanceOutputPolicyL"),
    ("txOptBalancingUtxos", "txOptBalancingUtxosL"),
    ("txOptEmulatorParamsModification", "txOptEmulatorParamsModificationL"),
    ("txOptCollateralUtxos", "txOptCollateralUtxosL"),
    ("txOptAnchorResolution", "txOptAnchorResolutionL"),
    ("txOptAutoReferenceScripts", "txOptAutoReferenceScriptsL")
  ]
  ''TxOpts

instance Default TxOpts where
  def =
    TxOpts
      { txOptEnsureMinAda = False,
        txOptAutoSlotIncrease = True,
        txOptUnsafeModTx = [],
        txOptBalancingPolicy = def,
        txOptBalanceOutputPolicy = def,
        txOptFeePolicy = def,
        txOptBalancingUtxos = def,
        txOptEmulatorParamsModification = Nothing,
        txOptCollateralUtxos = def,
        txOptAnchorResolution = def,
        txOptAutoReferenceScripts = False
      }

-- * Redeemers for transaction inputs

type RedeemerConstrs redeemer =
  ( Api.ToData redeemer,
    Show redeemer,
    PrettyCooked redeemer,
    PlutusTx.Eq redeemer,
    Typeable redeemer
  )

data Redeemer where
  EmptyRedeemer :: Redeemer
  SomeRedeemer :: (RedeemerConstrs redeemer) => redeemer -> Redeemer

deriving instance (Show Redeemer)

instance Eq Redeemer where
  EmptyRedeemer == EmptyRedeemer = True
  (SomeRedeemer r1) == (SomeRedeemer r2) =
    case typeOf r1 `eqTypeRep` typeOf r2 of
      Just HRefl -> r1 PlutusTx.== r2
      Nothing -> False
  _ == _ = False

data TxSkelRedeemer = TxSkelRedeemer
  { txSkelRedeemer :: Redeemer,
    -- An optional input containing a reference script
    txSkelReferenceInput :: Maybe Api.TxOutRef
  }
  deriving (Show, Eq)

-- Attempts to cast a redeemer to a certain type
toTypedRedeemer :: (Typeable a) => Redeemer -> Maybe a
toTypedRedeemer (SomeRedeemer red) = cast red
toTypedRedeemer EmptyRedeemer = Nothing

-- Two helpers to create skeleton redeemers
someTxSkelRedeemer :: (RedeemerConstrs redeemer) => redeemer -> TxSkelRedeemer
someTxSkelRedeemer a = TxSkelRedeemer (SomeRedeemer a) Nothing

emptyTxSkelRedeemer :: TxSkelRedeemer
emptyTxSkelRedeemer = TxSkelRedeemer EmptyRedeemer Nothing

-- Additional helper to specify a given reference input. As reference inputs are
-- automatically attached during transaction generation when they contain the
-- right scripts by default, there are only 3 cases where this can be useful:
-- - The reliance on a reference script needs to be made explicit
-- - A wrong reference script somehow needs to be attached
-- - The automated attachement of reference inputs has been disabled using the
-- `txOptAutoReferenceScripts` option

withReferenceInput :: TxSkelRedeemer -> Api.TxOutRef -> TxSkelRedeemer
withReferenceInput red ref = red {txSkelReferenceInput = Just ref}

-- * Description of the Governance actions (or proposal procedures)

-- These are all the protocol parameters. They are taken from
-- https://github.com/IntersectMBO/cardano-ledger/blob/c4fbc05999866fea7c0cb1b211fd5288f286b95d/eras/conway/impl/cddl-files/conway.cddl#L381-L412
-- and will most likely change in future eras.
data TxParameterChange where
  -- | The linear factor for the minimum fee calculation
  FeePerByte :: Integer -> TxParameterChange
  -- | The constant factor for the minimum fee calculation
  FeeFixed :: Integer -> TxParameterChange
  -- | Maximal block body size
  MaxBlockBodySize :: Integer -> TxParameterChange
  -- | Maximal transaction size
  MaxTxSize :: Integer -> TxParameterChange
  -- | Maximal block header size
  MaxBlockHeaderSize :: Integer -> TxParameterChange
  -- | The amount of a key registration deposit
  KeyDeposit :: Integer -> TxParameterChange
  -- | The amount of a pool registration deposit
  PoolDeposit :: Integer -> TxParameterChange
  -- | Maximum number of epochs in the future a pool retirement is allowed to
  -- be scheduled future for.
  PoolRetirementMaxEpoch :: Integer -> TxParameterChange
  -- | Desired number of pools
  PoolNumber :: Integer -> TxParameterChange
  -- | Pool influence
  PoolInfluence :: Rational -> TxParameterChange
  -- | Monetary expansion
  MonetaryExpansion :: Rational -> TxParameterChange
  -- | Treasury expansion
  TreasuryCut :: Rational -> TxParameterChange
  -- | Minimum Stake Pool Cost
  MinPoolCost :: Integer -> TxParameterChange
  -- | Cost in lovelace per byte of UTxO storage
  CoinsPerUTxOByte :: Integer -> TxParameterChange
  -- | Cost models for non-native script languages
  CostModels ::
    { cmPlutusV1Costs :: [Integer],
      cmPlutusV2Costs :: [Integer],
      cmPlutusV3Costs :: [Integer]
    } ->
    TxParameterChange
  -- | Prices of execution units
  Prices ::
    { pMemoryCost :: Rational,
      pStepCost :: Rational
    } ->
    TxParameterChange
  -- | Max total script execution resources units allowed per tx
  MaxTxExUnits ::
    { mteuMemory :: Integer,
      mteuSteps :: Integer
    } ->
    TxParameterChange
  -- | Max total script execution resources units allowed per block
  MaxBlockExUnits ::
    { mbeuMemory :: Integer,
      mbeuSteps :: Integer
    } ->
    TxParameterChange
  -- | Max size of a Value in an output
  MaxValSize :: Integer -> TxParameterChange
  -- | Percentage of the txfee which must be provided as collateral when
  -- including non-native scripts.
  CollateralPercentage :: Integer -> TxParameterChange
  -- | Maximum number of collateral inputs allowed in a transaction
  MaxCollateralInputs :: Integer -> TxParameterChange
  -- | Thresholds for pool votes
  PoolVotingThresholds ::
    { pvtMotionNoConfidence :: Rational,
      pvtCommitteeNormal :: Rational,
      pvtCommitteeNoConfidence :: Rational,
      pvtHardFork :: Rational,
      pvtSecurityGroup :: Rational
    } ->
    TxParameterChange
  -- | Thresholds for DRep votes
  DRepVotingThresholds ::
    { drvtMotionNoConfidence :: Rational,
      drvtCommitteeNormal :: Rational,
      drvtCommitteeNoConfidence :: Rational,
      drvtUpdateConstitution :: Rational,
      drvtHardForkInitialization :: Rational,
      drvtNetworkGroup :: Rational,
      drvtEconomicGroup :: Rational,
      drvtTechnicalGroup :: Rational,
      drvtGovernanceGroup :: Rational,
      drvtTreasuryWithdrawal :: Rational
    } ->
    TxParameterChange
  -- | Minimum size of the Constitutional Committee
  CommitteeMinSize :: Integer -> TxParameterChange
  -- | The Constitutional Committee Term limit in number of Slots
  CommitteeMaxTermLength :: Integer -> TxParameterChange
  -- | Gov action lifetime in number of Epochs
  GovActionLifetime :: Integer -> TxParameterChange
  -- | The amount of the Gov Action deposit
  GovActionDeposit :: Integer -> TxParameterChange
  -- | The amount of a DRep registration deposit
  DRepRegistrationDeposit :: Integer -> TxParameterChange
  -- | The number of Epochs that a DRep can perform no activity without losing
  -- their @Active@ status.
  DRepActivity :: Integer -> TxParameterChange
  -- Reference scripts fee for the minimum fee calculation
  -- will exist later on MinFeeRefScriptCostPerByte :: Integer -> TxParameterChange
  deriving (Show, Eq)

data TxGovAction where
  -- If several parameter changes are of the same kind, only the last
  -- one will take effect
  TxGovActionParameterChange :: [TxParameterChange] -> TxGovAction
  TxGovActionHardForkInitiation :: Api.ProtocolVersion -> TxGovAction
  TxGovActionTreasuryWithdrawals :: Map Api.Credential Api.Lovelace -> TxGovAction
  TxGovActionNoConfidence :: TxGovAction
  TxGovActionUpdateCommittee :: [Api.ColdCommitteeCredential] -> Map Api.ColdCommitteeCredential Integer -> PlutusTx.Rational -> TxGovAction
  TxGovActionNewConstitution :: Api.Constitution -> TxGovAction
  deriving (Show, Eq)

data TxSkelProposal where
  TxSkelProposal ::
    { -- | Whatever credential will get back the deposit
      txSkelProposalAddress :: Api.Address,
      -- | The proposed action
      txSkelProposalAction :: TxGovAction,
      -- | An optional script (typically the constitution script) to witness the
      -- proposal and validate it. Only parameter changes and treasury
      -- withdrawals can be subject to such a validation and transactions will
      -- not pass validation phase 1 if other actions are given a witness.
      txSkelProposalWitness :: Maybe (Script.Versioned Script.Script, TxSkelRedeemer),
      -- | An optional anchor to be given as additional data. It should
      -- correspond to the URL of a web page
      txSkelProposalAnchor :: Maybe String
    } ->
    TxSkelProposal
  deriving (Show, Eq)

makeLensesFor
  [ ("txSkelProposalAddress", "txSkelProposalAddressL"),
    ("txSkelProposalAction", "txSkelProposalActionL"),
    ("txSkelProposalWitness", "txSkelProposalWitnessL"),
    ("txSkelProposalAnchor", "txSkelProposalAnchorL")
  ]
  ''TxSkelProposal

simpleTxSkelProposal :: (ToAddress a) => a -> TxGovAction -> TxSkelProposal
simpleTxSkelProposal a govAction = TxSkelProposal (toAddress a) govAction Nothing Nothing

withWitness :: (ToVersionedScript a) => TxSkelProposal -> (a, TxSkelRedeemer) -> TxSkelProposal
withWitness prop (s, red) = prop {txSkelProposalWitness = Just (toVersionedScript s, red)}

withAnchor :: TxSkelProposal -> String -> TxSkelProposal
withAnchor prop url = prop {txSkelProposalAnchor = Just url}

-- * Description of the Withdrawals

-- | Withdrawals associate either a script or a private key with a redeemer and
-- a certain amount of ada. Note that the redeemer will be ignored in the case
-- of a private key.
type TxSkelWithdrawals =
  Map
    (Either (Script.Versioned Script.Script) Api.PubKeyHash)
    (TxSkelRedeemer, Script.Ada)

txSkelWithdrawnValue :: TxSkel -> Api.Value
txSkelWithdrawnValue = mconcat . (toValue . snd . snd <$>) . Map.toList . txSkelWithdrawals

txSkelWithdrawalsScripts :: TxSkel -> [Script.Versioned Script.Script]
txSkelWithdrawalsScripts = fst . partitionEithers . (fst <$>) . Map.toList . txSkelWithdrawals

pkWithdrawal :: (ToPubKeyHash pkh) => pkh -> Script.Ada -> TxSkelWithdrawals
pkWithdrawal pkh amount = Map.singleton (Right $ toPubKeyHash pkh) (emptyTxSkelRedeemer, amount)

scriptWithdrawal :: (ToVersionedScript script) => script -> TxSkelRedeemer -> Script.Ada -> TxSkelWithdrawals
scriptWithdrawal script red amount = Map.singleton (Left $ toVersionedScript script) (red, amount)

-- * Description of the Minting

-- | A description of what a transaction mints. For every policy, there can only
-- be one 'TxSkelRedeemer', and if there is, there must be some token names, each
-- with a non-zero amount of tokens.
--
-- You'll probably not construct this by hand, but use 'txSkelMintsFromList'.
type TxSkelMints =
  Map
    (Script.Versioned Script.MintingPolicy)
    (TxSkelRedeemer, NEMap Api.TokenName (NonZero Integer))

-- | Combining 'TxSkelMints' in a sensible way. In particular, this means that
--
-- > Map.fromList [(pol, (red, NEMap.fromList [(tName, 1)]))]
--
-- and
--
-- > Map.fromList [(pol, (red', NEMap.fromList [(tName, -1)]))]
--
-- will combine to become the empty 'TxSkelMints' (and similar examples, where
-- the values add up to zero, see the comment at the definition of
-- 'addToTxSkelMints').
--
-- In every case, if you add mints with a different redeemer for the same
-- policy, the redeemer used in the right argument takes precedence.
instance {-# OVERLAPPING #-} Semigroup TxSkelMints where
  a <> b = foldl (flip addToTxSkelMints) a (txSkelMintsToList b)

instance {-# OVERLAPPING #-} Monoid TxSkelMints where
  mempty = Map.empty

-- | Add a new entry to a 'TxSkelMints'. There are a few wrinkles:
--
-- (1) If for a given policy, redeemer, and token name, there are @n@ tokens in
-- the argument 'TxSkelMints', and you add @-n@ tokens, the corresponding entry
-- in the "inner map" of the policy will disappear (obviously, because all of
-- its values have to be non-zero). If that also means that the inner map
-- becomes empty, the policy will disappear from the 'TxSkelMints' altogether.
--
-- (2) If a policy is already present on the argument 'TxSkelMints' with a
-- redeemer @a@, and you add a mint with a different redeemer @b@, the old
-- redeemer is thrown away. The values associated with the token names of that
-- policy are added as described above, though. This means that any pre-existing
-- values will be minted with a new redeemer.
--
-- If, for some reason, you really want to generate a 'TxSkelMints' that has
-- both a negative and a positive entry of the same asset class and redeemer,
-- you'll have to do so manually. Note, however, that even if you do so, NO
-- VALIDATOR OR MINTING POLICY WILL EVER GET TO SEE A TRANSACTION WITH SUCH
-- CONFLICTING INFORMATION. This is not a design decision/limitation of
-- cooked-validators: The Cardano API 'TxBodyContent' type, that we're
-- translating everything into eventually, stores minting information as a
-- minted value together with a map from policy IDs to witnesses (which
-- represent the used redeemers). That means that we can only store _one_
-- redeemer per minting policy, and no conflicting mints of the same asset
-- class, since they'll just cancel.
addToTxSkelMints ::
  (Script.Versioned Script.MintingPolicy, TxSkelRedeemer, Api.TokenName, Integer) ->
  TxSkelMints ->
  TxSkelMints
addToTxSkelMints (pol, red, tName, amount) mints
  | 0 == amount = mints
  | otherwise = case mints Map.!? pol of
      Nothing ->
        -- The policy isn't yet in the given 'TxSkelMints', so we can just add a
        -- new entry:
        Map.insert pol (red, NEMap.singleton tName (NonZero amount)) mints
      Just (_oldRed, innerMap) ->
        -- Ignore the old redeemer: If it's the same as the new one, nothing
        -- will change, if not, the new redeemer will be kept.
        case innerMap NEMap.!? tName of
          Nothing ->
            -- The given token name has not yet occurred for the given
            -- policy. This means that we can just add the new tokens to the
            -- inner map:
            Map.insert pol (red, NEMap.insert tName (NonZero amount) innerMap) mints
          Just (NonZero oldAmount) ->
            let newAmount = oldAmount + amount
             in if newAmount /= 0
                  then -- If the sum of the old amount of tokens and the
                  -- additional tokens is non-zero, we can just update the
                  -- amount in the inner map:
                    Map.insert pol (red, NEMap.insert tName (NonZero newAmount) innerMap) mints
                  else -- If the sum is zero, we'll have to delete the token
                  -- name from the inner map. If that yields a completely empty
                  -- inner map, we'll have to remove the entry altogether:
                  case NEMap.nonEmptyMap $ NEMap.delete tName innerMap of
                    Nothing -> Map.delete pol mints
                    Just newInnerMap -> Map.insert pol (red, newInnerMap) mints

-- | Convert from 'TxSkelMints' to a list of tuples describing eveything that's
-- being minted.
txSkelMintsToList :: TxSkelMints -> [(Script.Versioned Script.MintingPolicy, TxSkelRedeemer, Api.TokenName, Integer)]
txSkelMintsToList =
  concatMap
    ( \(p, (r, m)) ->
        (\(t, NonZero n) -> (p, r, t, n))
          <$> NEList.toList (NEMap.toList m)
    )
    . Map.toList

-- | Smart constructor for 'TxSkelMints'. This function relies on
-- 'addToTxSkelMints'. So, some non-empty lists (where all amounts for a given
-- asset class an redeemer add up to zero) might be translated into the empty
-- 'TxSkelMints'.
txSkelMintsFromList :: [(Script.Versioned Script.MintingPolicy, TxSkelRedeemer, Api.TokenName, Integer)] -> TxSkelMints
txSkelMintsFromList = foldr addToTxSkelMints mempty

-- | Another smart constructor for 'TxSkelMints', where the redeemer and minting
-- policies are not duplicated.
txSkelMintsFromList' :: [(Script.Versioned Script.MintingPolicy, TxSkelRedeemer, [(Api.TokenName, Integer)])] -> TxSkelMints
txSkelMintsFromList' = txSkelMintsFromList . concatMap (\(mp, r, m) -> (\(tn, i) -> (mp, r, tn, i)) <$> m)

-- | The value described by a 'TxSkelMints'
txSkelMintsValue :: TxSkelMints -> Api.Value
txSkelMintsValue =
  foldMapOf
    (to txSkelMintsToList % folded)
    ( \(policy, _, tName, amount) ->
        Script.assetClassValue
          ( Script.assetClass
              (Script.scriptCurrencySymbol policy)
              tName
          )
          amount
    )

-- * Transaction outputs

class IsTxSkelOutAllowedOwner a where
  toPKHOrValidator :: a -> Either Api.PubKeyHash (Script.Versioned Script.Validator)

instance IsTxSkelOutAllowedOwner Api.PubKeyHash where
  toPKHOrValidator = Left

instance IsTxSkelOutAllowedOwner (Script.TypedValidator a) where
  toPKHOrValidator = Right . Script.tvValidator

instance IsTxSkelOutAllowedOwner (Either Api.PubKeyHash (Script.Versioned Script.Validator)) where
  toPKHOrValidator = id

instance {-# OVERLAPPABLE #-} (IsTxSkelOutAllowedOwner a) => ToCredential a where
  toCredential a = case toPKHOrValidator a of
    Left pkh -> toCredential pkh
    Right val -> toCredential val

-- | Transaction outputs. The 'Pays' constructor is really general, and you'll
-- probably want to use one of the smart constructors like 'paysScript' or
-- 'paysPK' in most cases.
data TxSkelOut where
  Pays ::
    ( Show o, -- This is needed only for the 'Show' instance of 'TxSkel', which
    -- in turn is only needed in tests.
      Typeable o,
      IsTxInfoOutput o,
      IsTxSkelOutAllowedOwner (OwnerType o),
      Typeable (OwnerType o),
      DatumType o ~ TxSkelOutDatum,
      ValueType o ~ Api.Value, -- needed for the 'txSkelOutValueL'
      ToVersionedScript (ReferenceScriptType o),
      Show (OwnerType o),
      Show (ReferenceScriptType o),
      Typeable (ReferenceScriptType o)
    ) =>
    {producedOutput :: o} ->
    TxSkelOut

instance Eq TxSkelOut where
  Pays a == Pays b = case typeOf a `eqTypeRep` typeOf b of
    Just HRefl -> outputTxOut a == outputTxOut b
    Nothing -> False

deriving instance Show TxSkelOut

data Payable where
  Payable ::
    { payableDatum :: Maybe TxSkelOutDatum,
      payableStakingCred :: Maybe Api.StakingCredential,
      payableReferenceScript :: Maybe (Script.Versioned Script.Script),
      payableValue :: Maybe Api.Value
    } ->
    Payable

instance Semigroup Payable where
  Payable pd1 psc1 prs1 pv1 <> Payable pd2 psc2 prs2 pv2 =
    Payable (pd2 <|> pd1) (psc2 <|> psc1) (prs2 <|> prs1) (pv2 <|> pv1)

instance Monoid Payable where
  mempty = Payable Nothing Nothing Nothing Nothing

class IsPayable a where
  toPayable :: a -> Payable

instance IsPayable TxSkelOutDatum where
  toPayable dat = mempty {payableDatum = Just dat}

instance IsPayable Api.StakingCredential where
  toPayable stCred = mempty {payableStakingCred = Just stCred}

instance IsPayable (Script.Versioned Script.Script) where
  toPayable script = mempty {payableReferenceScript = Just script}

instance IsPayable Api.Value where
  toPayable value = mempty {payableValue = Just value}

instance IsPayable Payable where
  toPayable = id

receives :: (Show owner, Typeable owner, IsTxSkelOutAllowedOwner owner, IsPayable payment) => owner -> payment -> TxSkelOut
receives owner (toPayable -> Payable {..}) =
  Pays $
    ConcreteOutput
      owner
      payableStakingCred
      (fromMaybe TxSkelOutNoDatum payableDatum)
      (fromMaybe mempty payableValue)
      payableReferenceScript

infix 8 `receives`

infixl 9 &>

(&>) :: (IsPayable a, IsPayable b) => a -> b -> Payable
pa &> pb = toPayable pa <> toPayable pb

txSkelOutDatumL :: Lens' TxSkelOut TxSkelOutDatum
txSkelOutDatumL =
  lens
    (\(Pays output) -> output ^. outputDatumL)
    (\(Pays output) newDatum -> Pays $ output & outputDatumL .~ newDatum)

txSkelOutValueL :: Lens' TxSkelOut Api.Value
txSkelOutValueL =
  lens
    (\(Pays output) -> outputValue output)
    (\(Pays output) newValue -> Pays $ output & outputValueL .~ newValue)

txSkelOutValue :: TxSkelOut -> Api.Value
txSkelOutValue = (^. txSkelOutValueL)

txSkelOutValidator :: TxSkelOut -> Maybe (Script.Versioned Script.Validator)
txSkelOutValidator (Pays output) = rightToMaybe (toPKHOrValidator $ output ^. outputOwnerL)

type TxSkelOutDatumConstrs a = (Show a, PrettyCooked a, Api.ToData a, PlutusTx.Eq a, Typeable a)

-- | On transaction outputs, we have the options to use
--
-- 1. no datum
-- 2. only a datum hash
-- 3. a "normal" datum
-- 4. an inline datum
--
-- These four options are also what the type 'TxSkelOutDatum' records. The
-- following table explains their differences.
--
-- +------------------------+------------------+---------------------+-----------------------+
-- |                        | datum stored in  |                     | 'Api.OutputDatum'     |
-- |                        | in the simulated | datum resolved      | constructor           |
-- |                        | chain state      | on the 'txInfoData' | seen by the validator |
-- +========================+==================+=====================+=======================+
-- | 'TxSkelOutNoDatum'     | no               | no                  | 'Api.NoOutputDatum'   |
-- +------------------------+------------------+---------------------+-----------------------+
-- | 'TxSkelOutDatumHash'   | yes              | no                  | 'Api.OutputDatumHash' |
-- +------------------------+------------------+---------------------+-----------------------+
-- | 'TxSkelOutDatum'       | yes              | yes                 | 'Api.OutputDatumHash' |
-- +------------------------+------------------+---------------------+-----------------------+
-- | 'TxSkelOutInlineDatum' | yes              | no                  | 'Api.OutputDatum'     |
-- +------------------------+------------------+---------------------+-----------------------+
--
-- That is:
--
-- - Whenever there is a datum, we'll store it in the state of our simulated
--   chain. This will make it possible to retrieve it later, using functions
--   such as 'datumFromHash'.
--
-- - Both of the 'TxSkelOutDatumHash' and 'TxSkelOutDatum' constructors will
--   create an output that scripts see on the 'txInfo' as having a datum
--   hash. The difference is whether that hash will be resolvable using
--   validator functions like 'findDatum'.
data TxSkelOutDatum where
  -- | use no datum
  TxSkelOutNoDatum :: TxSkelOutDatum
  -- | only include the hash on the transaction
  TxSkelOutDatumHash :: (TxSkelOutDatumConstrs a) => a -> TxSkelOutDatum
  -- | use a 'Api.OutputDatumHash' on the transaction output, but generate the
  -- transaction in such a way that the complete datum is included in the
  -- 'txInfoData' seen by validators
  TxSkelOutDatum :: (TxSkelOutDatumConstrs a) => a -> TxSkelOutDatum
  -- | use an inline datum
  TxSkelOutInlineDatum :: (TxSkelOutDatumConstrs a) => a -> TxSkelOutDatum

deriving instance Show TxSkelOutDatum

instance Eq TxSkelOutDatum where
  x == y = compare x y == EQ

instance Ord TxSkelOutDatum where
  compare TxSkelOutNoDatum TxSkelOutNoDatum = EQ
  compare (TxSkelOutDatumHash d1) (TxSkelOutDatumHash d2) =
    case compare (SomeTypeRep (typeOf d1)) (SomeTypeRep (typeOf d2)) of
      LT -> LT
      GT -> GT
      EQ -> case typeOf d1 `eqTypeRep` typeOf d2 of
        Just HRefl -> compare (Api.toBuiltinData d1) (Api.toBuiltinData d2)
        Nothing -> error "This branch cannot happen: un-equal type representations that compare to EQ"
  compare (TxSkelOutDatum d1) (TxSkelOutDatum d2) =
    compare (TxSkelOutDatumHash d1) (TxSkelOutDatumHash d2)
  compare (TxSkelOutInlineDatum d1) (TxSkelOutInlineDatum d2) =
    compare (TxSkelOutDatumHash d1) (TxSkelOutDatumHash d2)
  compare TxSkelOutDatumHash {} TxSkelOutNoDatum = GT
  compare TxSkelOutDatum {} TxSkelOutNoDatum = GT
  compare TxSkelOutDatum {} TxSkelOutDatumHash {} = GT
  compare TxSkelOutInlineDatum {} _ = GT
  compare _ _ = LT

instance ToOutputDatum TxSkelOutDatum where
  toOutputDatum TxSkelOutNoDatum = Api.NoOutputDatum
  toOutputDatum (TxSkelOutDatumHash datum) = Api.OutputDatumHash . Script.datumHash . Api.Datum . Api.toBuiltinData $ datum
  toOutputDatum (TxSkelOutDatum datum) = Api.OutputDatumHash . Script.datumHash . Api.Datum . Api.toBuiltinData $ datum
  toOutputDatum (TxSkelOutInlineDatum datum) = Api.OutputDatum . Api.Datum . Api.toBuiltinData $ datum

txSkelOutUntypedDatum :: TxSkelOutDatum -> Maybe Api.Datum
txSkelOutUntypedDatum = \case
  TxSkelOutNoDatum -> Nothing
  TxSkelOutDatumHash x -> Just (Api.Datum $ Api.toBuiltinData x)
  TxSkelOutDatum x -> Just (Api.Datum $ Api.toBuiltinData x)
  TxSkelOutInlineDatum x -> Just (Api.Datum $ Api.toBuiltinData x)

txSkelOutTypedDatum :: (Api.FromData a) => TxSkelOutDatum -> Maybe a
txSkelOutTypedDatum = Api.fromBuiltinData . Api.getDatum <=< txSkelOutUntypedDatum

-- ** Smart constructors for transaction outputs

-- | Pays a certain value to a public key.
paysPK :: (ToPubKeyHash a) => a -> Api.Value -> TxSkelOut
paysPK pkh value = toPubKeyHash pkh `receives` value

-- | Pays a script a certain value with a certain datum hash, using the
-- 'TxSkelOutDatum' constructor. The resolved datum is provided in the body of
-- the transaction that issues the payment.
paysScript ::
  ( Api.ToData (Script.DatumType a),
    Show (Script.DatumType a),
    Typeable (Script.DatumType a),
    PlutusTx.Eq (Script.DatumType a),
    PrettyCooked (Script.DatumType a),
    Typeable a
  ) =>
  Script.TypedValidator a ->
  Script.DatumType a ->
  Api.Value ->
  TxSkelOut
paysScript validator datum value = validator `receives` value &> TxSkelOutDatum datum

-- | Pays a script a certain value with a certain inlined datum.
paysScriptInlineDatum ::
  ( Api.ToData (Script.DatumType a),
    Show (Script.DatumType a),
    Typeable (Script.DatumType a),
    PlutusTx.Eq (Script.DatumType a),
    PrettyCooked (Script.DatumType a),
    Typeable a
  ) =>
  Script.TypedValidator a ->
  Script.DatumType a ->
  Api.Value ->
  TxSkelOut
paysScriptInlineDatum validator datum value = validator `receives` value &> TxSkelOutInlineDatum datum

-- | Pays a script a certain value with a certain hashed datum, whose resolved
-- datum is not provided in the transaction body that issues the payment (as
-- opposed to "paysScript").
paysScriptUnresolvedDatumHash ::
  ( Api.ToData (Script.DatumType a),
    Show (Script.DatumType a),
    Typeable (Script.DatumType a),
    PlutusTx.Eq (Script.DatumType a),
    PrettyCooked (Script.DatumType a),
    Typeable a
  ) =>
  Script.TypedValidator a ->
  Script.DatumType a ->
  Api.Value ->
  TxSkelOut
paysScriptUnresolvedDatumHash validator datum value = validator `receives` value &> TxSkelOutDatumHash datum

-- | Pays a script a certain value without any datum. Intended to be used with
-- 'withDatum', 'withUnresolvedDatumHash', or 'withInlineDatum' to try a datum whose type
-- does not match the validator's.
paysScriptNoDatum :: (Typeable a) => Script.TypedValidator a -> Api.Value -> TxSkelOut
paysScriptNoDatum = receives

-- | Set the datum in a payment to the given datum (whose type may not fit the
-- typed validator in case of a script).
withDatum :: (Api.ToData a, Show a, Typeable a, PlutusTx.Eq a, PrettyCooked a) => TxSkelOut -> a -> TxSkelOut
withDatum (Pays output) datum = Pays $ (fromAbstractOutput output) {concreteOutputDatum = TxSkelOutDatum datum}

-- | Set the datum in a payment to the given inlined datum (whose type may not
-- fit the typed validator in case of a script).
withInlineDatum :: (Api.ToData a, Show a, Typeable a, PlutusTx.Eq a, PrettyCooked a) => TxSkelOut -> a -> TxSkelOut
withInlineDatum (Pays output) datum = Pays $ (fromAbstractOutput output) {concreteOutputDatum = TxSkelOutInlineDatum datum}

-- | Set the datum in a payment to the given hashed (not resolved in the
-- transaction) datum (whose type may not fit the typed validator in case of a
-- script).
withUnresolvedDatumHash :: (Api.ToData a, Show a, Typeable a, PlutusTx.Eq a, PrettyCooked a) => TxSkelOut -> a -> TxSkelOut
withUnresolvedDatumHash (Pays output) datum = Pays $ (fromAbstractOutput output) {concreteOutputDatum = TxSkelOutDatumHash datum}

-- | Add a reference script to a transaction output (or replace it if there is
-- already one)
withReferenceScript :: (Show script, ToVersionedScript script, Typeable script, ToScriptHash script) => TxSkelOut -> script -> TxSkelOut
withReferenceScript (Pays output) script = Pays $ (fromAbstractOutput output) {concreteOutputReferenceScript = Just script}

-- | Add a staking credential to a transaction output (or replace it if there is
-- already one)
withStakingCredential :: TxSkelOut -> Api.StakingCredential -> TxSkelOut
withStakingCredential (Pays output) stakingCredential = Pays $ (fromAbstractOutput output) {concreteOutputStakingCredential = Just stakingCredential}

-- * Transaction skeletons

data TxSkel where
  TxSkel ::
    { -- | Labels do not influence the transaction generation at all; they are
      -- pretty-printed whenever cooked-validators prints a transaction, and can
      -- therefore make the output more informative (and greppable).
      txSkelLabel :: Set TxLabel,
      -- | Some options that control transaction generation.
      txSkelOpts :: TxOpts,
      -- | Any value minted or burned by the transaction. You'll probably want
      -- to use 'txSkelMintsFromList' to construct this.
      txSkelMints :: TxSkelMints,
      -- | The wallets signing the transaction. This list must contain at least
      -- one element. By default, the first signer will pay for fees and
      -- balancing. You can change that with 'txOptBalanceWallet'.
      txSkelSigners :: [Wallet],
      txSkelValidityRange :: Ledger.SlotRange,
      -- | To each 'TxOutRef' the transaction should consume, add a redeemer
      -- specifying how to spend it. You must make sure that
      --
      -- - On 'TxOutRef's referencing UTxOs belonging to public keys, you use
      --   the 'emptyTxSkelRedeemer' smart constructor.
      --
      -- - On 'TxOutRef's referencing UTxOs belonging to scripts, you must make
      --   sure that the type of the redeemer is appropriate for the script.
      txSkelIns :: Map Api.TxOutRef TxSkelRedeemer,
      -- | All outputs referenced by the transaction.
      txSkelInsReference :: Set Api.TxOutRef,
      -- | The outputs of the transaction. These will occur in exactly this
      -- order on the transaction.
      txSkelOuts :: [TxSkelOut],
      -- | Possible proposals issued in this transaction to be voted on and
      -- possible enacted later on.
      txSkelProposals :: [TxSkelProposal],
      -- | Withdrawals performed by the transaction
      txSkelWithdrawals :: TxSkelWithdrawals
    } ->
    TxSkel
  deriving (Show, Eq)

makeLensesFor
  [ ("txSkelLabel", "txSkelLabelL"),
    ("txSkelOpts", "txSkelOptsL"),
    ("txSkelMints", "txSkelMintsL"),
    ("txSkelValidityRange", "txSkelValidityRangeL"),
    ("txSkelSigners", "txSkelSignersL"),
    ("txSkelIns", "txSkelInsL"),
    ("txSkelInsReference", "txSkelInsReferenceL"),
    ("txSkelOuts", "txSkelOutsL"),
    ("txSkelProposals", "txSkelProposalsL"),
    ("txSkelWithdrawals", "txSkelWithdrawalsL")
  ]
  ''TxSkel

-- | A convenience template of an empty transaction skeleton.
txSkelTemplate :: TxSkel
txSkelTemplate =
  TxSkel
    { txSkelLabel = Set.empty,
      txSkelOpts = def,
      txSkelMints = Map.empty,
      txSkelValidityRange = Api.always,
      txSkelSigners = [],
      txSkelIns = Map.empty,
      txSkelInsReference = Set.empty,
      txSkelOuts = [],
      txSkelProposals = [],
      txSkelWithdrawals = Map.empty
    }

-- | The missing information on a 'TxSkel' that can only be resolved by querying
-- the state of the blockchain.
data SkelContext = SkelContext
  { skelContextTxOuts :: Map Api.TxOutRef Api.TxOut,
    skelContextTxSkelOutDatums :: Map Api.DatumHash TxSkelOutDatum
  }

-- | Returns the full value contained in the skeleton outputs
txSkelValueInOutputs :: TxSkel -> Api.Value
txSkelValueInOutputs = foldOf (txSkelOutsL % folded % txSkelOutValueL)

-- | Return all data on transaction outputs. This can contain duplicates, which
-- is intended.
txSkelDataInOutputs :: TxSkel -> [(Api.DatumHash, TxSkelOutDatum)]
txSkelDataInOutputs =
  foldMapOf
    ( txSkelOutsL
        % folded
        % txSkelOutDatumL
    )
    ( \txSkelOutDatum ->
        maybe
          []
          (\datum -> [(Script.datumHash datum, txSkelOutDatum)])
          (txSkelOutUntypedDatum txSkelOutDatum)
    )

-- | All validators which will receive transaction outputs
txSkelValidatorsInOutputs :: TxSkel -> Map Script.ValidatorHash (Script.Versioned Script.Validator)
txSkelValidatorsInOutputs =
  Map.fromList
    . mapMaybe (fmap (\val -> (Script.validatorHash val, val)) . txSkelOutValidator)
    . txSkelOuts

-- | All validators in the reference script field of transaction outputs
txSkelReferenceScripts :: TxSkel -> Map Script.ValidatorHash (Script.Versioned Script.Validator)
txSkelReferenceScripts =
  mconcat
    . map
      ( \(Pays output) ->
          case output ^. outputReferenceScriptL of
            Nothing -> Map.empty
            Just x ->
              let vScript@(Script.Versioned script version) = toVersionedScript x
                  Script.ScriptHash hash = toScriptHash vScript
               in Map.singleton (Script.ValidatorHash hash) $ Script.Versioned (Script.Validator script) version
      )
    . txSkelOuts

-- | All `TxOutRefs` in reference inputs
txSkelReferenceTxOutRefs :: TxSkel -> [Api.TxOutRef]
txSkelReferenceTxOutRefs TxSkel {..} =
  -- direct reference inputs
  Set.toList txSkelInsReference
    -- reference inputs in inputs redeemers
    <> mapMaybe txSkelReferenceInput (Map.elems txSkelIns)
    -- reference inputs in proposals redeemers
    <> mapMaybe (txSkelReferenceInput . snd) (mapMaybe txSkelProposalWitness txSkelProposals)
    -- reference inputs in mints redeemers
    <> mapMaybe (txSkelReferenceInput . fst . snd) (Map.toList txSkelMints)

-- | All `TxOutRefs` known by a given transaction skeleton. This includes
-- TxOutRef`s used as inputs of the skeleton and `TxOutRef`s used as reference
-- inputs of the skeleton.  This does not include additional possible
-- `TxOutRef`s used for balancing and additional `TxOutRef`s used as collateral
-- inputs, as they are not part of the skeleton.
txSkelKnownTxOutRefs :: TxSkel -> [Api.TxOutRef]
txSkelKnownTxOutRefs skel@TxSkel {..} = txSkelReferenceTxOutRefs skel <> Map.keys txSkelIns

-- * Various Optics on 'TxSkels' and all the other types defined here

-- | Decide if a transaction output has a certain owner and datum type.
txSkelOutOwnerTypeP ::
  forall ownerType.
  ( ToCredential ownerType,
    Show ownerType,
    IsTxSkelOutAllowedOwner ownerType,
    Typeable ownerType
  ) =>
  Prism' TxSkelOut (ConcreteOutput ownerType TxSkelOutDatum Api.Value (Script.Versioned Script.Script))
txSkelOutOwnerTypeP =
  prism'
    Pays
    ( \(Pays output) ->
        case typeOf (output ^. outputOwnerL) `eqTypeRep` typeRep @ownerType of
          Just HRefl ->
            let cOut = fromAbstractOutput output
             in Just $ cOut {concreteOutputReferenceScript = toVersionedScript <$> concreteOutputReferenceScript cOut}
          Nothing -> Nothing
    )

txSkelOutputDatumTypeAT ::
  (Api.FromData a, Typeable a) =>
  AffineTraversal' TxSkelOut a
txSkelOutputDatumTypeAT =
  atraversal
    ( \txSkelOut -> case txSkelOutDatumComplete txSkelOut of
        Nothing -> Left txSkelOut
        Just (Api.Datum datum) -> case Api.fromBuiltinData datum of
          Just tyDatum -> Right tyDatum
          Nothing -> Left txSkelOut
    )
    ( \(Pays output) newTyDatum ->
        Pays $
          over
            outputDatumL
            ( \case
                TxSkelOutNoDatum -> TxSkelOutNoDatum
                TxSkelOutDatum tyDatum -> TxSkelOutDatum $ replaceDatumOnCorrectType tyDatum newTyDatum
                TxSkelOutDatumHash tyDatum -> TxSkelOutDatumHash $ replaceDatumOnCorrectType tyDatum newTyDatum
                TxSkelOutInlineDatum tyDatum -> TxSkelOutInlineDatum $ replaceDatumOnCorrectType tyDatum newTyDatum
            )
            output
    )
  where
    replaceDatumOnCorrectType :: (Typeable b, Typeable a) => b -> a -> b
    replaceDatumOnCorrectType old new = case typeOf old `eqTypeRep` typeOf new of
      Just HRefl -> new
      Nothing -> old

    txSkelOutDatumComplete :: TxSkelOut -> Maybe Api.Datum
    txSkelOutDatumComplete (Pays output) = txSkelOutUntypedDatum $ output ^. outputDatumL
