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
  ( module X,
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
    TxSkelOut (..),
    receives,
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
  )
where

import Cooked.Conversion
import Cooked.Output
import Cooked.Skeleton.Datum as X
import Cooked.Skeleton.Label as X
import Cooked.Skeleton.Option as X
import Cooked.Skeleton.Payable as X
import Cooked.Skeleton.Redeemer as X
import Cooked.Wallet
import Data.Default
import Data.Either
import Data.Either.Combinators
import Data.Function
import Data.List.NonEmpty qualified as NEList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
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

instance IsTxSkelOutAllowedOwner Wallet where
  toPKHOrValidator = Left . toPubKeyHash

instance IsTxSkelOutAllowedOwner (Script.Versioned Script.Validator) where
  toPKHOrValidator = Right

instance IsTxSkelOutAllowedOwner (Script.TypedValidator a) where
  toPKHOrValidator = Right . Script.tvValidator

instance IsTxSkelOutAllowedOwner (Either Api.PubKeyHash (Script.Versioned Script.Validator)) where
  toPKHOrValidator = id

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
      ToCredential (OwnerType o),
      Typeable (OwnerType o),
      DatumType o ~ TxSkelOutDatum,
      ValueType o ~ Api.Value, -- needed for the 'txSkelOutValueL'
      ToVersionedScript (ReferenceScriptType o),
      Show (OwnerType o),
      Show (ReferenceScriptType o),
      Typeable (ReferenceScriptType o)
    ) =>
    o ->
    TxSkelOut

instance Eq TxSkelOut where
  Pays a == Pays b = case typeOf a `eqTypeRep` typeOf b of
    Just HRefl -> outputTxOut a == outputTxOut b
    Nothing -> False

deriving instance Show TxSkelOut

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

-- | Smart constructor to build @TxSkelOut@ from an owner and payment. This
-- should be the main way of building outputs.
receives :: (Show owner, Typeable owner, IsTxSkelOutAllowedOwner owner, ToCredential owner) => owner -> Payable els -> TxSkelOut
receives owner = go $ Pays $ ConcreteOutput owner Nothing TxSkelOutNoDatum mempty $ Nothing @(Script.Versioned Script.Script)
  where
    go :: TxSkelOut -> Payable els -> TxSkelOut
    go (Pays output) (VisibleHashedDatum dat) = Pays $ setDatum output $ TxSkelOutDatum dat
    go (Pays output) (InlineDatum dat) = Pays $ setDatum output $ TxSkelOutInlineDatum dat
    go (Pays output) (HiddenHashedDatum dat) = Pays $ setDatum output $ TxSkelOutDatumHash dat
    go (Pays output) (Value v) = Pays $ setValue output $ toValue v
    go (Pays output) (ReferenceScript script) = Pays $ setReferenceScript output $ toVersionedScript script
    go (Pays output) (StakingCredential (toMaybeStakingCredential -> Just stCred)) = Pays $ setStakingCredential output stCred
    go pays (StakingCredential _) = pays
    go pays (PayableAnd p1 p2) = go (go pays p1) p2

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
