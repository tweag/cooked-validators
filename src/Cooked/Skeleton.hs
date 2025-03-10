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
    TxSkel (..),
    txSkelLabelL,
    txSkelOptsL,
    txSkelMintsL,
    txSkelValidityRangeL,
    txSkelProposalsL,
    txSkelSignersL,
    txSkelInsL,
    txSkelInsReferenceL,
    txSkelOutsL,
    txSkelWithdrawalsL,
    txSkelTemplate,
    txSkelDataInOutputs,
    txSkelValidatorsInOutputs,
    txSkelKnownTxOutRefs,
    txSkelWithdrawnValue,
    txSkelWithdrawalsScripts,
    txSkelValueInOutputs,
    txSkelReferenceScripts,
    txSkelReferenceTxOutRefs,
  )
where

import Cooked.Conversion
import Cooked.Output
import Cooked.Skeleton.Datum as X
import Cooked.Skeleton.Label as X
import Cooked.Skeleton.Mint as X
import Cooked.Skeleton.Option as X
import Cooked.Skeleton.Output as X
import Cooked.Skeleton.Payable as X
import Cooked.Skeleton.Proposal as X
import Cooked.Skeleton.Redeemer as X
import Cooked.Skeleton.Withdrawal as X
import Cooked.Wallet
import Data.Default
import Data.Either
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Slot qualified as Ledger
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

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
    . mapMaybe (fmap (\val -> (Script.toValidatorHash val, val)) . txSkelOutValidator)
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
              let vScript@(Script.Versioned script version) = Script.toVersioned x
                  Script.ScriptHash hash = Script.toScriptHash vScript
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

txSkelWithdrawnValue :: TxSkel -> Api.Value
txSkelWithdrawnValue = mconcat . (toValue . snd . snd <$>) . Map.toList . txSkelWithdrawals

txSkelWithdrawalsScripts :: TxSkel -> [Script.Versioned Script.Script]
txSkelWithdrawalsScripts = fst . partitionEithers . (fst <$>) . Map.toList . txSkelWithdrawals
