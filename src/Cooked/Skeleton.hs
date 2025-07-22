-- | This module provides the description of a transaction skeleton. We have our
-- own representation of a transaction for many reasons. Here are some of them:
--
-- - our transaction skeletons are typed (datums, validators, outputs...)
--
-- - each transaction skeleton comes with its own set of generation options
--
-- - our transaction skeleton is by default anchored in the latest Cardano era
--
-- - each field in our transaction skeleton comes with a set of helpers and
-- - smart constructor to ease the transaction creation
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
    txSkelKnownTxOutRefs,
    txSkelWithdrawnValue,
    txSkelWithdrawingScripts,
    txSkelValueInOutputs,
    txSkelInsReferenceInRedeemers,
    txSkelProposingScripts,
    txSkelMintingScripts,
  )
where

import Cooked.Skeleton.Datum as X
import Cooked.Skeleton.Label as X
import Cooked.Skeleton.Mint as X
import Cooked.Skeleton.Option as X
import Cooked.Skeleton.Output as X
import Cooked.Skeleton.Payable as X
import Cooked.Skeleton.Proposal as X
import Cooked.Skeleton.Redeemer as X
import Cooked.Skeleton.ReferenceScript as X
import Cooked.Skeleton.Withdrawal as X
import Cooked.Wallet
import Data.Default
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

-- | A transaction skeleton. This is cooked-validators's variant of transaction
-- bodies, eventually translated to Cardano @TxBody@.
data TxSkel where
  TxSkel ::
    { -- | Labels do not influence the transaction generation at all; they are
      -- pretty-printed whenever cooked-validators prints a transaction, and can
      -- therefore make the output more informative (and greppable).
      txSkelLabel :: Set TxSkelLabel,
      -- | Some options that control transaction generation.
      txSkelOpts :: TxSkelOpts,
      -- | Any value minted or burned by the transaction. You'll probably want
      -- to use 'Cooked.Skeleton.Mint.txSkelMintsFromList' to construct this.
      txSkelMints :: TxSkelMints,
      -- | The wallets signing the transaction. This list must contain at least
      -- one element. By default, the first signer will pay for fees and
      -- balancing. You can change that with
      -- 'Cooked.Skeleton.Option.txOptBalancingPolicy'.
      txSkelSigners :: [Wallet],
      txSkelValidityRange :: Ledger.SlotRange,
      -- | To each 'Api.TxOutRef' the transaction should consume, add a redeemer
      -- specifying how to spend it. You must make sure that
      --
      -- - On 'Api.TxOutRef's referencing UTxOs belonging to public keys, use
      --   the 'Cooked.Skeleton.Redeemer.emptyTxSkelRedeemer' smart constructor.
      --
      -- - On 'Api.TxOutRef's referencing UTxOs belonging to scripts, use
      --   the 'Cooked.Skeleton.Redeemer.someTxSkelRedeemer' smart constructor.
      txSkelIns :: Map Api.TxOutRef TxSkelRedeemer,
      -- | All outputs directly referenced by the transaction. Each of them will
      -- be directly translated into a Cardano reference input. Additional
      -- reference inputs can be found within the various redeemers of the
      -- skeleton to host reference scripts. Function
      -- 'txSkelInsReferenceInRedeemers' collects those all.
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

-- | A lens to set of get labels from a 'TxSkel'
makeLensesFor [("txSkelLabel", "txSkelLabelL")] ''TxSkel

-- | A lens to set of get options from a 'TxSkel'
makeLensesFor [("txSkelOpts", "txSkelOptsL")] ''TxSkel

-- | A lens to set of get the minted value of a 'TxSkel'
makeLensesFor [("txSkelMints", "txSkelMintsL")] ''TxSkel

-- | A lens to set of get the validity range of a 'TxSkel'
makeLensesFor [("txSkelValidityRange", "txSkelValidityRangeL")] ''TxSkel

-- | A lens to set of get signers from a 'TxSkel'
makeLensesFor [("txSkelSigners", "txSkelSignersL")] ''TxSkel

-- | A lens to set of get inputs from a 'TxSkel'
makeLensesFor [("txSkelIns", "txSkelInsL")] ''TxSkel

-- | A lens to set of get reference inputs from a 'TxSkel'
makeLensesFor [("txSkelInsReference", "txSkelInsReferenceL")] ''TxSkel

-- | A lens to set of get outputs from a 'TxSkel'
makeLensesFor [("txSkelOuts", "txSkelOutsL")] ''TxSkel

-- | A lens to set of get proposals from a 'TxSkel'
makeLensesFor [("txSkelProposals", "txSkelProposalsL")] ''TxSkel

-- | A lens to set of get withdrawals from a 'TxSkel'
makeLensesFor [("txSkelWithdrawals", "txSkelWithdrawalsL")] ''TxSkel

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

-- | All 'Api.TxOutRef's in reference inputs from redeemers
txSkelInsReferenceInRedeemers :: TxSkel -> Set Api.TxOutRef
txSkelInsReferenceInRedeemers TxSkel {..} =
  Set.fromList $
    mapMaybe txSkelRedeemerReferenceInput $
      Map.elems txSkelIns
        <> (snd <$> mapMaybe txSkelProposalWitness txSkelProposals)
        <> (fst <$> Map.elems txSkelMints)
        <> (fst <$> Map.elems txSkelWithdrawals)

-- | All `Api.TxOutRef`s known by a given transaction skeleton. This includes
-- TxOutRef`s used as inputs of the skeleton and 'Api.TxOutRef's used as reference
-- inputs of the skeleton.  This does not include additional possible
-- 'Api.TxOutRef's used for balancing and additional 'Api.TxOutRef's used as collateral
-- inputs, as they are not part of the skeleton.
txSkelKnownTxOutRefs :: TxSkel -> Set Api.TxOutRef
txSkelKnownTxOutRefs skel@TxSkel {..} =
  txSkelInsReferenceInRedeemers skel
    <> Map.keysSet txSkelIns
    <> txSkelInsReference

-- | Returns the total value withdrawn in this 'TxSkel'
txSkelWithdrawnValue :: TxSkel -> Api.Value
txSkelWithdrawnValue = review valueLovelaceP . foldOf (txSkelWithdrawalsL % to Map.toList % traversed % _2 % _2)

-- | Returns all the scripts involved in withdrawals in this 'TxSkel'
txSkelWithdrawingScripts :: TxSkel -> [Script.Versioned Script.Script]
txSkelWithdrawingScripts = toListOf (txSkelWithdrawalsL % to Map.toList % traversed % _1 % _Left)

-- | Returns all the scripts involved in proposals in this 'TxSkel'
txSkelProposingScripts :: TxSkel -> [Script.Versioned Script.Script]
txSkelProposingScripts = toListOf (txSkelProposalsL % traversed % txSkelProposalWitnessL % _Just % _1)

-- | Returns all the scripts involved in minting in this 'TxSkel'
txSkelMintingScripts :: TxSkel -> [Script.Versioned Script.Script]
txSkelMintingScripts = toListOf (txSkelMintsL % txSkelMintsListI % traversed % mintVersionedScriptL)
