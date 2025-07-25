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
    txSkelCertificatesL,
    txSkelTemplate,
    txSkelKnownTxOutRefs,
    txSkelWithdrawnValue,
    txSkelWithdrawingScripts,
    txSkelValueInOutputs,
    txSkelInsReferenceInRedeemers,
    txSkelProposingScripts,
    txSkelMintingScripts,
    txSkelDepositedValueInCertificates,
    txSkelCertifyingScripts,
  )
where

import Cooked.Skeleton.Certificate as X
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
      txSkelWithdrawals :: TxSkelWithdrawals,
      -- | Certificates issued by the transaction
      txSkelCertificates :: [TxSkelCertificate]
    } ->
    TxSkel
  deriving (Show, Eq)

-- | Focusing on the labels of a 'TxSkel'
makeLensesFor [("txSkelLabel", "txSkelLabelL")] ''TxSkel

-- | Focusing on the optics of a 'TxSkel'
makeLensesFor [("txSkelOpts", "txSkelOptsL")] ''TxSkel

-- | Focusing on the minted value of a 'TxSkel'
makeLensesFor [("txSkelMints", "txSkelMintsL")] ''TxSkel

-- | Focusing on the validity range of a 'TxSkel'
makeLensesFor [("txSkelValidityRange", "txSkelValidityRangeL")] ''TxSkel

-- | Focusing on the signers of a 'TxSkel'
makeLensesFor [("txSkelSigners", "txSkelSignersL")] ''TxSkel

-- | Focusing on the inputs of a 'TxSkel'
makeLensesFor [("txSkelIns", "txSkelInsL")] ''TxSkel

-- | Focusing on the reference inputs of a 'TxSkel'
makeLensesFor [("txSkelInsReference", "txSkelInsReferenceL")] ''TxSkel

-- | Focusing on the outputs of a 'TxSkel'
makeLensesFor [("txSkelOuts", "txSkelOutsL")] ''TxSkel

-- | Focusing on the proposals of a 'TxSkel'
makeLensesFor [("txSkelProposals", "txSkelProposalsL")] ''TxSkel

-- | Focusing on the withdrawals of a 'TxSkel'
makeLensesFor [("txSkelWithdrawals", "txSkelWithdrawalsL")] ''TxSkel

-- | Focusing on the certificates of a 'TxSkel'
makeLensesFor [("txSkelCertificates", "txSkelCertificatesL")] ''TxSkel

-- | A lens to set or

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
      txSkelWithdrawals = Map.empty,
      txSkelCertificates = []
    }

-- | Returns the full value contained in the skeleton outputs
txSkelValueInOutputs :: TxSkel -> Api.Value
txSkelValueInOutputs = foldOf (txSkelOutsL % folded % txSkelOutValueL)

-- | All 'Api.TxOutRef's in reference inputs from redeemers
txSkelInsReferenceInRedeemers :: TxSkel -> Set Api.TxOutRef
txSkelInsReferenceInRedeemers TxSkel {..} =
  Set.fromList $
    toListOf (traversed % txSkelRedeemerReferenceInputL % _Just) (Map.elems txSkelIns)
      <> toListOf (traversed % txSkelProposalRedeemedScriptAT % redeemedScriptRedeemerL % txSkelRedeemerReferenceInputL % _Just) txSkelProposals
      <> toListOf (traversed % _1 % txSkelRedeemerReferenceInputL % _Just) (Map.elems txSkelMints)
      <> toListOf (traversed % _1 % txSkelRedeemerReferenceInputL % _Just) (Map.elems txSkelWithdrawals)
      <> toListOf (traversed % txSkelCertificateRedeemedScriptAT % redeemedScriptRedeemerL % txSkelRedeemerReferenceInputL % _Just) txSkelCertificates

-- | All `Api.TxOutRef`s known by a given transaction skeleton. This includes
-- TxOutRef`s used as inputs of the skeleton and 'Api.TxOutRef's used as reference
-- inputs of the skeleton.  This does not include additional possible
-- 'Api.TxOutRef's used for balancing and additional 'Api.TxOutRef's used as collateral
-- inputs, as they are not part of the skeleton.
txSkelKnownTxOutRefs :: TxSkel -> Set Api.TxOutRef
txSkelKnownTxOutRefs skel@TxSkel {..} = txSkelInsReferenceInRedeemers skel <> Map.keysSet txSkelIns <> txSkelInsReference

-- | Returns the total value withdrawn in this 'TxSkel'
txSkelWithdrawnValue :: TxSkel -> Api.Value
txSkelWithdrawnValue = review valueLovelaceP . foldOf (txSkelWithdrawalsL % to Map.toList % traversed % _2 % _2)

-- | Returns the total value deposited in this 'TxSkel'
txSkelDepositedValueInCertificates :: TxSkel -> Api.Value
txSkelDepositedValueInCertificates = foldOf (txSkelCertificatesL % traversed % txSkelCertificateDepositedValueG)

-- | Returns all the scripts involved in withdrawals in this 'TxSkel'
txSkelWithdrawingScripts :: TxSkel -> [Script.Versioned Script.Script]
txSkelWithdrawingScripts = toListOf (txSkelWithdrawalsL % to Map.toList % traversed % _1 % _Left)

-- | Returns all the scripts involved in proposals in this 'TxSkel'
txSkelProposingScripts :: TxSkel -> [Script.Versioned Script.Script]
txSkelProposingScripts = toListOf (txSkelProposalsL % traversed % txSkelProposalRedeemedScriptAT % redeemedScriptVersionedL)

-- | Returns all the scripts involved in minting in this 'TxSkel'
txSkelMintingScripts :: TxSkel -> [Script.Versioned Script.Script]
txSkelMintingScripts = toListOf (txSkelMintsL % txSkelMintsListI % traversed % mintRedeemedScriptL % redeemedScriptVersionedL)

-- | Returns all the scripts involved in certificates in this 'TxSkel'
txSkelCertifyingScripts :: TxSkel -> [Script.Versioned Script.Script]
txSkelCertifyingScripts = toListOf (txSkelCertificatesL % traversed % txSkelCertificateRedeemedScriptAT % redeemedScriptVersionedL)
