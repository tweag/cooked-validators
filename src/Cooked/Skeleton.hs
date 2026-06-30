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

    -- * Data type
    TxSkel (..),

    -- * Optics
    txSkelLabelsL,
    txSkelOptsL,
    txSkelMintsL,
    txSkelValidityRangeL,
    txSkelProposalsL,
    txSkelSignatoriesL,
    txSkelInputsL,
    txSkelReferenceInputsL,
    txSkelOutputsL,
    txSkelWithdrawalsL,
    txSkelCertificatesL,
    txSkelRedeemersT,

    -- * Smart constructor
    txSkelTemplate,

    -- * Utilities
    txSkelKnownTxOutRefs,
    txSkelWithdrawnValue,
    txSkelWithdrawingScripts,
    txSkelPaidValue,
    txSkelReferenceInputsInRedeemers,
    txSkelProposingScripts,
    txSkelMintingScripts,
    txSkelCertifyingScripts,
  )
where

import Cooked.Skeleton.Anchor as X
import Cooked.Skeleton.Certificate as X
import Cooked.Skeleton.Datum as X
import Cooked.Skeleton.Label as X
import Cooked.Skeleton.Mint as X
import Cooked.Skeleton.Option as X
import Cooked.Skeleton.Output as X
import Cooked.Skeleton.Proposal as X
import Cooked.Skeleton.Redeemer as X
import Cooked.Skeleton.Signatory as X
import Cooked.Skeleton.User as X
import Cooked.Skeleton.Value as X
import Cooked.Skeleton.Withdrawal as X
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger.Slot qualified as P.Ledger
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | A transaction skeleton. This is cooked-validators's variant of transaction
-- bodies, eventually translated to Cardano @TxBody@.
data TxSkel where
  TxSkel ::
    { -- | Labels do not influence the transaction generation at all; they are
      -- pretty-printed whenever cooked-validators prints a transaction, and can
      -- therefore make the output more informative. They can also be used to
      -- select skeletons to be modified during a mockchain run.
      txSkelLabels :: Set TxSkelLabel,
      -- | Some options that control transaction generation.
      txSkelOpts :: TxSkelOpts,
      -- | Any value minted or burned by the transaction. You'll probably want
      -- to use 'Cooked.Skeleton.Mint.txSkelMintsFromList' to construct this.
      txSkelMints :: TxSkelMints,
      -- | The signatories of the transaction. When auto-balancing is enabled,
      -- this list must contain at least one element, which is always expected
      -- to be case in practice anyway. By default, the first signatory will pay
      -- for fees and balancing. You can change that with
      -- 'Cooked.Skeleton.Option.txSkelOptBalancingPolicy'.
      txSkelSignatories :: [TxSkelSignatory],
      txSkelValidityRange :: P.Ledger.SlotRange,
      -- | To each 'Api.TxOutRef' the transaction should consume, add a redeemer
      -- specifying how to spend it. You must make sure that
      --
      -- - On 'Api.TxOutRef's referencing UTxOs belonging to public keys, use
      --   the 'Cooked.Skeleton.Redeemer.emptyTxSkelRedeemer' smart constructor.
      --
      -- - On 'Api.TxOutRef's referencing UTxOs belonging to scripts, use
      --   the 'Cooked.Skeleton.Redeemer.someTxSkelRedeemer' smart constructor.
      txSkelInputs :: Map Api.TxOutRef TxSkelRedeemer,
      -- | All outputs directly referenced by the transaction. Each of them will
      -- be directly translated into a Cardano reference input. Additional
      -- reference inputs can be found within the various redeemers of the
      -- skeleton to host reference scripts. Function
      -- 'txSkelReferenceInputsInRedeemers' collects those all.
      txSkelReferenceInputs :: Set Api.TxOutRef,
      -- | The outputs of the transaction. These will occur in exactly this
      -- order on the transaction.
      txSkelOutputs :: [TxSkelOut],
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

-- | Focuses on the labels of a 'TxSkel'
makeLensesFor [("txSkelLabels", "txSkelLabelsL")] ''TxSkel

-- | Focuses on the options of a 'TxSkel'
makeLensesFor [("txSkelOpts", "txSkelOptsL")] ''TxSkel

-- | Focuses on the minted value of a 'TxSkel'
makeLensesFor [("txSkelMints", "txSkelMintsL")] ''TxSkel

-- | Focuses on the validity range of a 'TxSkel'
makeLensesFor [("txSkelValidityRange", "txSkelValidityRangeL")] ''TxSkel

-- | Focuses on the proposals of a 'TxSkel'
makeLensesFor [("txSkelProposals", "txSkelProposalsL")] ''TxSkel

-- | Focuses on the signatories of a 'TxSkel'
makeLensesFor [("txSkelSignatories", "txSkelSignatoriesL")] ''TxSkel

-- | Focuses on the inputs of a 'TxSkel'
makeLensesFor [("txSkelInputs", "txSkelInputsL")] ''TxSkel

-- | Focuses on the reference inputs of a 'TxSkel'
makeLensesFor [("txSkelReferenceInputs", "txSkelReferenceInputsL")] ''TxSkel

-- | Focuses on the outputs of a 'TxSkel'
makeLensesFor [("txSkelOutputs", "txSkelOutputsL")] ''TxSkel

-- | Focuses on the withdrawals of a 'TxSkel'
makeLensesFor [("txSkelWithdrawals", "txSkelWithdrawalsL")] ''TxSkel

-- | Focuses on the certificates of a 'TxSkel'
makeLensesFor [("txSkelCertificates", "txSkelCertificatesL")] ''TxSkel

-- | A traversal focusing every 'TxSkelRedeemer' of a 'TxSkel', in all five
-- positions (spending, minting, proposing, withdrawing and certifying).
txSkelRedeemersT :: Traversal' TxSkel TxSkelRedeemer
txSkelRedeemersT =
  (txSkelInputsL % iso Map.toList Map.fromList % traversed % _2)
    `adjoin` (txSkelMintsL % txSkelMintsListI % traversed % mintRedeemedScriptL % userTxSkelRedeemerL)
    `adjoin` (txSkelProposalsL % traversed % txSkelProposalMConstitutionAT % _Just % userTxSkelRedeemerL)
    `adjoin` (txSkelWithdrawalsL % txSkelWithdrawalsListI % traversed % withdrawalUserL % userTxSkelRedeemerAT)
    `adjoin` (txSkelCertificatesL % traversed % txSkelCertificateOwnerAT % userTxSkelRedeemerL)

-- | A convenience template of an empty transaction skeleton.
txSkelTemplate :: TxSkel
txSkelTemplate =
  TxSkel
    { txSkelLabels = mempty,
      txSkelOpts = def,
      txSkelMints = mempty,
      txSkelValidityRange = Api.always,
      txSkelSignatories = mempty,
      txSkelInputs = mempty,
      txSkelReferenceInputs = mempty,
      txSkelOutputs = mempty,
      txSkelProposals = mempty,
      txSkelWithdrawals = mempty,
      txSkelCertificates = mempty
    }

-- | All `Api.TxOutRef`s known by a given transaction skeleton. This includes
-- TxOutRef`s used as inputs of the skeleton and 'Api.TxOutRef's used as reference
-- inputs of the skeleton.  This does not include additional possible
-- 'Api.TxOutRef's used for balancing and additional 'Api.TxOutRef's used as collateral
-- inputs, as they are not part of the skeleton.
txSkelKnownTxOutRefs :: TxSkel -> Set Api.TxOutRef
txSkelKnownTxOutRefs skel@TxSkel {..} = txSkelReferenceInputsInRedeemers skel <> Map.keysSet txSkelInputs <> txSkelReferenceInputs

-- | Returns the total value withdrawn in this 'TxSkel'
txSkelWithdrawnValue :: TxSkel -> Api.Value
txSkelWithdrawnValue = Script.toValue . txSkelWithdrawals

-- | Returns all the scripts involved in withdrawals in this 'TxSkel'
txSkelWithdrawingScripts :: TxSkel -> [VScript]
txSkelWithdrawingScripts = toListOf (txSkelWithdrawalsL % txSkelWithdrawalsListI % traversed % withdrawalUserL % userVScriptAT)

-- | Returns the full value contained in the skeleton outputs
txSkelPaidValue :: TxSkel -> Api.Value
txSkelPaidValue = foldOf (txSkelOutputsL % folded % txSkelOutValueL)

-- | All 'Api.TxOutRef's in reference inputs from redeemers
txSkelReferenceInputsInRedeemers :: TxSkel -> Set Api.TxOutRef
txSkelReferenceInputsInRedeemers =
  Set.fromList . toListOf (txSkelRedeemersT % txSkelRedeemerReferenceInputAT)

-- | Returns all the scripts involved in proposals in this 'TxSkel'
txSkelProposingScripts :: TxSkel -> [VScript]
txSkelProposingScripts = toListOf (txSkelProposalsL % traversed % txSkelProposalMConstitutionAT % _Just % userVScriptL)

-- | Returns all the scripts involved in minting in this 'TxSkel'
txSkelMintingScripts :: TxSkel -> [VScript]
txSkelMintingScripts = toListOf (txSkelMintsL % txSkelMintsListI % traversed % mintRedeemedScriptL % userVScriptL)

-- | Returns all the scripts involved in certificates in this 'TxSkel'
txSkelCertifyingScripts :: TxSkel -> [VScript]
txSkelCertifyingScripts = toListOf (txSkelCertificatesL % traversed % txSkelCertificateOwnerAT @IsEither % userVScriptAT)
