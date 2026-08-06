-- | This module runs the full automation pipeline that completes a
-- `Cooked.Skeleton.TxSkel` into an actual transaction. It also serves as an
-- umbrella re-exporting all the automation submodules (auto-filling, balancing
-- and transaction generation).
module Cooked.MockChain.Automation
  ( runAutomationPipeline,
    module X,
  )
where

import Cooked.MockChain.Automation.AutoFilling.Constitution as X
import Cooked.MockChain.Automation.AutoFilling.MinAda as X
import Cooked.MockChain.Automation.AutoFilling.ReferenceScripts as X
import Cooked.MockChain.Automation.AutoFilling.Withdrawals as X
import Cooked.MockChain.Automation.Balancing as X
import Cooked.MockChain.Automation.GenerateTx.Anchor as X
import Cooked.MockChain.Automation.GenerateTx.Body as X
import Cooked.MockChain.Automation.GenerateTx.Certificate as X
import Cooked.MockChain.Automation.GenerateTx.Collateral as X
import Cooked.MockChain.Automation.GenerateTx.Credential as X
import Cooked.MockChain.Automation.GenerateTx.Input as X
import Cooked.MockChain.Automation.GenerateTx.Mint as X
import Cooked.MockChain.Automation.GenerateTx.Output as X
import Cooked.MockChain.Automation.GenerateTx.Proposal as X
import Cooked.MockChain.Automation.GenerateTx.ReferenceInputs as X
import Cooked.MockChain.Automation.GenerateTx.Withdrawals as X
import Cooked.MockChain.Automation.GenerateTx.Witness as X
import Cooked.MockChain.Common
import Cooked.MockChain.Effect.Log
import Cooked.MockChain.Effect.Read.Chain
import Cooked.MockChain.Effect.Read.Conf
import Cooked.MockChain.Runtime.Error
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Query
import Cooked.Tweak.Update
import Ledger.Orphans ()
import Ledger.Tx qualified as P.Ledger
import Optics.Core
import Polysemy
import Polysemy.Error
import Polysemy.Fail

-- | This runs the full automation pipeline, in that order:
-- 1. autofill min ada on eligible outputs
-- 2. autofill constution on eligible proposals
-- 3. autofill reference inputs on eligible redeemers
-- 4. autofill amount on eligible withdrawals
-- 5. balance the skeleton according to the inner options
-- 6. generate the transaction associated with the balanced skeleton
-- It logs relevant events in the process, and returns the transaction.
runAutomationPipeline ::
  ( Members
      '[ Error P.Ledger.ToCardanoError,
         Error MockChainError,
         MockChainLog,
         MockChainReadChain,
         MockChainReadConf,
         Fail
       ]
      effs
  ) =>
  TxSkel ->
  Sem effs (TxSkel, (P.Ledger.CardanoTx, Maybe Collaterals, Fee))
runAutomationPipeline txSkel = runTweak txSkel $ do
  -- We log the submission of the new skeleton
  viewTweak simple >>= logEvent . MCLogSubmittedTxSkel
  -- We retrieve the current skeleton options
  TxSkelOpts {..} <- viewTweak txSkelOptsL
  -- We ensure that the outputs have the required minimal amount of ada, when
  -- requested in the skeleton options
  autoFillMinAda
  -- We retrieve the official constitution script and attach it to each
  -- proposal that requires it, if it's not empty
  autoFillConstitution
  -- We add reference scripts in the various redeemers of the skeleton, when
  -- they can be found in the index and are allowed to be auto filled
  autoFillReferenceScripts
  -- We attach the reward amount to withdrawals when applicable
  autoFillWithdrawalAmounts
  -- We balance the skeleton when requested in the skeleton option, and get
  -- the associated fee, collateral inputs and return collateral user
  ExtendedTxSkel finalTxSkel fee mCollaterals body <- viewTweak simple >>= balanceTxSkel
  -- We store the balanced skeleton
  setTweak simple finalTxSkel
  -- We log the balanced skeleton
  logEvent $ MCLogAdjustedTxSkel finalTxSkel fee mCollaterals
  -- We retrieve the extra signatories to add to the transaction
  signatories <- viewTweak txSkelSignatoriesL
  -- We generate the transaction associated with the skeleton, and apply on it
  -- the modifications from the skeleton options
  return
    ( P.Ledger.CardanoEmulatorEraTx $ txSkelOptModTx $ txSignatoriesAndBodyToCardanoTx signatories body,
      mCollaterals,
      fee
    )
