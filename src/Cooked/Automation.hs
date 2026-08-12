-- | This module runs the full automation pipeline that completes a
-- `Cooked.Skeleton.TxSkel` into an actual transaction. It also serves as an
-- umbrella re-exporting all the automation submodules (auto-filling, balancing
-- and transaction generation).
module Cooked.Automation
  ( runAutomationPipeline,
    module X,
  )
where

import Control.Monad
import Cooked.Automation.AutoFilling.Constitution as X
import Cooked.Automation.AutoFilling.MinAda as X
import Cooked.Automation.AutoFilling.ReferenceScripts as X
import Cooked.Automation.AutoFilling.Withdrawals as X
import Cooked.Automation.Balancing as X
import Cooked.Automation.GenerateTx.Anchor as X
import Cooked.Automation.GenerateTx.Body as X
import Cooked.Automation.GenerateTx.Certificate as X
import Cooked.Automation.GenerateTx.Collateral as X
import Cooked.Automation.GenerateTx.Credential as X
import Cooked.Automation.GenerateTx.Input as X
import Cooked.Automation.GenerateTx.Mint as X
import Cooked.Automation.GenerateTx.Output as X
import Cooked.Automation.GenerateTx.Proposal as X
import Cooked.Automation.GenerateTx.ReferenceInputs as X
import Cooked.Automation.GenerateTx.Withdrawals as X
import Cooked.Automation.GenerateTx.Witness as X
import Cooked.Effect.Log
import Cooked.Effect.Params
import Cooked.Effect.Query
import Cooked.Runtime.Error
import Cooked.Skeleton
import Cooked.Tweak.Common
import Ledger.Orphans ()
import Ledger.Tx qualified as P.Ledger
import Polysemy
import Polysemy.Error
import Polysemy.Fail

-- | This runs the full automation pipeline:
-- 1. autofill min ada on eligible outputs
-- 2. autofill constution on eligible proposals
-- 3. autofill reference inputs on eligible redeemers
-- 4. autofill amount on eligible withdrawals
-- 5. balance the skeleton
-- 6. compute fees and collaterals
-- 7. generate a cardano transaction body
-- 8. fetch phase 2 failures
runAutomationPipeline ::
  ( Members
      '[ Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Log,
         Query,
         Params,
         Fail
       ]
      effs
  ) =>
  TxSkel ->
  Sem effs ExtendedTxSkel
runAutomationPipeline =
  ( `execTweak`
      do
        autoFillMinAda
        autoFillConstitution
        autoFillReferenceScripts
        autoFillWithdrawalAmounts
  )
    >=> balanceTxSkel
