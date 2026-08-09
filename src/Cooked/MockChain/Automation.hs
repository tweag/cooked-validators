-- | This module runs the full automation pipeline that completes a
-- `Cooked.Skeleton.TxSkel` into an actual transaction. It also serves as an
-- umbrella re-exporting all the automation submodules (auto-filling, balancing
-- and transaction generation).
module Cooked.MockChain.Automation
  ( runAutomationPipeline,
    module X,
  )
where

import Control.Monad
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
import Cooked.MockChain.Effect.Log
import Cooked.MockChain.Effect.Read.Chain
import Cooked.MockChain.Effect.Read.Conf
import Cooked.MockChain.Runtime.Error
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
         MockChainLog,
         MockChainReadChain,
         MockChainReadConf,
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
