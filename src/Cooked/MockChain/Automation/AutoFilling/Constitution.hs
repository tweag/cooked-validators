-- | This module exposes a function to automatically fill the constitution
-- scripts of the proposals in a 'Cooked.Skeleton.TxSkel' based on the current
-- state of the blockchain.
module Cooked.MockChain.Automation.AutoFilling.Constitution where

import Control.Monad
import Cooked.MockChain.Effect.Log
import Cooked.MockChain.Effect.Read
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Update
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import Polysemy

-- * Auto filling constitution script

-- | Goes through all the proposals of the input skeleton and attempts to fill
-- out the constitution scripts with the current one. Does not tamper with an
-- existing specified script in such proposals. Logs an event when the
-- constitution script has been successfully auto-filled.
autoFillConstitution ::
  (Members '[MockChainRead, Tweak, MockChainLog] effs) =>
  Sem effs ()
autoFillConstitution = do
  currentConstitution <- getConstitutionScript
  case currentConstitution of
    Nothing -> return ()
    Just constitutionScript -> do
      traverseTweak (txSkelProposalsL % traversed) $ \prop -> do
        when (isn't txSkelProposalConstitutionAT prop) $
          logEvent $
            MCLogAutoFilledConstitution $
              Script.toScriptHash constitutionScript
        return (fillConstitution constitutionScript prop)
