{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module CrowdFundingSpec where

import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified CrowdFunding
import qualified PlutusTx.Builtins as Pl
import Control.Monad (void)

ada :: Integer -> Pl.Value
ada = Pl.lovelaceValueOf . (* 1_000_000)

createProject :: MonadBlockChain m => Wallet -> Pl.BuiltinByteString -> Integer -> Pl.POSIXTime -> m Pl.TxId
createProject wallet projectName threshold deadline =
  validateTxSkel $ txSkel [ PaysScript CrowdFunding.crowdFundingValidator myDatum mempty ]
  where
    myDatum = CrowdFunding.ProjectProposal (walletPKHash wallet) projectName threshold (Pl.to deadline)

exampleTrace :: MonadMockChain m => m ()
exampleTrace = void $ do
  createProject (wallet 1) "project1" 2_000_000 
