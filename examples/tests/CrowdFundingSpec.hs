{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module CrowdFundingSpec where

import Control.Monad (void)
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified CrowdFunding as CF
import Data.Default (def)
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified PlutusTx.Builtins as Pl

--------------------------------
-- Some top-level definitions --
--------------------------------

ada :: Integer -> Pl.Value
ada = Pl.lovelaceValueOf . (* 1_000_000)

alice, bob, judith, marc :: Wallet
[alice, bob, judith, marc] = wallet <$> [1 .. 4]

month :: Pl.POSIXTime
month = 3600 * 24 * 30

-----------------------------------------------------
-- Endpoints for the crowd funding smart contracts --
-----------------------------------------------------

-- A peer can create a project
createProject :: MonadBlockChain m => Wallet -> Pl.BuiltinByteString -> Integer -> Pl.POSIXTime -> m ()
createProject myWallet projectName threshold deadline =
  let myDatum = CF.ProjectProposal (walletPKHash myWallet) projectName threshold (Pl.to deadline)
   in void $ validateTxSkel $ txSkel [PaysScript CF.crowdFundingValidator myDatum (ada 2)]

-- A peer can fund a project
fundProject :: MonadBlockChain m => Wallet -> Pl.BuiltinByteString -> Integer -> m ()
fundProject myWallet projectName funding =
  let myDatum = CF.Funding (walletPKHash myWallet) projectName
   in void $ validateTxSkel $ txSkelOpts (def {adjustUnbalTx = True}) [PaysScript CF.crowdFundingValidator myDatum (ada funding)]

-- A project owner can cancel a project
-- cancelProject :: MonadBlockChain m => m ()
-- cancelProject = validateTxSkel $ txSkel []

------------------------
-- Examples of traces --
------------------------

exampleTrace :: MonadMockChain m => m ()
exampleTrace = do
  t <- currentTime
  createProject alice "project1" 2_000_000 (t + month) `as` alice
  fundProject bob "project1" 3 `as` bob
  fundProject marc "project1" 4 `as` marc
