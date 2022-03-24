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
createProject :: MonadBlockChain m => Wallet -> Pl.BuiltinByteString -> Integer -> Pl.POSIXTime -> m Pl.TxId
createProject myWallet projectName threshold deadline = do
  let myDatum = CF.ProjectProposal (walletPKHash myWallet) projectName threshold (Pl.to deadline)
  validateTxSkel $ txSkel [PaysScript CF.validator myDatum (ada 2)]

-- A peer can fund a project
fundProject :: MonadBlockChain m => Wallet -> Pl.BuiltinByteString -> Integer -> m Pl.TxId
fundProject myWallet projectName funding = do
  let myDatum = CF.Funding (walletPKHash myWallet) projectName
  validateTxSkel $ txSkelOpts (def {adjustUnbalTx = True}) [PaysScript CF.validator myDatum (ada funding)]

-- A peer can cancel a project
cancelProject :: MonadBlockChain m => Pl.BuiltinByteString -> m [Pl.TxId]
cancelProject projectName = do
  l <- scriptUtxosSuchThat CF.validator (\d _ -> CF.retrievePName d == projectName)
  mapM (\p@(sOut,datum) -> validateTxSkel $ txSkel ([ SpendsScript CF.validator CF.Cancel p ] :=>:
                          [ paysPK (CF.retrievePKH datum) (sOutValue sOut) ])) l

------------------------
-- Examples of traces --
------------------------

exampleTrace :: MonadMockChain m => m ()
exampleTrace = void $ do
  t <- currentTime
  createProject alice "project_alice" 2_000_000 (t + month) `as` alice
  createProject bob "project_bob" 2_000_000 (t + month) `as` bob
  fundProject bob "project_alice" 3 `as` bob
  fundProject marc "project_alice" 4 `as` marc
  cancelProject "project_alice" `as` alice
