{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BettingGameSpec where

import Control.Applicative
import Control.Monad
import Cooked.Attack
import Cooked.Ltl
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default (def)
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Ledger.Ada as Pl
import qualified Ledger.Address as Pl
import qualified Plutus.V1.Ledger.Api as Pl
import qualified BettingGame
import BettingGame.OffChain
import Test.Tasty
import Test.Tasty.HUnit


testParams :: Pl.POSIXTime -> BettingGame.BetParams
testParams t = BettingGame.BetParams
  { BettingGame.bettingDeadline = t + 20_000
  , BettingGame.collectionDeadline = t + 30_000
  , BettingGame.publishingDeadline = t + 60_000
  , BettingGame.minimumBet = Pl.lovelaceValueOf 4_000_000
  , BettingGame.description =
      fromString $ "Barcelona vs Real Madrid " ++ show (t + 40_000)
  , BettingGame.operator = walletPKHash (wallet 1)
  }

tests :: TestTree
tests =
  testGroup
    "BettingGameSpec"
    [ testCase "Simple closing example" $
        testSucceeds $ do
          t0 <- currentTime
          let p = testParams t0
          txStart p `as` wallet 1
          txBet (p, (True, Pl.lovelaceValueOf 5_000_000)) `as` wallet 2
          txBet (p, (False, Pl.lovelaceValueOf 10_000_000)) `as` wallet 3
          awaitTime (t0 + 24_000)
          txCollectBets p `as` wallet 1
          awaitTime (t0 + 40_000)
          txClose (p, False) `as` wallet 1

    , testCase "Simple reclaim example" $
        testSucceeds $ do
          t0 <- currentTime
          let p = testParams t0
          txStart p `as` wallet 1
          txBet (p, (True, Pl.lovelaceValueOf 5_000_000)) `as` wallet 2
          txBet (p, (False, Pl.lovelaceValueOf 10_000_000)) `as` wallet 3
          awaitTime (t0 + 24_000)
          txCollectBets p `as` wallet 1
          awaitTime (t0 + 64_000)
          txReclaim p `as` wallet 1

    , testGroup "attacks"
      [ testCase "datum hijacking" $
          testFailsFrom'
            isCekEvaluationFailure
            def
            tryDatumHijack
      ]
    ]

tryDatumHijack :: (Alternative m, MonadModalMockChain m) => m ()
tryDatumHijack =
  somewhere
    ( datumHijackingAttack @BettingGame.BettingGame
        ( \_ d _ -> case d of
            BettingGame.CollectedBets{} -> True
            _ -> False
        )
        (0 ==)
    )
    (simpleClosingExample
     -- <|> ...
    )
    -- (noBids <|> oneBid <|> twoBids)
  where
    simpleClosingExample :: MonadMockChain m => m ()
    simpleClosingExample = do
      t0 <- currentTime
      let p = testParams t0
      txStart p `as` wallet 1
      txBet (p, (True, Pl.lovelaceValueOf 5_000_000)) `as` wallet 2
      txBet (p, (False, Pl.lovelaceValueOf 10_000_000)) `as` wallet 3
      awaitTime (t0 + 24_000)
      txCollectBets p `as` wallet 1
