{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This is an interface between cooked and plutus-contract.
--
-- NB: This isn't necessary to test the contract with cooked.
module BettingGame.Endpoints where

import BettingGame
import BettingGame.OffChain
import qualified Ledger as Pl
import Playground.Contract (mkSchemaDefinitions)
import qualified Plutus.Contract as C

type BettingGameSchema =
  C.Endpoint "start" BetParams
    C..\/ C.Endpoint "bet" (BetParams, (GameResult, Pl.Value))
    C..\/ C.Endpoint "collectBets" BetParams
    C..\/ C.Endpoint "close" (BetParams, GameResult)
    C..\/ C.Endpoint "reclaim" BetParams

mkSchemaDefinitions ''BettingGameSchema

endpoints :: (C.AsContractError e) => C.Promise w BettingGameSchema e ()
endpoints =
  C.endpoint @"start" txStart
    `C.select` C.endpoint @"bet" txBet
    `C.select` C.endpoint @"collectBets" txCollectBets
    `C.select` C.endpoint @"close" txClose
    `C.select` C.endpoint @"reclaim" txReclaim
