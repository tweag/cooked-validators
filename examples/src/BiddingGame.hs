{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | This module implements a bidding game.
--
-- Every game has a deadline for bids, a deadline for collecting the
-- bids, and a deadline for publishing the game results.
--
-- The expected flow is as follows:
--
-- 1. Operator starts a bidding game
-- 2. Players can place bids until the bidding deadline expires
-- 3. The operator collects the bids after the bidding deadline
--    and before the bid-collecting deadline.
-- 4. The operator publishes the game result after the bid-collecting
--    deadline and before the publishing deadline, and it distributes
--    the money among the winners
-- 5. The winners are able to get their money back if the operator
--    does not publish the game results before the publishing
--    deadline.
--
-- The operator can only close the game once. In particular, she
-- shouldn't be able to close the game more than once with
-- different sets of bids.
--
-- Bids should be possible to do in parallel. Note that this
-- requirement makes impossible:
-- * to guarantee that the operator doesn't leave any bids unconsumed.
-- * to stop bidders from bidding more than once.
--
-- The operator must collect the bids ahead of knowing the result
-- of the game. Otherwise, she could chose to only collect the bids of
-- the losers and never pay any money back to the winners.
--
module BiddingGame where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value (geq)
import Plutus.V1.Ledger.Contexts (ScriptContext (..))
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))
import Schema (ToSchema)
import qualified Prelude as Haskell


data BidParams = BidParams
  { biddingDeadline :: Ledger.POSIXTime
  , publishingDeadline :: Ledger.POSIXTime
  , minimumBid :: Integer
  , description :: String
  , operator :: Ledger.PubKeyHash
  }
  deriving stock (Haskell.Show, Haskell.Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type GameResult = Bool
type BidDatum = ()

data BidRedeemer
  = GameStart
  | Bid GameResult
  | BidCollection
  | GameClose GameResult
  | BidReclaim

validateBid
  :: BidParams -> BidDatum -> BidRedeemer -> ScriptContext -> Bool
validateBid o d r s = case r of
    GameStart ->
      traceIfFalse "the bidding deadline needs to come before the publishing deadline" False
      && traceIfFalse "there must be an output for the operator" False
    Bid gr ->
      traceIfFalse "bidding deadline expired" False
      && traceIfFalse "bid is too small" False
    BidCollection ->
         traceIfFalse "collecting bids needs to consume the output of the game start" False
      && traceIfFalse "there must be an output containing the list of bidders and the total money that each one bet" False
      && traceIfFalse "there must not be other outputs" False
      && traceIfFalse "the bidder of this output must be included in the list of bidders in the transaction output" False
    GameClose gr ->
         traceIfFalse "publication deadline expired" False
      && traceIfFalse "bidding deadline hasn't expired" False
      && traceIfFalse "the transaction must consume the output of exactly one bid collecting transaction" False
      && traceIfFalse "the operator must earns a commission" False
      && traceIfFalse "all winners must earn in proportion to what they bid" False
      && traceIfFalse "nobody else is allowed to earn anything" False
      && traceIfFalse "the transaction must be signed by the operator (so we can trust the result)" False
    BidReclaim ->
         traceIfFalse "publication deadline must have expired" False
      && traceIfFalse "the transaction must be signed by the bidder" False

-- Plutus boilerplate

data BiddingGame

PlutusTx.makeLift ''BidDatum
PlutusTx.unstableMakeIsData ''BidDatum

instance Scripts.ValidatorTypes BiddingGame where
  type RedeemerType Split = ()
  type DatumType Split = SplitDatum

bidValidator :: Scripts.TypedValidator BidDatum
bidValidator =
  Scripts.mkTypedValidator @BiddingGame
    $$(PlutusTx.compile [||validateBid||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @SplitDatum @SplitRedeemer
