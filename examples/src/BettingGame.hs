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

-- | This module implements a betting game.
--
-- Every game has a deadline for bets, a deadline for collecting the
-- bets, and a deadline for publishing the game results.
--
-- The expected flow is as follows:
--
-- 1. Operator starts a betting game
-- 2. Players can place bets until the betting deadline expires
-- 3. The operator collects the bets after the betting deadline
--    and before the bet-collecting deadline.
-- 4. The operator publishes the game result after the bet-collecting
--    deadline and before the publishing deadline, and it distributes
--    the money among the winners
-- 5. Players are able to get their money back if the operator
--    does not publish the game results before the publishing
--    deadline.
--
-- The operator can only close the game once. In particular, she
-- shouldn't be able to close the game more than once with
-- different sets of bets.
--
-- Bets should be possible to do in parallel. Note that this
-- requirement makes impossible:
-- * to guarantee that the operator doesn't leave any bets unconsumed.
-- * to stop players from betting more than once.
--
-- The operator must collect the bets ahead of knowing the result
-- of the game. Otherwise, she could chose to only collect the bets of
-- the losers and never pay any money back to the winners.
--
-- If a player bets more than once, the on-chain validation doesn't ensure
-- that the operator pays her for all her winning bets.
--
module BettingGame where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Api as Api
import Plutus.V1.Ledger.Contexts (ScriptContext (..))
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))
import Schema (ToSchema)
import qualified Prelude as Haskell


-- * On-chain validation
--
-- $contract-flow
-- Contract flow
--
-- * The operator creates an output to the script with datum @GameStart@
-- * The betders pay to the script with datum @Bet GameResult@
-- * The operator collects the bets and pays to the script with all of
--   the collected money and datum @CollectedBets _bets@.
-- * The operator publishes the result and pays to the winners.
--
-- If the operator does not collect, players reclaim their individual bets.
-- If the operator does not publish the result, anyone can return the
-- collected bets to all players.

data BetParams = BetParams
  { bettingDeadline :: Ledger.POSIXTime
  , collectionDeadline :: Ledger.POSIXTime
  , publishingDeadline :: Ledger.POSIXTime
  , minimumBet :: Ledger.Value
  , description :: BuiltinByteString
  , operator :: Ledger.PubKeyHash
  }
  deriving stock (Haskell.Show, Haskell.Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type GameResult = Bool

data BetData = BetData
  { player :: Ledger.PubKeyHash
  , gameResult :: GameResult
  , amount :: Ledger.Value
  }
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

instance Eq BetData where
  BetData a b c == BetData a' b' c' = a == a' && b == b' && c == c'

data BetDatum
  = GameStart BetParams
  | Bet BetData
  | -- | Contains the players, with duplicates if they bet more than once.
    CollectedBets [BetData]
  deriving stock (Haskell.Show, Haskell.Eq, Generic)

data BetRedeemer
  = BetCollection [BetData]
  | GameClose GameResult
  | BetReclaim

minLovelace :: Integer
minLovelace = 2000000

validateBet
  :: BetParams -> BetDatum -> BetRedeemer -> ScriptContext -> Bool
validateBet p d0 r ctx =
    let info :: Ledger.TxInfo
        info = Ledger.scriptContextTxInfo ctx

        outputs = Ledger.txInfoOutputs info

        outputHasCollectedBets bets =
          case Ledger.findDatumHash
                 (Api.Datum $ PlutusTx.toBuiltinData $ CollectedBets bets)
                 info
            of
            Just h ->
              any (\o ->    Just h == Ledger.txOutDatumHash o
                         && Ledger.txOutValue o == sum (map amount bets)
                  )
                  outputs
            Nothing -> False

        paysTo h v = Ledger.valuePaidTo info h == v

        paysToAllWinners gr bets =
          let winners = filter ((gr ==) . gameResult) bets
              total_winners = sum (map amount winners)
              commission = Ada.lovelaceValueOf (1000000 * length bets)
              total = sum (map amount bets) - commission
              winnerIsPaid b =
                if gameResult b /= gr then True
                else
                  paysTo
                    (player b)
                    (Api.unionWith Builtins.divideInteger
                       (Api.unionWith (*) total (amount b))
                       total_winners
                    )
           in
              all winnerIsPaid bets

     in case r of
      BetCollection bets ->
        case d0 of
          GameStart _p ->
             traceIfFalse "only the operator can collect the output of gamestart"
               (elem (operator p) $ Ledger.txInfoSignatories info)
          Bet bet ->
               traceIfFalse "collection deadline expired"
                 (Ledger.to (collectionDeadline p) `Ledger.contains` Ledger.txInfoValidRange info)
            && traceIfFalse "the transaction must consume an output with datum GameStart"
                 (isJust $ findInputWithDatum (GameStart p) info)
            && traceIfFalse "there must be an output containing the list of bets"
                 (outputHasCollectedBets bets)
            && traceIfFalse "the bet of this output must be included in the list of bets in the transaction output"
                 (elem bet bets)
          _ ->
             traceIfFalse "BetCollection can only take outputs with GameStart and Bet datums" False
      GameClose gr ->
        case d0 of
          CollectedBets bets ->
               traceIfFalse "publication deadline expired"
                 (Ledger.to (publishingDeadline p) `Ledger.contains` Ledger.txInfoValidRange info)
            && traceIfFalse "betting deadline hasn't expired"
                 (bettingDeadline p `Ledger.before` Ledger.txInfoValidRange info)
            && traceIfFalse "the operator must earn a commission"
                 (paysTo (operator p) (Ada.lovelaceValueOf (1000000 * length bets)))
            && traceIfFalse "all winners must earn in proportion to what they bet"
                 (paysToAllWinners gr bets)
            && traceIfFalse "the transaction must be signed by the operator (so we can trust the result)"
                 (elem (operator p) $ Ledger.txInfoSignatories info)
          _ ->
            traceIfFalse "GameClose can only take an output with datum BetCollection" False

      BetReclaim ->
        case d0 of
          Bet bet ->
               traceIfFalse "collection deadline must have expired"
                 (collectionDeadline p `Ledger.before` Ledger.txInfoValidRange info)
            && traceIfFalse "the transaction must pay to the player"
                 (paysTo (player bet) (amount bet))
          CollectedBets bets ->
               traceIfFalse "publication deadline must have expired"
                 (publishingDeadline p `Ledger.before` Ledger.txInfoValidRange info)
            && traceIfFalse "the transaction must pay to all players"
                 (all (\b -> paysTo (player b) (amount b)) bets)
          _ ->
             traceIfFalse "BetReclaim can only take outputs with Bet and BetCollection datums" False
  where
    findInputWithDatum :: BetDatum -> Ledger.TxInfo -> Maybe Ledger.TxInInfo
    findInputWithDatum d info =
      case Ledger.findDatumHash
             (Api.Datum $ PlutusTx.toBuiltinData d)
             info
        of
        Just h ->
          let hasDatum =
                (Just h ==) . Ledger.txOutDatumHash . Ledger.txInInfoResolved
           in find hasDatum (Ledger.txInfoInputs info)
        Nothing -> Nothing

-- Plutus boilerplate

data BettingGame

PlutusTx.makeLift ''BetParams
PlutusTx.unstableMakeIsData ''BetData
PlutusTx.unstableMakeIsData ''BetDatum
PlutusTx.unstableMakeIsData ''BetParams
PlutusTx.unstableMakeIsData ''BetRedeemer

instance Scripts.ValidatorTypes BettingGame where
  type RedeemerType BettingGame = BetRedeemer
  type DatumType BettingGame = BetDatum

betValidator :: BetParams -> Scripts.TypedValidator BettingGame
betValidator p =
  Scripts.mkTypedValidator @BettingGame
    ($$(PlutusTx.compile [||validateBet||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @BetDatum @BetRedeemer
