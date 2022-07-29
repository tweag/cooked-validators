{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module BettingGame.OffChain where

import BettingGame
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default (def)
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Plutus.V1.Ledger.Api as Api
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx.Prelude as Pl (sum, (-))

-- * Transaction Skeleton Generators

minValue :: Pl.Value
minValue = Pl.lovelaceValueOf 2000000

txStart :: MonadBlockChain m => BetParams -> m ()
txStart p =
  void $
    validateTxConstrLbl
      (TxStart p)
      [ PaysScript
          (betValidator p)
          (GameStart p)
          minValue
      ]

-- | Label for 'txStart' skeleton
newtype TxStart = TxStart BetParams deriving (Show, Eq)

txBet :: MonadBlockChain m => (BetParams, (GameResult, Pl.Value)) -> m ()
txBet (p, (gr, v)) = do
  player <- ownPaymentPubKeyHash
  let d = BetData player gr v
  void $
    validateTxConstrLbl
      (TxBet p d)
      ( [Before (bettingDeadline p)]
          :=>: [PaysScript (betValidator p) (Bet d) v]
      )

-- | Label for 'txBet' skeleton
data TxBet = TxBet BetParams BetData deriving (Show, Eq)

txCollectBets :: MonadBlockChain m => BetParams -> m ()
txCollectBets p = do
  let script = betValidator p
  betOutputs <- scriptUtxosSuchThat script isValidBet
  gameStartOutput : _ <- scriptUtxosSuchThat script isGameStart

  let bets = map betFromOutput betOutputs
  void $
    validateTxConstrLbl
      TxCollectBets
      ( ( ValidateIn
            ( Pl.Interval
                (Pl.strictLowerBound $ bettingDeadline p)
                (Pl.upperBound $ collectionDeadline p)
            ) :
          map (SpendsScript script (BetCollection bets)) (gameStartOutput : betOutputs)
        )
          :=>: [PaysScript script (CollectedBets bets) (Api.unionWith max (Pl.sum (map amount bets)) minValue)]
      )
  where
    isValidBet (Bet d) v =
      v == amount d
    isValidBet _ _ = False

    isGameStart (GameStart p') _ = p == p'
    isGameStart _ _ = False

    betFromOutput (_, Bet betData) = betData
    betFromOutput _ = error "unexpected datum in bet"

-- | Label for 'txCollectBets' skeleton
data TxCollectBets = TxCollectBets deriving (Show, Eq)

txClose :: MonadBlockChain m => (BetParams, GameResult) -> m ()
txClose (p, gr0) = do
  let script = betValidator p
  -- TODO: check that the collected bets are legit and not forged by an
  -- attacker.
  collectedBetsOutput@(_, CollectedBets bets) : _ <- scriptUtxosSuchThat script isCollectedBets
  let commission = Pl.lovelaceValueOf (fromIntegral (1000000 * length bets))
  void $
    validateTxSkel $
      txSkelLblOpts
        (TxClose p)
        def -- print the actual transaction to stderr
        -- (def { unsafeModTx = RawModTxAfterBalancing Debug.Trace.traceShowId })
        ( [ ValidateIn
              ( Pl.Interval
                  (Pl.strictLowerBound $ bettingDeadline p)
                  (Pl.upperBound $ publishingDeadline p)
              ),
            SpendsScript script (GameClose gr0) collectedBetsOutput
          ]
            :=>: ( paysPK (operator p) commission :
                     [paysPK (player b) v | b <- bets, Just v <- [paidToBet commission gr0 bets b]]
                 )
        )
  where
    isCollectedBets CollectedBets {} _ = True
    isCollectedBets _ _ = False

    paidToBet :: Pl.Value -> GameResult -> [BetData] -> BetData -> Maybe Pl.Value
    paidToBet commission gr bets b =
      let winners = filter ((gr ==) . gameResult) bets
          total_winners = Pl.sum (map amount winners)
          total = Pl.sum (map amount bets) Pl.- commission
       in if gameResult b /= gr
            then Nothing
            else
              Just $
                Api.unionWith
                  Builtins.divideInteger
                  (Api.unionWith (*) total (amount b))
                  total_winners

-- | Label for 'txClose' skeleton
data TxClose = TxClose BetParams deriving (Show, Eq)

txReclaim :: MonadBlockChain m => BetParams -> m ()
txReclaim p = do
  let script = betValidator p
  ply <- ownPaymentPubKeyHash
  betOutputs <- scriptUtxosSuchThat script (isBetOf ply)
  unless (null betOutputs) $ do
    -- TODO: Allow to reclaim bets where the amount doesn't
    -- match the output value
    let total = Pl.sum [amount d | (_, Bet d) <- betOutputs]
    void $
      validateTxConstrLbl
        TxReclaim
        ( ( After (collectionDeadline p) :
            map (SpendsScript script BetReclaim) betOutputs
          )
            :=>: [paysPK ply total]
        )
  collectedBetsOutputs <- scriptUtxosSuchThat script isCollectedBets
  case collectedBetsOutputs of
    collectedBetsOutput@(_, CollectedBets bets) : _ ->
      void $
        validateTxConstrLbl
          TxReclaim
          ( [ After (publishingDeadline p),
              SpendsScript script BetReclaim collectedBetsOutput
            ]
              :=>: [paysPK (player b) (amount b) | b <- bets]
          )
    _ -> return ()
  where
    isBetOf ply (Bet d) _ = ply == player d
    isBetOf _ _ _ = False

    isCollectedBets CollectedBets {} _ = True
    isCollectedBets _ _ = False

-- | Label for 'txReclaim' skeleton
data TxReclaim = TxReclaim deriving (Show, Eq)
