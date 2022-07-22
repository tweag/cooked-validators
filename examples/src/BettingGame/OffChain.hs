{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module BettingGame.OffChain where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Typed.Scripts as Pl
import Playground.Contract hiding (ownPaymentPubKeyHash)
import qualified Plutus.Contract as C
import qualified Plutus.V1.Ledger.Api as Api
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx.Prelude as Pl ((-), sum)
import BettingGame
import qualified Wallet.Emulator.Wallet as C

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

txBet :: MonadBlockChain m => BetParams -> GameResult -> Pl.Value -> m ()
txBet p gr v = do
    player <- ownPaymentPubKeyHash
    let d = BetData player gr v
    void $
      validateTxConstrLbl
        (TxBet p d)
        [ PaysScript
            (betValidator p)
            (Bet d)
            v
        ]

-- | Label for 'txBet' skeleton
data TxBet = TxBet BetParams BetData deriving (Show, Eq)

txCollectBets :: MonadBlockChain m => BetParams -> m ()
txCollectBets p = do
    let script = betValidator p
    betOutputs <- scriptUtxosSuchThat script isValidBet
    gameStartOutput : _ <-  scriptUtxosSuchThat script isGameStart

    let bets = map betFromOutput betOutputs
    void $
      validateTxConstrLbl
        TxCollectBets
        ( map (SpendsScript script (BetCollection bets)) (gameStartOutput : betOutputs)
          :=>: [ PaysScript script (CollectedBets bets) (Api.unionWith max (Pl.sum (map amount bets)) minValue) ]
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

txClose :: MonadBlockChain m => BetParams -> GameResult -> m ()
txClose p gr0 = do
    let script = betValidator p
    -- TODO: check that the collected bets are legit and not forged by an
    -- attacker.
    collectedBetsOutput@(_, CollectedBets bets) : _ <-  scriptUtxosSuchThat script isCollectedBets
    let commission = Pl.lovelaceValueOf (fromIntegral (1000000 * length bets))
    void $
      validateTxConstrLbl
        TxCollectBets
        ( [ SpendsScript script (GameClose gr0) collectedBetsOutput ]
          :=>:
          (paysPK (operator p) commission
            : [ paysPK (player b) v | b <- bets, Just v <- [paidToBet commission gr0 bets b] ]
          )
        )

  where
    isCollectedBets CollectedBets{} _ = True
    isCollectedBets _ _ = False

    paidToBet :: Pl.Value -> GameResult -> [BetData] -> BetData -> Maybe Pl.Value
    paidToBet commission gr bets b =
      let winners = filter ((gr ==) . gameResult) bets
          total_winners = Pl.sum (map amount winners)
          total = Pl.sum (map amount bets) Pl.- commission
       in if gameResult b /= gr then Nothing
          else
            Just $ Api.unionWith Builtins.divideInteger
              (Api.unionWith (*) total (amount b))
              total_winners

-- | Label for 'txCollectBets' skeleton
data TxClose = TxClose BetParams deriving (Show, Eq)


{-

-- * Transaction Skeleton Generators

-- | Transaction to lock some amount into the split contract; note that
-- we receive the split contract as parameter because we use this same function
-- in the @tests/SplitSpec.hs@ and @tests/SplitUPLCSpec.hs@. The later loads
-- the split contract as a raw untyped PlutusCore contract.
txLock :: MonadBlockChain m => Pl.TypedValidator Split -> SplitDatum -> m ()
txLock script datum =
  void $
    validateTxConstrLbl
      (TxLock datum)
      [ PaysScript
          script
          datum
          (Pl.lovelaceValueOf (Split.amount datum))
      ]

-- | Label for 'txLock' skeleton, making it immediately recognizable
-- when printing traces.
newtype TxLock = TxLock SplitDatum deriving (Show, Eq)

-- | Whether a script output concerns a public key hash
isARecipient :: Pl.PubKeyHash -> SplitDatum -> a -> Bool
isARecipient pkh datum _ = pkh `elem` [Split.recipient1 datum, Split.recipient2 datum]

-- | Unlocks the first 'SplitDatum' where our wallet is a recipient of.
txUnlock :: (MonadBlockChain m) => Pl.TypedValidator Split -> m ()
txUnlock script = do
  pkh <- ownPaymentPubKeyHash
  (output, datum@(Split.SplitDatum r1 r2 amount)) : _ <-
    scriptUtxosSuchThat script (isARecipient pkh)
  let half = div amount 2
  let share1 = half
  let share2 = amount - half
  void $
    validateTxConstrLbl
      TxUnlock
      ( [SpendsScript script () (output, datum)]
          :=>: [ paysPK r1 (Pl.lovelaceValueOf share1),
                 paysPK r2 (Pl.lovelaceValueOf share2)
               ]
      )

-- | Label for 'txUnlock' skeleton
data TxUnlock = TxUnlock deriving (Show, Eq)

-- * Contract monad endpoints and schema

data LockArgs = LockArgs
  { recipient1Wallet :: C.Wallet,
    recipient2Wallet :: C.Wallet,
    totalAda :: Pl.Ada
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type SplitSchema = C.Endpoint "lock" LockArgs C..\/ C.Endpoint "unlock" ()

mkSchemaDefinitions ''SplitSchema

mkSplitData :: LockArgs -> SplitDatum
mkSplitData LockArgs {recipient1Wallet, recipient2Wallet, totalAda} =
  let convert :: C.Wallet -> Pl.PubKeyHash
      convert = Pl.unPaymentPubKeyHash . C.mockWalletPaymentPubKeyHash
   in SplitDatum
        { recipient1 = convert recipient1Wallet,
          recipient2 = convert recipient2Wallet,
          amount = fromIntegral totalAda
        }

endpoints :: (C.AsContractError e) => C.Promise w SplitSchema e ()
endpoints =
  C.endpoint @"lock" (txLock splitValidator . mkSplitData)
    `C.select` C.endpoint @"unlock" (const $ txUnlock splitValidator)
-}
