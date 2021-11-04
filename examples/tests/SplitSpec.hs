{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module SplitSpec where

import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Generator
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import qualified Plutus.V1.Ledger.Ada as Pl
import qualified Split
import Test.Hspec

-- | Transaction to lock some amount from a given wallet to the script
txLock :: Wallet -> Split.SplitParams -> MockChain TxSkel
txLock w splitParams = return (TxSkel w constraints)
  where
    constraints =
      [ PaysScript
          Split.splitValidator
          [
            ( Split.makeDatum splitParams
            , Pl.lovelaceValueOf $ Split.amount splitParams
            )
          ]
      ]

-- | Whether a script output concerns a given wallet (i.e. the wallet is a
-- recipient)
isARecipient :: Wallet -> Split.SplitDatum -> a -> Bool
isARecipient w datum _ =
  let wHash = walletPKHash w
   in elem wHash [Split.datumRecipient1 datum, Split.datumRecipient2 datum]

-- | Template for unlock (that is split among recipients).
txUnlockTemplate ::
  -- | Optionally override first recipient
  Maybe Wallet ->
  -- | Optionally override second recipient
  Maybe Wallet ->
  -- | Optionally override the amount given to recipient
  Maybe (Integer -> Integer) ->
  -- | Issuer
  Wallet ->
  MockChain TxSkel
txUnlockTemplate mRecipient1 mRecipient2 mAmountChanger issuer = do
  (output, datum@(Split.SplitDatum r1 r2 amount)) : _ <-
    scriptUtxosSuchThat Split.splitValidator (isARecipient issuer)
  return $
    let half = div amount 2
        share1 = fromMaybe id mAmountChanger half
        share2 = fromMaybe id mAmountChanger (amount - half)
        constraints =
          [ SpendsScript Split.splitValidator () (output, datum)
          , PaysPK (maybe r1 walletPKHash mRecipient1) (Pl.lovelaceValueOf share1)
          , PaysPK (maybe r2 walletPKHash mRecipient2) (Pl.lovelaceValueOf share2)
          ]
        remainder = amount - share1 - share2
        remainderConstraint =
          PaysScript
            Split.splitValidator
            [
              ( Split.SplitDatum r1 r2 remainder
              , Pl.lovelaceValueOf remainder
              )
            ]
     in TxSkel
          issuer
          (constraints <> [remainderConstraint | remainder > 0])

-- | Legit transaction
txUnlock :: Wallet -> MockChain TxSkel
txUnlock = txUnlockTemplate Nothing Nothing Nothing

-- | Transaction that does not pay enough to the recipients
txUnlockNotEnough :: Wallet -> MockChain TxSkel
txUnlockNotEnough = txUnlockTemplate Nothing Nothing (Just (`div` 2))

-- | Transaction that gives everything to the first recipient
txUnlockTooMuch :: Wallet -> MockChain TxSkel
txUnlockTooMuch = txUnlockTemplate Nothing Nothing (Just (* 2))

-- | Transaction that pays everything to the issuer
txUnlockGreedy :: Wallet -> MockChain TxSkel
txUnlockGreedy w = txUnlockTemplate (Just w) (Just w) Nothing w

-- | Parameters to share 400 among wallets 2 and 3
run1LockParams :: Split.SplitParams
run1LockParams =
  Split.SplitParams
    { Split.recipient1 = walletPK (wallet 2)
    , Split.recipient2 = walletPK (wallet 3)
    , Split.amount = 400
    }

-- | Regular run
run1 :: Either MockChainError ((), UtxoState)
run1 = runMockChain $ do
  txLock (wallet 1) run1LockParams >>= validateTxFromSkeleton
  txUnlock (wallet 2) >>= validateTxFromSkeleton

-- | Run containing only a paiement to the script
runIncomplete :: Either MockChainError ((), UtxoState)
runIncomplete = runMockChain $ do
  txLock (wallet 1) run1LockParams >>= validateTxFromSkeleton

-- | Valid run with overpayment
run2 :: Either MockChainError ((), UtxoState)
run2 = runMockChain $ do
  txLock (wallet 1) run1LockParams >>= validateTxFromSkeleton
  txUnlockTooMuch (wallet 2) >>= validateTxFromSkeleton

-- | Faulty run
run3 :: Either MockChainError ((), UtxoState)
run3 = runMockChain $ do
  txLock (wallet 1) run1LockParams >>= validateTxFromSkeleton
  txUnlockNotEnough (wallet 2) >>= validateTxFromSkeleton

-- | Faulty run
run4 :: Either MockChainError ((), UtxoState)
run4 = runMockChain $ do
  txLock (wallet 1) run1LockParams >>= validateTxFromSkeleton
  txUnlockGreedy (wallet 2) >>= validateTxFromSkeleton

-- Test spec
spec :: Spec
spec = do
  it "succeeds when the amount is split among recipients" $ do
    run1 `shouldSatisfy` isRight
  it "succeeds when too much is paid to the recipients" $ do
    run2 `shouldSatisfy` isRight
  it "fails when not enough is paid to the recipients" $ do
    run3 `shouldSatisfy` isLeft
  it "fails when a recipient is forgotten" $ do
    run4 `shouldSatisfy` isLeft
