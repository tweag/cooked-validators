{-# LANGUAGE NumericUnderscores #-}

module SplitSpec where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import qualified Plutus.V1.Ledger.Ada as Pl
import qualified Split
import Split.OffChain
import Test.Hspec
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- | A more general version of 'txUnlock' above. In fact,
-- @txUnlock' Nothing Nothing Nothing == txUnlock@, but we keep
-- two functions for pedagogical purposes.
txUnlock' ::
  MonadMockChain m =>
  -- | Optionally override first recipient
  Maybe Wallet ->
  -- | Optionally override second recipient
  Maybe Wallet ->
  -- | Optionally override the amount given to recipient
  Maybe (Integer -> Integer) ->
  -- | Issuer
  Wallet ->
  m ()
txUnlock' mRecipient1 mRecipient2 mAmountChanger issuer = do
  (output, datum@(Split.SplitDatum r1 r2 amount)) : _ <-
    scriptUtxosSuchThat Split.splitValidator (isARecipient $ walletPKHash issuer)
  let half = div amount 2
      share1 = fromMaybe id mAmountChanger half
      share2 = fromMaybe id mAmountChanger (amount - half)
      constraints =
        [ SpendsScript Split.splitValidator () (output, datum),
          PaysPK (maybe r1 walletPKHash mRecipient1) (Pl.lovelaceValueOf share1),
          PaysPK (maybe r2 walletPKHash mRecipient2) (Pl.lovelaceValueOf share2)
        ]
      remainder = amount - share1 - share2
      remainderConstraint =
        PaysScript
          Split.splitValidator
          [ ( Split.SplitDatum r1 r2 remainder,
              Pl.lovelaceValueOf remainder
            )
          ]
  void $
    validateTxConstrLbl
      (TxUnlock' mRecipient1 mRecipient2 (fmap ($ 100) mAmountChanger))
      (constraints <> [remainderConstraint | remainder > 0])
      `as` issuer

data TxUnlock' = TxUnlock' (Maybe Wallet) (Maybe Wallet) (Maybe Integer) deriving (Show)

-- | Template for an unlock attack.
-- Conditions for the attack: 2 split utxos in the ledger, with the same locked
-- amount, and sharing the same second recipient.
-- Attack: the second recipient is paid only one of their shares, the remainder
-- goes to the issuer of the transaction.
txUnlockAttack :: MonadMockChain m => Wallet -> m ()
txUnlockAttack issuer = do
  (output1, datum1@(Split.SplitDatum r11 r12 amount1))
    : (output2, datum2@(Split.SplitDatum r21 r22 amount2))
    : _ <-
    scriptUtxosSuchThat Split.splitValidator (\_ _ -> True)
  unless (r12 == r22) (fail "second recipiend must match")
  let half1 = Pl.lovelaceValueOf (amount1 `div` 2)
      half2 = Pl.lovelaceValueOf (amount2 `div` 2)
      constraints =
        [ SpendsScript Split.splitValidator () (output1, datum1),
          SpendsScript Split.splitValidator () (output2, datum2),
          PaysPK r11 half1,
          PaysPK r12 (if amount1 > amount2 then half1 else half2),
          PaysPK r21 half2
        ]
  void $ validateTxConstrLbl TxUnlockAttack constraints `as` issuer

data TxUnlockAttack = TxUnlockAttack deriving (Show)

-- | Transaction that does not pay enough to the recipients
txUnlockNotEnough :: MonadMockChain m => Wallet -> m ()
txUnlockNotEnough = txUnlock' Nothing Nothing (Just (`div` 2))

-- | Transaction that gives everything to the first recipient
txUnlockTooMuch :: MonadMockChain m => Wallet -> m ()
txUnlockTooMuch = txUnlock' Nothing Nothing (Just (* 2))

-- | Transaction that pays everything to the issuer
txUnlockGreedy :: MonadMockChain m => Wallet -> m ()
txUnlockGreedy w = txUnlock' (Just w) (Just w) Nothing w

-- | Parameters to share 400 among wallets 2 and 3
lockParams :: Split.SplitDatum
lockParams =
  Split.SplitDatum
    { Split.recipient1 = walletPKHash (wallet 2),
      Split.recipient2 = walletPKHash (wallet 3),
      Split.amount = 20_000_000
    }

-- | Parameters to share 400 among wallets 3 and 4
lockParams2 :: Split.SplitDatum
lockParams2 =
  Split.SplitDatum
    { Split.recipient1 = walletPKHash (wallet 4),
      Split.recipient2 = walletPKHash (wallet 3),
      Split.amount = 40_000_000
    }

usageExample :: Assertion
usageExample = assertSucceeds $ do
  txLock Split.splitValidator lockParams `as` wallet 1
  txUnlock Split.splitValidator `as` wallet 2

ex :: (MonadMockChain m) => m ()
ex = do
  txLock Split.splitValidator lockParams `as` wallet 1
  txLock Split.splitValidator lockParams2 `as` wallet 1
  txUnlock Split.splitValidator `as` wallet 2

tests :: TestTree
tests =
  testGroup
    "SplitSpec"
    [ testCase "Simple example succeeds" usageExample,
      -- TODO: afaic this test should fail; but somehow, the previous suite marked
      -- it as passing; hence, I'll add it as expectFail here
      expectFail $
        testCase "Unlocking too much" $
          assertFails $ do
            txLock Split.splitValidator lockParams `as` wallet 1
            txUnlockTooMuch (wallet 2),
      testCase "Cannot unlock in small parts" $
        assertFails $ do
          txLock Split.splitValidator lockParams `as` wallet 1
          txUnlockNotEnough (wallet 2)
          txUnlockNotEnough (wallet 2),
      testCase "Forgets a recipient" $
        assertFails $ do
          txLock Split.splitValidator lockParams `as` wallet 1
          txUnlockGreedy (wallet 2),
      -- we know that this implementation of split is vulnerable to this attack;
      -- Still, I rather phrase the test as we would in practice and flag it with 'expectFail'
      expectFail $
        testCase "Is not vulnerable to double split attack" $
          assertFails $ do
            txLock Split.splitValidator lockParams `as` wallet 1
            txLock Split.splitValidator lockParams2 `as` wallet 1
            txUnlockAttack (wallet 5)
    ]
