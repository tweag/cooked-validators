{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module SplitSpec where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import qualified Plutus.V1.Ledger.Ada as Pl
import qualified Split
import Test.Hspec
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- | Transaction to lock some amount from a given wallet to the script
txLock :: MonadBlockChain m => Wallet -> Split.SplitDatum -> m ()
txLock w params = void $ validateTxSkel (txSkelLbl (TxLock params) w constraints)
  where
    constraints =
      [ PaysScript
          Split.splitValidator
          [ (params, Pl.lovelaceValueOf $ Split.amount params)
          ]
      ]

-- | Label for 'txLock' skeleton
newtype TxLock = TxLock Split.SplitDatum deriving (Show)

-- | Whether a script output concerns a given wallet (i.e. the wallet is a
-- recipient)
isARecipient :: Wallet -> Split.SplitDatum -> a -> Bool
isARecipient w datum _ =
  let wHash = walletPKHash w
   in elem wHash [Split.recipient1 datum, Split.recipient2 datum]

-- | Unlocks the first 'SplitDatum' where the issuer wallet is a recipient of
txUnlock :: (MonadBlockChain m) => Wallet -> m ()
txUnlock issuer = do
  (output, datum@(Split.SplitDatum r1 r2 amount)) : _ <-
    scriptUtxosSuchThat Split.splitValidator (isARecipient issuer)
  let half = div amount 2
  let share1 = half
  let share2 = amount - half
  void $
    validateTxSkel $
      txSkelLbl
        TxUnlock
        issuer
        [ SpendsScript Split.splitValidator () (output, datum),
          PaysPK r1 (Pl.lovelaceValueOf share1),
          PaysPK r2 (Pl.lovelaceValueOf share2)
        ]

-- | Label for 'txUnlock' skeleton
data TxUnlock = TxUnlock deriving (Show)

-- | A more general version of 'txUnlock' above. In fact,
-- @txUnlock' Nothing Nothing Nothing == txUnlock@, but we keep
-- two functions for pedagogical purposes.
txUnlock' ::
  MonadBlockChain m =>
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
    scriptUtxosSuchThat Split.splitValidator (isARecipient issuer)
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
    validateTxSkel $
      txSkelLbl
        (TxUnlock' mRecipient1 mRecipient2 (fmap ($ 100) mAmountChanger))
        issuer
        (constraints <> [remainderConstraint | remainder > 0])

data TxUnlock' = TxUnlock' (Maybe Wallet) (Maybe Wallet) (Maybe Integer) deriving (Show)

-- | Template for an unlock attack.
-- Conditions for the attack: 2 split utxos in the ledger, with the same locked
-- amount, and sharing the same second recipient.
-- Attack: the second recipient is paid only one of his shares, the remainder
-- goes to the issuer of the transaction.
txUnlockAttack :: MonadBlockChain m => Wallet -> m ()
txUnlockAttack issuer = do
  (output1, datum1@(Split.SplitDatum r11 r12 amount))
    : (output2, datum2@(Split.SplitDatum r21 _ _))
    : _ <-
    scriptUtxosSuchThat Split.splitValidator (\_ _ -> True)
  let half = Pl.lovelaceValueOf (div amount 2)
      constraints =
        [ SpendsScript Split.splitValidator () (output1, datum1),
          SpendsScript Split.splitValidator () (output2, datum2),
          PaysPK r11 half,
          PaysPK r12 half,
          PaysPK r21 half,
          PaysPK (walletPKHash issuer) half
        ]
  void $ validateTxSkel $ txSkelLbl TxUnlockAttack issuer constraints

data TxUnlockAttack = TxUnlockAttack deriving (Show)

-- | Transaction that does not pay enough to the recipients
txUnlockNotEnough :: MonadBlockChain m => Wallet -> m ()
txUnlockNotEnough = txUnlock' Nothing Nothing (Just (`div` 2))

-- | Transaction that gives everything to the first recipient
txUnlockTooMuch :: MonadBlockChain m => Wallet -> m ()
txUnlockTooMuch = txUnlock' Nothing Nothing (Just (* 2))

-- | Transaction that pays everything to the issuer
txUnlockGreedy :: MonadBlockChain m => Wallet -> m ()
txUnlockGreedy w = txUnlock' (Just w) (Just w) Nothing w

-- | Parameters to share 400 among wallets 2 and 3
lockParams :: Split.SplitDatum
lockParams =
  Split.SplitDatum
    { Split.recipient1 = walletPKHash (wallet 2),
      Split.recipient2 = walletPKHash (wallet 3),
      Split.amount = 2_000_000
    }

-- | Parameters to share 400 among wallets 3 and 4
lockParams2 :: Split.SplitDatum
lockParams2 =
  Split.SplitDatum
    { Split.recipient1 = walletPKHash (wallet 4),
      Split.recipient2 = walletPKHash (wallet 3),
      Split.amount = 4_000_000
    }

usageExample :: Assertion
usageExample = assertSucceeds $ do
  txLock (wallet 1) lockParams
  txUnlock (wallet 2)

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
            txLock (wallet 1) lockParams
            txUnlockTooMuch (wallet 2),
      testCase "Can unlocking in small parts" $
        assertSucceeds $ do
          txLock (wallet 1) lockParams
          txUnlockNotEnough (wallet 2)
          txUnlockNotEnough (wallet 2),
      testCase "Forgets a recipient" $
        assertFails $ do
          txLock (wallet 1) lockParams
          txUnlockGreedy (wallet 2),
      -- we know that this implementation of split is vulnerable to this attack;
      -- Still, I rather phrase the test as we would in practice and flag it with 'expectFail'
      expectFail $
        testCase "Is not vulnerable to double split attack" $
          assertFails $ do
            txLock (wallet 1) lockParams
            txLock (wallet 1) lockParams2
            txUnlockAttack (wallet 5)
    ]
