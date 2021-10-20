module TestSplit where

import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Generator
import qualified Plutus.V1.Ledger.Ada as Pl
import qualified Split

-- | Transaction to lock some amount from a given wallet to the script
txLock :: Wallet -> Split.SplitParams -> MockChain TxSkel
txLock w splitParams = return (TxSkel w constraints)
  where
    constraints =
      [ PaysScript
          Split.splitValidator
          [ ( Split.makeDatum splitParams,
              Pl.lovelaceValueOf $ Split.amount splitParams
            )
          ]
      ]

-- | Whether a script output concerns a given wallet (i.e. the wallet is a
-- recipient)
isARecipient :: Wallet -> Split.SplitDatum -> a -> Bool
isARecipient w datum _ =
  let wHash = walletPKHash w
   in elem wHash [Split.datumRecipient1 datum, Split.datumRecipient2 datum]

-- | Transaction to unlock (that is split among recipients) money from a script
-- UTxO which has the given wallet as a recipient in the datum
txUnlock :: Wallet -> MockChain TxSkel
txUnlock w = do
  (output, datum@(Split.SplitDatum r1 r2 amount)) : _ <-
    scriptUtxosSuchThat Split.splitValidator (isARecipient w)
  let half = div amount 2
  return
    ( TxSkel
        w
        [ SpendsScript Split.splitValidator () (output, datum),
          PaysPK r1 (Pl.lovelaceValueOf half),
          PaysPK r2 (Pl.lovelaceValueOf (amount - half))
        ]
    )

-- | Transaction to unlock (that is split among recipients) money from a script
-- UTxO which has the given wallet as a recipient in the datum.
-- This transaction violates the contract (duplicates instead of splitting)
txUnlockTooMuch :: Wallet -> MockChain TxSkel
txUnlockTooMuch w = do
  (output, datum@(Split.SplitDatum r1 r2 amount)) : _ <-
    scriptUtxosSuchThat Split.splitValidator (isARecipient w)
  return
    ( TxSkel
        w
        [ SpendsScript Split.splitValidator () (output, datum),
          PaysPK r1 (Pl.lovelaceValueOf amount),
          PaysPK r2 (Pl.lovelaceValueOf amount)
        ]
    )

-- | Transaction to unlock (that is split among recipients) money from a script
-- UTxO which has the given wallet as a recipient in the datum.
-- This transaction violates the contract (gives thirds instead of halves)
txUnlockNotEnough :: Wallet -> MockChain TxSkel
txUnlockNotEnough w = do
  (output, datum@(Split.SplitDatum r1 r2 amount)) : _ <-
    scriptUtxosSuchThat Split.splitValidator (isARecipient w)
  let third = div amount 3
  return
    ( TxSkel
        w
        [ SpendsScript Split.splitValidator () (output, datum),
          PaysPK r1 (Pl.lovelaceValueOf third),
          PaysPK r2 (Pl.lovelaceValueOf third),
          let remainder = amount - (2 * third)
           in PaysScript
                Split.splitValidator
                [ ( Split.SplitDatum r1 r2 remainder,
                    Pl.lovelaceValueOf remainder
                  )
                ]
        ]
    )

-- | Parameters to share 400 among wallets 2 and 3
run1LockParams :: Split.SplitParams
run1LockParams =
  Split.SplitParams
    { Split.recipient1 = walletPK (wallet 2),
      Split.recipient2 = walletPK (wallet 3),
      Split.amount = 400
    }

-- | Regular run
run1 :: Either MockChainError ((), UtxoState)
run1 = runMockChain $ do
  txLock (wallet 1) run1LockParams >>= validateTxFromSkeleton
  txUnlock (wallet 2) >>= validateTxFromSkeleton

-- | Faulty run
run2 :: Either MockChainError ((), UtxoState)
run2 = runMockChain $ do
  txLock (wallet 1) run1LockParams >>= validateTxFromSkeleton
  txUnlockTooMuch (wallet 2) >>= validateTxFromSkeleton

-- | Faulty run
run3 :: Either MockChainError ((), UtxoState)
run3 = runMockChain $ do
  txLock (wallet 1) run1LockParams >>= validateTxFromSkeleton
  txUnlockNotEnough (wallet 2) >>= validateTxFromSkeleton
