module TestSplit where

import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Generator
import qualified Plutus.V1.Ledger.Ada as Pl
import qualified Split

txLockAmount :: Wallet -> Split.SplitParams -> MockChain TxSkel
txLockAmount w splitParams = return (TxSkel w constraints)
  where
    constraints =
      [ PaysScript
          Split.splitValidator
          [ ( Split.makeDatum splitParams,
              Pl.lovelaceValueOf $ Split.amount splitParams
            )
          ]
      ]

isARecipient :: Wallet -> Split.SplitDatum -> a -> Bool
isARecipient w datum _ =
  let wHash = walletPKHash w
   in elem wHash [Split.datumRecipient1 datum, Split.datumRecipient2 datum]

txUnlockAmount :: Wallet -> MockChain TxSkel
txUnlockAmount w = do
  (output, datum) : _ <- scriptUtxosSuchThat Split.splitValidator (isARecipient w)
  let half = div (Split.datumAmount datum) 2
      constraints1 = SpendsScript Split.splitValidator () (output, datum)
      constraints2 =
        PaysPK (Split.datumRecipient1 datum) (Pl.lovelaceValueOf half)
      constraints3 =
        PaysPK (Split.datumRecipient2 datum) (Pl.lovelaceValueOf (Split.datumAmount datum - half))
  return (TxSkel w [constraints1, constraints2, constraints3])

run1LockParams :: Split.SplitParams
run1LockParams =
  Split.SplitParams
    { Split.recipient1 = walletPK (wallet 2),
      Split.recipient2 = walletPK (wallet 3),
      Split.amount = 400
    }

run1 :: Either MockChainError ((), UtxoState)
run1 = runMockChain $ do
  txLockAmount (wallet 1) run1LockParams >>= validateTxFromSkeleton
  txUnlockAmount (wallet 2) >>= validateTxFromSkeleton
