{-# LANGUAGE NumericUnderscores #-}

module Cooked.OutputReorderingSpec (tests) where

import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import Data.Either.Combinators (rightToMaybe)
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Credential as Pl
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "ordering two outputs" $
      assertBool "doesn't satisfy" $
        maybe False (firstRecipientsAre (wallet 2) (wallet 3)) $
          genTx
            (skel (wallet 2) (wallet 3)),
    testCase
      "reversing the ordering of two outputs"
      $ assertBool "satisfies" $
        not $
          maybe False (firstRecipientsAre (wallet 2) (wallet 3)) $
            genTx (skel (wallet 3) (wallet 2))
  ]

-- | Generates the transaction corresponding to a 'TxSkel' under the default
-- distribution
genTx :: TxSkel -> Maybe Pl.Tx
genTx = fmap fst . rightToMaybe . runMockChain . fmap snd . generateTx'

-- | Pays 1_000 lovelace to 2 given wallets in a transaction that forces
-- the ordering of the outputs
skel :: Wallet -> Wallet -> TxSkel
skel w1 w2 =
  txSkelOpts
    (def {forceOutputOrdering = True})
    [ paysPK (walletPKHash w1) (Pl.lovelaceValueOf 1_000),
      paysPK (walletPKHash w2) (Pl.lovelaceValueOf 1_000)
    ]

-- | Checks that the first two outputs in a transaction are payments to the two
-- given wallets
firstRecipientsAre :: Wallet -> Wallet -> Pl.Tx -> Bool
firstRecipientsAre w1 w2 tx =
  case Pl.txOutputs tx of
    (Pl.TxOut (Pl.Address (Pl.PubKeyCredential pkh1) _) _ _)
      : (Pl.TxOut (Pl.Address (Pl.PubKeyCredential pkh2) _) _ _)
      : _ -> walletPKHash w1 == pkh1 && walletPKHash w2 == pkh2
    _ -> False
