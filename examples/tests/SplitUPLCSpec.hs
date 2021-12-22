{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module SplitUPLCSpec where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import qualified Ledger.Typed.Scripts as Pl
import qualified Plutus.V1.Ledger.Ada as Pl
import PlutusTx.Builtins
import qualified PlutusTx.IsData.Class as Pl
import qualified Split
import Split.ToUPLC (splitBS)
import Test.Hspec
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- | Transaction to lock some amount from a given wallet to the script
txLock :: MonadMockChain m => Wallet -> Pl.TypedValidator Pl.Any -> Split.SplitDatum -> m ()
txLock w script datum = void $ validateTxSkel (txSkelLbl (TxLock datum) w constraints)
  where
    constraints =
      [ PaysScript
          script
          [ ( Pl.toBuiltinData datum,
              Pl.lovelaceValueOf $ Split.amount datum
            )
          ]
      ]

-- | Label for 'txLock' skeleton
newtype TxLock = TxLock Split.SplitDatum deriving (Show)

-- | Whether a script output concerns a given wallet (i.e. the wallet is a
-- recipient)
isARecipient :: Wallet -> BuiltinData -> a -> Bool
isARecipient w d _
  | Just datum <- Pl.fromBuiltinData d =
    let wHash = walletPKHash w
     in elem wHash [Split.recipient1 datum, Split.recipient2 datum]
  | otherwise = False

-- | Unlocks the first 'SplitDatum' where the issuer wallet is a recipient of
txUnlock :: (MonadMockChain m) => Wallet -> Pl.TypedValidator Pl.Any -> m ()
txUnlock issuer script = do
  (output, datum) : _ <- scriptUtxosSuchThat script (isARecipient issuer)
  let Just (Split.SplitDatum r1 r2 amount) = Pl.fromBuiltinData datum
  let half = div amount 2
  let share1 = half
  let share2 = amount - half
  void $
    validateTxSkel $
      txSkelLbl
        TxUnlock
        issuer
        [ SpendsScript script (Pl.toBuiltinData ()) (output, Pl.toBuiltinData datum),
          PaysPK r1 (Pl.lovelaceValueOf share1),
          PaysPK r2 (Pl.lovelaceValueOf share2)
        ]

-- | Label for 'txUnlock' skeleton
data TxUnlock = TxUnlock deriving (Show)

-- | Parameters to share 400 among wallets 2 and 3
lockParams :: Split.SplitDatum
lockParams =
  Split.SplitDatum
    { Split.recipient1 = walletPKHash (wallet 2),
      Split.recipient2 = walletPKHash (wallet 3),
      Split.amount = 2_000_000
    }

tests :: TestTree
tests =
  testGroup
    "SplitSpec imported from UPLC"
    [ testCase "Simple example succeeds" $
        assertSucceeds $ do
          script <- case uplcFromBS splitBS of
            Left err -> fail "couldn't load the Split contract from its binary repr"
            Right r -> return r
          txLock (wallet 1) script lockParams
          txUnlock (wallet 2) script
    ]
