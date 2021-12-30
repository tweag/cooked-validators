{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module SplitUPLCSpec where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import qualified Ledger as Pl (scriptAddress)
import qualified Ledger.Typed.Scripts as Pl
import qualified Plutus.V1.Ledger.Ada as Pl
import PlutusTx.Builtins
import qualified PlutusTx.IsData.Class as Pl
import qualified Split
import Split.ToUPLC (splitBS)
import qualified SplitSpec
import Test.Hspec
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- | Transaction to lock some amount from a given wallet to the script
txLock :: MonadBlockChain m => Pl.TypedValidator Split.Split -> Split.SplitDatum -> m ()
txLock script datum = void $ validateTxSkel (txSkelLbl (TxLock datum) constraints)
  where
    constraints =
      [ PaysScript
          script
          [ ( datum,
              Pl.lovelaceValueOf $ Split.amount datum
            )
          ]
      ]

-- | Label for 'txLock' skeleton
newtype TxLock = TxLock Split.SplitDatum deriving (Show)

-- | Unlocks the first 'SplitDatum' where the issuer wallet is a recipient of
txUnlock :: (MonadMockChain m) => Wallet -> Pl.TypedValidator Split.Split -> m ()
txUnlock issuer script = do
  (output, datum@(Split.SplitDatum r1 r2 amount)) : _ <-
    scriptUtxosSuchThat script (SplitSpec.isARecipient issuer)
  let half = div amount 2
  let share1 = half
  let share2 = amount - half
  void $
    validateTxConstr'
      TxUnlock
      [ SpendsScript script () (output, datum),
        PaysPK r1 (Pl.lovelaceValueOf share1),
        PaysPK r2 (Pl.lovelaceValueOf share2)
      ]
      `as` issuer

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
          script <- case unsafeTypedValidatorFromBS splitBS of
            Left err -> fail "couldn't load the Split contract from its binary repr"
            Right r -> return r
          txLock script lockParams `as` wallet 1
          txUnlock (wallet 2) script,
      -- This is marked as an expected failure until we sort issue 57 out
      expectFail $
        testCase "Same address as compiled script" $
          case unsafeTypedValidatorFromBS @Split.Split splitBS of
            Left err -> assertFailure "couldn't load the Split contract from its binary repr"
            Right res ->
              let defAddr = Pl.scriptAddress $ Pl.validatorScript Split.splitValidator
                  bsAddr = Pl.scriptAddress $ Pl.validatorScript res
               in defAddr @=? bsAddr
    ]
