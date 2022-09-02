{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module SplitUPLCSpec where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import qualified Ledger.Ada as Pl
import qualified Ledger.Typed.Scripts as Pl
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as Scripts
import PlutusTx.Builtins
import qualified PlutusTx.IsData.Class as Pl
import qualified Split
import Split.OffChain
import Split.ToUPLC (splitBS)
import qualified SplitSpec
import Test.Hspec
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- | Parameters to share 400 among wallets 2 and 3
lockParams :: Split.SplitDatum
lockParams =
  Split.SplitDatum
    { Split.recipient1 = walletPKHash (wallet 2),
      Split.recipient2 = walletPKHash (wallet 3),
      Split.amount = 20_000_000
    }

tests :: TestTree
tests =
  testGroup
    "SplitSpec imported from UPLC"
    [ testCase "Simple example succeeds" $
        testSucceeds $ do
          script <- case unsafeTypedValidatorFromBS splitBS of
            Left err -> fail "couldn't load the Split contract from its binary repr"
            Right r -> return r
          txLock script lockParams `as` wallet 1
          txUnlock script `as` wallet 2,
      -- This is marked as an expected failure until we sort issue 57 out
      expectFail $
        testCase "Same address as compiled script" $
          case unsafeTypedValidatorFromBS @Split.Split splitBS of
            Left err -> assertFailure "couldn't load the Split contract from its binary repr"
            Right res ->
              let defAddr = Scripts.validatorAddress Split.splitValidator
                  bsAddr = Scripts.validatorAddress res
               in defAddr @=? bsAddr
    ]
