module Cooked.QuickValueSpec (tests) where

import Cooked
import Data.Default
import qualified Data.Map as Map
import qualified Ledger.Ada as Pl
import qualified Ledger.Value as Pl
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testCase "make it possible to provide additional assets in the initial state" $
      assertBool "doesn't satisfy" $
        isRightAndSatifies
          (hasQuickValueAmount (wallet 1) "goldenCoins" 20)
          quickValuesInitialisation,
    testCase "are exchangeable like any other asset between wallets" $
      assertBool "doesn't satisfy" $
        isRightAndSatifies
          (hasQuickValueAmount (wallet 2) "goldenCoins" 12)
          paymentAfterCustomInitialization
  ]

customInitialDistribution :: InitialDistribution
customInitialDistribution =
  initialDistribution' [(wallet 1, [minAda <> quickValue "goldenCoins" 20])]

paymentAfterCustomInitialization :: Either MockChainError ((), UtxoState)
paymentAfterCustomInitialization =
  runMockChainFrom customInitialDistribution $ do
    validateTxConstrOpts
      -- we have to adjust the tx in order for it not to fail with ValueLessThanMinAda
      (def {adjustUnbalTx = True})
      [paysPK (walletPKHash $ wallet 2) (quickValue "goldenCoins" 12)]
    return ()

quickValuesInitialisation :: Either MockChainError ((), UtxoState)
quickValuesInitialisation = runMockChainFrom customInitialDistribution (return ())

hasQuickValueAmount :: Wallet -> String -> Integer -> UtxoState -> Bool
hasQuickValueAmount wallet quickValueName amount (UtxoState state) =
  case Map.lookup (walletAddress wallet) state of
    Nothing -> False
    Just (UtxoValueSet valuesAndDatums) ->
      amount
        == Pl.assetClassValueOf
          (mconcat (map fst valuesAndDatums))
          (quickAssetClass quickValueName)

isRightAndSatifies :: (UtxoState -> Bool) -> Either e (a, UtxoState) -> Bool
isRightAndSatifies property (Right (_, state)) = property state
isRightAndSatifies _ _ = False
