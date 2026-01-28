module Spec.Tweak.Labels where

import Control.Monad
import Cooked
import Data.Set qualified as Set
import Data.Text (Text)
import Optics.Core
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1 qualified as Api
import Test.Tasty

alice, bob, carrie :: Wallet
alice = wallet 1
bob = wallet 2
carrie = wallet 3

payTo :: Wallet -> Integer -> StagedMockChain ()
payTo target amount = do
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelOuts = [target `receives` Value (Script.ada amount)]
      }

payments :: StagedMockChain ()
payments = do
  payTo alice 10
  payTo bob 5
  payTo bob 8
  payTo alice 25
  payTo alice 32

labelAmountTweak :: StagedTweak ()
labelAmountTweak = do
  [target] <- viewAllTweak (txSkelOutsL % _head % txSkelOutValueL % valueLovelaceL)
  addLabelTweak $ Api.getLovelace target

labelNameTweak :: StagedTweak ()
labelNameTweak = do
  target <-
    viewAllTweak
      ( txSkelOutsL
          % _head
          % txSkelOutOwnerL
          % userEitherPubKeyP
          % userTypedPubKeyAT @Wallet
      )
  case target of
    [t] | t == alice -> addLabelTweak @Text "Alice"
    [t] | t == bob -> addLabelTweak @Text "Bob"
    _ -> mzero

labelNames :: StagedMockChain ()
labelNames = everywhere labelNameTweak payments

tests :: TestTree
tests =
  testGroup
    "Label Tweaks"
    [ testCooked "Adding labels everywhere" $ mustSucceedTest $ everywhere labelNameTweak payments,
      testCooked "Adding labels everywhere, but if fails somewhere" $
        mustFailTest $
          everywhere labelNameTweak $
            there
              0
              (redirectOutputTweakAll (const (Just carrie)) (== 0))
              payments,
      testCooked "Adding labels whenever possible" $
        mustSucceedTest $
          whenAble labelNameTweak $
            there
              0
              (redirectOutputTweakAll (const (Just carrie)) (== 0))
              payments,
      testCooked "Applying a modification to all transactions with a given exact label" $
        mustSucceedTest $
          whenAble (labelled' "Alice" labelAmountTweak) $
            everywhere labelNameTweak payments,
      testCooked "Apply a modification to all transactions with a given type of label"
        $ mustSucceedTest
        $ everywhere
          ( do
              txSkelLabels <- viewAllTweak $ txSkelLabelsL % to Set.toList % traversed % txSkelLabelTypedP @Text
              guard $ not $ null txSkelLabels
              labelAmountTweak
          )
        $ everywhere labelNameTweak payments
    ]
