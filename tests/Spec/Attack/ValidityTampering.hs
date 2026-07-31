module Spec.Attack.ValidityTampering where

import Control.Applicative
import Cooked
import Ledger.Slot qualified as Ledger
import Optics.Core
import PlutusLedgerApi.V1.Interval qualified as Api
import Polysemy
import Polysemy.NonDet
import Test.Tasty
import Test.Tasty.HUnit

upperInfSlotRange :: Ledger.SlotRange
upperInfSlotRange = Api.from 10

lowerInfSlotRange :: Ledger.SlotRange
lowerInfSlotRange = Api.to 10

finiteSlotRange :: Ledger.SlotRange
finiteSlotRange = Api.interval 10 10

runValidityTampering ::
  ( Foldable f,
    Alternative f,
    Is k A_Traversal
  ) =>
  Ledger.SlotRange ->
  ValidityTamperingParams k is f a ->
  [Ledger.SlotRange]
runValidityTampering initialRange params =
  fmap (view txSkelValidityRangeL)
    . run
    . runNonDet
    . execTweak (txSkelTemplate {txSkelValidityRange = initialRange})
    $ validityTamperingAttack params

tests :: TestTree
tests =
  testGroup
    "Validity tampering"
    [ testGroup
        "Tampering with the lower bound"
        [ testCase "Strict tampering of inifinite lower bound" $
            []
              @=? runValidityTampering
                lowerInfSlotRange
                (lowerStrictValidityTamperingParams ((: []) . (+ 1))),
          testCase "Strict tampering of finite lower bound" $
            [Api.from 11]
              @=? runValidityTampering
                upperInfSlotRange
                (lowerStrictValidityTamperingParams ((: []) . (+ 1))),
          testCase "Extended tampering of infinite lower bound" $
            [Api.interval 10 10]
              @=? runValidityTampering
                lowerInfSlotRange
                (lowerExtendedValidityTamperingParams (const [Just 10])),
          testCase "Extended tampering of finite lower bound" $
            [Api.always]
              @=? runValidityTampering
                upperInfSlotRange
                (lowerExtendedValidityTamperingParams (const [Nothing]))
        ],
      testGroup
        "Tampering with the upper bound"
        [ testCase "Strict tampering of inifinite upper bound" $
            []
              @=? runValidityTampering
                upperInfSlotRange
                (upperStrictValidityTamperingParams ((: []) . (+ 1))),
          testCase "Strict tampering of finite upper bound" $
            [Api.to 11]
              @=? runValidityTampering
                lowerInfSlotRange
                (upperStrictValidityTamperingParams ((: []) . (+ 1))),
          testCase "Extended tampering of infinite upper bound" $
            [Api.interval 10 10]
              @=? runValidityTampering
                upperInfSlotRange
                (upperExtendedValidityTamperingParams (const [Just 10])),
          testCase "Extended tampering of finite upper bound" $
            [Api.always]
              @=? runValidityTampering
                lowerInfSlotRange
                (upperExtendedValidityTamperingParams (const [Nothing]))
        ],
      testGroup
        "Tampering with both bounds"
        [ testCase "Strict tampering with infinite lower bound" $
            [Api.to 11]
              @=? runValidityTampering
                lowerInfSlotRange
                (bothStrictValidityTamperingParams ((: []) . (+ 1))),
          testCase "Strict tampering with infinite upper bound" $
            [Api.from 11]
              @=? runValidityTampering
                upperInfSlotRange
                (bothStrictValidityTamperingParams ((: []) . (+ 1))),
          testCase "Strict tampering with both finite bounds" $
            [Api.interval 11 11]
              @=? runValidityTampering
                finiteSlotRange
                (bothStrictValidityTamperingParams ((: []) . (+ 1))),
          testCase "Strict tampering with both infinite bounds" $
            []
              @=? runValidityTampering
                Api.always
                (bothStrictValidityTamperingParams ((: []) . (+ 1))),
          testCase "Extended tampering with both bounds" $
            replicate 4 [Api.always]
              @=? (`runValidityTampering` bothExtendedValidityTamperingParams (const [Nothing]))
                <$> [finiteSlotRange, lowerInfSlotRange, upperInfSlotRange, Api.always]
        ],
      testCase "Tampering with the whole interval" $
        [lowerInfSlotRange, upperInfSlotRange]
          @=? runValidityTampering
            Api.always
            (intervalValidityTamperingParams $ const [lowerInfSlotRange, upperInfSlotRange])
    ]
