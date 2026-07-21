-- | Tests for 'Cooked.Tweak.Redeemers'.
module Spec.Tweak.Redeemers where

import Cooked
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import Polysemy
import Polysemy.NonDet
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

-- | A fabricated 'Api.TxOutRef' for building spending inputs in pure tweak
-- tests (no mockchain needed).
oref :: Integer -> Api.TxOutRef
oref = Api.TxOutRef (Api.TxId "")

-- | A skeleton spending three inputs: two with 'Integer' redeemers and one with
-- a redeemer of a different type, which redeemer tweaks targeting 'Integer'
-- must leave untouched.
baseSkel :: TxSkel
baseSkel =
  txSkelTemplate
    { txSkelInputs =
        Map.fromList
          [ (oref 0, someTxSkelRedeemer (10 :: Integer)),
            (oref 1, someTxSkelRedeemer (20 :: Integer)),
            (oref 2, someTxSkelRedeemer True)
          ]
    }

-- | The list of 'Integer'-typed spending redeemers of a skeleton.
integerRedeemers :: TxSkel -> [Integer]
integerRedeemers = toListOf (txSkelSpendingRedeemersT % txSkelRedeemerTypedAT)

-- | A skeleton registering a single script certificate whose redeemer is the
-- given one. Certificate owners are stored with an 'IsEither' kind, which used
-- to make their redeemers invisible to the redeemer traversals.
certificateSkel :: TxSkelRedeemer -> TxSkel
certificateSkel red =
  txSkelTemplate
    { txSkelCertificates =
        [TxSkelCertificate (UserRedeemedScript (toVScript $ Script.trueMPScript @()) red) StakingRegister]
    }

-- | The list of 'Integer'-typed certifying redeemers of a skeleton.
certificateIntegerRedeemers :: TxSkel -> [Integer]
certificateIntegerRedeemers =
  toListOf (txSkelCertificatesL % traversed % txSkelCertificateOwnerAT @IsEither % userRedeemerAT % txSkelRedeemerTypedAT @Integer)

tamperSpendingRedeemersTest :: TestTree
tamperSpendingRedeemersTest =
  testCase "tamperSpendingRedeemersTweak only touches redeemers of the right type and records them in its label" $
    [(Set.singleton (TxSkelLabel (TamperedRedeemerLabel [10, 20 :: Integer])), [11, 21])]
      @=? ( fmap (\(skel, _) -> (view txSkelLabelsL skel, integerRedeemers skel)) . run . runNonDet $
              runTweak
                baseSkel
                (tamperSpendingRedeemersTweak @Integer OneBranchForAllFoci (Just . (+ 1)))
          )

tamperAllRedeemersTest :: TestTree
tamperAllRedeemersTest =
  testCase "tamperAllRedeemersTweak reaches the spending redeemers" $
    [[0, 0]]
      @=? ( fmap (integerRedeemers . fst) . run . runNonDet $
              runTweak baseSkel (tamperAllRedeemersTweak @Integer @Integer OneBranchForAllFoci (const $ Just 0))
          )

-- | A change returning several options branches the tweak into every
-- combination of per-redeemer choices.
tamperBranchingTest :: TestTree
tamperBranchingTest =
  testCase "tamperRedeemersTweak branches on every combination of redeemer modifications" $
    assertSameSets
      [ [11, 21],
        [11, 22],
        [12, 21],
        [12, 22]
      ]
      ( fmap (integerRedeemers . fst) . run . runNonDet $
          runTweak
            baseSkel
            (tamperSpendingRedeemersTweak @Integer OneBranchForAllFoci (\n -> [n + 1, n + 2]))
      )

-- | Transforming @Integer@ redeemers into raw 'Api.BuiltinData' (of a possibly
-- unrelated type) still works: 'Api.BuiltinData' satisfies 'RedeemerConstrs',
-- and setting through 'txSkelRedeemerTypedAT' re-encodes with the identity, so
-- genuinely malformed redeemers are reachable. Offering two data options per
-- redeemer (its actual representation and a constant one) and branching with
-- 'PowerSet' modifies every non-empty subset of the two integer redeemers,
-- branching over each option within a subset. The groupings @[0]@, @[1]@ and
-- @[0, 1]@ produce 2 + 2 + 4 = 8 skeletons, with repeats: keeping both
-- redeemers occurs in all three groupings, and each single change occurs both
-- in its singleton grouping and in the @[0, 1]@ grouping.
tamperToBuiltinDataTest :: TestTree
tamperToBuiltinDataTest =
  testCase "tamperRedeemersTweak can malform redeemers into arbitrary BuiltinData, trying every subset" $
    let allData :: TxSkel -> [PlutusTx.BuiltinData]
        allData = toListOf (txSkelInputsL % to Map.elems % folded % txSkelRedeemerBuiltinDataL)
        dI :: Integer -> PlutusTx.BuiltinData
        dI = PlutusTx.toBuiltinData
        dB :: Bool -> PlutusTx.BuiltinData
        dB = PlutusTx.toBuiltinData
     in assertSameSets
          [ -- grouping [10]
            [dI (10 + 1), dI 20, dB True],
            [dB False, dI 20, dB True],
            -- grouping [20]
            [dI 10, dI (20 + 1), dB True],
            [dI 10, dB False, dB True],
            -- grouping [10,20]
            [dI (10 + 1), dI (20 + 1), dB True],
            [dB False, dI (20 + 1), dB True],
            [dI (10 + 1), dB False, dB True],
            [dB False, dB False, dB True]
          ]
          ( fmap (allData . fst) . run . runNonDet $
              runTweak
                baseSkel
                ( tamperRedeemersTweak @Integer @Api.BuiltinData
                    OneBranchPerSubset
                    txSkelSpendingRedeemersT
                    (\n -> [dI (n + 1), dB False])
                )
          )

-- | Regression test for the certificate-redeemer kind bug: certificate owners
-- are stored with an 'IsEither' kind, which previously made their redeemers
-- invisible to 'tamperAllRedeemersTweak'.
tamperCertificateRedeemersTest :: TestTree
tamperCertificateRedeemersTest =
  testCase "tamperAllRedeemersTweak reaches the certifying redeemers" $
    [[0]]
      @=? ( fmap (certificateIntegerRedeemers . fst) . run . runNonDet $
              runTweak
                (certificateSkel $ someTxSkelRedeemer (10 :: Integer))
                (tamperAllRedeemersTweak @Integer @Integer OneBranchForAllFoci (const $ Just 0))
          )

-- | Regression test for the certificate-redeemer kind bug at the
-- 'txSkelRedeemersT' level: reference inputs carried by a certificate redeemer
-- were previously not collected by 'txSkelReferenceInputsInRedeemers'.
certificateReferenceInputsTest :: TestTree
certificateReferenceInputsTest =
  testCase "txSkelReferenceInputsInRedeemers collects certifying redeemer reference inputs" $
    Set.singleton (oref 7)
      @=? txSkelReferenceInputsInRedeemers
        (certificateSkel $ someTxSkelRedeemer (10 :: Integer) `withReferenceInput` oref 7)

tests :: TestTree
tests =
  testGroup
    "Redeemer tweaks"
    [ tamperSpendingRedeemersTest,
      tamperAllRedeemersTest,
      tamperBranchingTest,
      tamperToBuiltinDataTest,
      tamperCertificateRedeemersTest,
      certificateReferenceInputsTest
    ]
