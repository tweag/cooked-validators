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
  testCase "tamperSpendingRedeemersOfTypeTweak only touches redeemers of the right type and records them in its label" $
    [(Set.singleton (TxSkelLabel (TamperedRedeemerLbl [10, 20 :: Integer])), [11, 21])]
      @=? ( fmap (\(skel, _) -> (view txSkelLabelsL skel, integerRedeemers skel)) . run . runNonDet $
              runTweak
                baseSkel
                (tamperSpendingRedeemersOfTypeTweak @Integer (\n -> Just (n + 1)))
          )

tamperAllRedeemersTest :: TestTree
tamperAllRedeemersTest =
  testCase "tamperAllRedeemersOfTypeTweak reaches the spending redeemers" $
    [[0, 0]]
      @=? ( fmap (integerRedeemers . fst) . run . runNonDet $
              runTweak baseSkel (tamperAllRedeemersOfTypeTweak @Integer @Integer (const $ Just 0))
          )

-- | A change returning several options branches the tweak into every
-- combination of per-redeemer choices.
tamperBranchingTest :: TestTree
tamperBranchingTest =
  testCase "tamperRedeemersOfTypeTweak branches on every combination of redeemer modifications" $
    assertSameSets
      [ [11, 21],
        [11, 22],
        [12, 21],
        [12, 22]
      ]
      ( fmap (integerRedeemers . fst) . run . runNonDet $
          runTweak
            baseSkel
            (tamperRedeemersOfTypeTweak @Integer txSkelSpendingRedeemersT (\n -> [n + 1, n + 2]))
      )

-- | Transforming @Integer@ redeemers into raw 'Api.BuiltinData' (of a possibly
-- unrelated type) still works: 'Api.BuiltinData' satisfies 'RedeemerConstrs',
-- and setting through 'txSkelRedeemerTypedAT' re-encodes with the identity, so
-- genuinely malformed redeemers are reachable. Offering two data options per
-- redeemer (its actual representation and a constant one) branches into all
-- combinations across the two spending redeemers.
tamperToBuiltinDataTest :: TestTree
tamperToBuiltinDataTest =
  testCase "tamperRedeemersOfTypeTweak can malform redeemers into arbitrary BuiltinData, trying all combinations" $
    let allData :: TxSkel -> [PlutusTx.BuiltinData]
        allData = toListOf (txSkelInputsL % to Map.elems % folded % txSkelRedeemerBuiltinDataL)
        d :: (PlutusTx.ToData a) => a -> PlutusTx.BuiltinData
        d = PlutusTx.toBuiltinData
     in assertSameSets
          [ [d (10 :: Integer), d (20 :: Integer), d True], -- both redeemers kept as-is
            [d False, d (20 :: Integer), d True], -- only the first integer redeemer changed
            [d (10 :: Integer), d False, d True], -- only the second integer redeemer changed
            [d False, d False, d True] -- both integer redeemers changed
          ]
          ( fmap (allData . fst) . run . runNonDet $
              runTweak
                baseSkel
                (tamperRedeemersOfTypeTweak @Integer @Api.BuiltinData txSkelSpendingRedeemersT (\n -> [d n, d False]))
          )

-- | Regression test for the certificate-redeemer kind bug: certificate owners
-- are stored with an 'IsEither' kind, which previously made their redeemers
-- invisible to 'tamperAllRedeemersOfTypeTweak'.
tamperCertificateRedeemersTest :: TestTree
tamperCertificateRedeemersTest =
  testCase "tamperAllRedeemersOfTypeTweak reaches the certifying redeemers" $
    [[0]]
      @=? ( fmap (certificateIntegerRedeemers . fst) . run . runNonDet $
              runTweak
                (certificateSkel $ someTxSkelRedeemer (10 :: Integer))
                (tamperAllRedeemersOfTypeTweak @Integer @Integer (const $ Just 0))
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
