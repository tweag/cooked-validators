-- | Tests for 'Cooked.Tweak.Redeemers'.
module Spec.Tweak.Redeemers where

import Cooked
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics.Core
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
    { txSkelIns =
        Map.fromList
          [ (oref 0, someTxSkelRedeemer (10 :: Integer)),
            (oref 1, someTxSkelRedeemer (20 :: Integer)),
            (oref 2, someTxSkelRedeemer True)
          ]
    }

-- | The list of 'Integer'-typed spending redeemers of a skeleton.
integerRedeemers :: TxSkel -> [Integer]
integerRedeemers = toListOf (txSkelInsL % to Map.elems % folded % txSkelRedeemerTypedAT @Integer)

modifySpendRedeemersTest :: TestTree
modifySpendRedeemersTest =
  testCase "modifySpendRedeemersOfTypeTweak only touches redeemers of the right type" $
    [11, 21]
      @=? ( integerRedeemers . fst . run $
              runTweak
                baseSkel
                (modifySpendRedeemersOfTypeTweak @Integer @Integer (\n -> Just (n + 1)))
          )

modifyAllRedeemersTest :: TestTree
modifyAllRedeemersTest =
  testCase "modifyRedeemersOfTypeTweak reaches the spending redeemers" $
    [0, 0]
      @=? ( integerRedeemers . fst . run $
              runTweak baseSkel (modifyRedeemersOfTypeTweak @Integer @Integer (const $ Just 0))
          )

tamperRedeemerTest :: TestTree
tamperRedeemerTest =
  testCase "tamperRedeemerTweak modifies redeemers and adds its label" $
    [(Set.singleton (TxSkelLabel TamperRedeemerLbl), [11, 21])]
      @=? ( fmap (\(skel, _) -> (view txSkelLabelsL skel, integerRedeemers skel)) . run . runNonDet $
              runTweak
                baseSkel
                (tamperRedeemerTweak @Integer (\n -> Just (n + 1)))
          )

malformRedeemerTest :: TestTree
malformRedeemerTest =
  testCase "malformRedeemerTweak tries all combinations of redeemer modifications" $
    let allData :: TxSkel -> [PlutusTx.BuiltinData]
        allData = toListOf (txSkelInsL % to Map.elems % folded % txSkelRedeemerBuiltinDataL)
        d :: (PlutusTx.ToData a) => a -> PlutusTx.BuiltinData
        d = PlutusTx.toBuiltinData
     in assertSameSets
          [ [d False, d (20 :: Integer), d True], -- only the first integer redeemer changed
            [d (10 :: Integer), d False, d True], -- only the second integer redeemer changed
            [d False, d False, d True] -- both integer redeemers changed
          ]
          ( (fmap allData . run . runNonDet)
              ( execTweak
                  baseSkel
                  (malformRedeemerTweak @Integer (const [PlutusTx.toBuiltinData False]))
              )
          )

tests :: TestTree
tests =
  testGroup
    "Redeemer tweaks"
    [ modifySpendRedeemersTest,
      modifyAllRedeemersTest,
      tamperRedeemerTest,
      malformRedeemerTest
    ]
