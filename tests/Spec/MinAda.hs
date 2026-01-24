module Spec.MinAda where

import Cooked
import Optics.Core
import Plutus.Script.Utils.Value qualified as Script
import PlutusTx qualified
import PlutusTx.Eq qualified as PlutusTx
import Test.Tasty

newtype HeavyDatum = HeavyDatum [Integer]
  deriving (Show, Eq)

PlutusTx.unstableMakeIsData ''HeavyDatum

instance PlutusTx.Eq HeavyDatum where
  (==) = (==)

heavyDatum :: HeavyDatum
heavyDatum = HeavyDatum (take 100 [0 ..])

instance PrettyCooked HeavyDatum where
  prettyCookedOpt opts (HeavyDatum ints) = prettyItemizeNoTitle opts "-" ints

paymentWithMinAda :: DirectMockChain Integer
paymentWithMinAda = do
  tx <-
    validateTxSkel
      txSkelTemplate
        { txSkelOuts = [wallet 2 `receives` VisibleHashedDatum heavyDatum],
          txSkelSignatories = txSkelSignatoriesFromList [wallet 1]
        }
  view (txSkelOutValueL % valueLovelaceL % lovelaceIntegerI) . snd . (!! 0) <$> utxosFromCardanoTx tx

paymentWithoutMinAda :: Integer -> DirectMockChain ()
paymentWithoutMinAda paidLovelaces = do
  validateTxSkel_
    txSkelTemplate
      { txSkelOuts = [wallet 2 `receives` FixedValue (Script.lovelace paidLovelaces) <&&> VisibleHashedDatum heavyDatum],
        txSkelSignatories = txSkelSignatoriesFromList [wallet 1]
      }

tests :: TestTree
tests =
  testGroup
    "MinAda auto adjustment of transaction outputs"
    [ testCooked "adjusted transaction passes" $ mustSucceedTest paymentWithMinAda,
      testCooked "adjusted transaction contains minimal amount" $ mustFailInPhase1Test $ paymentWithMinAda >>= paymentWithoutMinAda . (+ (-1))
    ]
