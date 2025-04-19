module Cooked.MinAdaSpec where

import Cooked
import Optics.Core ((^.))
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
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

paymentWithMinAda :: (MonadBlockChain m) => m Integer
paymentWithMinAda = do
  Api.getLovelace . (^. Script.adaL) . outputValue . snd . (!! 0) . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOuts = [wallet 2 `receives` VisibleHashedDatum heavyDatum],
          txSkelSigners = [wallet 1]
        }

paymentWithoutMinAda :: (MonadBlockChain m) => Integer -> m ()
paymentWithoutMinAda paidLovelaces = do
  validateTxSkel_
    txSkelTemplate
      { txSkelOuts = [wallet 2 `receives` (FixedValue (Script.lovelace paidLovelaces) <&&> VisibleHashedDatum heavyDatum)],
        txSkelSigners = [wallet 1]
      }

tests :: TestTree
tests =
  testGroup
    "MinAda auto adjustment of transaction outputs"
    [ testCooked "adjusted transaction passes" $ mustSucceedTest paymentWithMinAda,
      testCooked "adjusted transaction contains minimal amount" $ mustFailInPhase1Test $ paymentWithMinAda >>= paymentWithoutMinAda . (+ (-1))
    ]
