module Cooked.ReferenceInputsSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Data.Set qualified as Set
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter qualified as PP
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

-- Foo and Bar are two dummy scripts to test reference inputs. They
-- serve no purpose and make no real sense.
--
-- Foo contains a pkh in its datum. It can only be spent by ANOTHER
-- public key.
--
-- Bar has no datum nor redeemer. Its outputs can only be spent by a
-- public key who can provide a Foo UTxO containing its pkh as
-- reference input (that is a UTxO they could not actually spend,
-- according the the design of Foo).
--
-- The datum in Foo outputs in expected to be inlined.

data Foo

newtype FooDatum = FooDatum Api.PubKeyHash deriving (Show)

instance PrettyCooked FooDatum where
  prettyCookedOpt opts (FooDatum pkh) = "FooDatum" PP.<+> prettyCookedOpt opts pkh

instance PlutusTx.Eq FooDatum where
  FooDatum pkh1 == FooDatum pkh2 = pkh1 == pkh2

PlutusTx.makeLift ''FooDatum
PlutusTx.unstableMakeIsData ''FooDatum

instance Script.ValidatorTypes Foo where
  type RedeemerType Foo = ()
  type DatumType Foo = FooDatum

-- | Outputs can only be spent by pks whose hash is not the one in the
-- datum.
fooValidator :: FooDatum -> () -> Api.ScriptContext -> Bool
fooValidator (FooDatum pkh) _ (Api.ScriptContext txInfo _) =
  PlutusTx.not PlutusTx.$ PlutusTx.elem pkh (Api.txInfoSignatories txInfo)

fooTypedValidator :: Script.TypedValidator Foo
fooTypedValidator =
  let wrap = Script.mkUntypedValidator
   in Script.mkTypedValidator @Foo
        $$(PlutusTx.compile [||fooValidator||])
        $$(PlutusTx.compile [||wrap||])

data Bar

instance Script.ValidatorTypes Bar where
  type RedeemerType Bar = ()
  type DatumType Bar = ()

-- | Outputs can only be spent by pks who provide a reference input to
-- a Foo in which they are mentioned (in an inlined datum).
barValidator :: () -> () -> Api.ScriptContext -> Bool
barValidator _ _ (Api.ScriptContext txInfo _) =
  (PlutusTx.not . PlutusTx.null) (PlutusTx.filter f (Api.txInfoReferenceInputs txInfo))
  where
    f :: Api.TxInInfo -> Bool
    f (Api.TxInInfo _ (Api.TxOut _ _ (Api.OutputDatum (Api.Datum datum)) _)) =
      case Api.fromBuiltinData @FooDatum datum of
        Nothing -> False
        Just (FooDatum pkh) -> PlutusTx.elem pkh (Api.txInfoSignatories txInfo)
    f _ = False

barTypedValidator :: Script.TypedValidator Bar
barTypedValidator =
  let wrap = Script.mkUntypedValidator
   in Script.mkTypedValidator @Bar
        $$(PlutusTx.compile [||barValidator||])
        $$(PlutusTx.compile [||wrap||])

trace1 :: (MonadBlockChain m) => m ()
trace1 = do
  txOutRefFoo : txOutRefBar : _ <-
    validateTxSkel'
      txSkelTemplate
        { txSkelOuts =
            [ paysScriptInlineDatum fooTypedValidator (FooDatum (walletPKHash (wallet 3))) (ada 4),
              paysScript barTypedValidator () (ada 5)
            ],
          txSkelSigners = [wallet 2]
        }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton txOutRefBar $ TxSkelRedeemerForScript (),
          txSkelInsReference = Set.singleton txOutRefFoo,
          txSkelOuts = [paysPK (wallet 4) (ada 5)],
          txSkelSigners = [wallet 3]
        }

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Reference inputs"
    [Tasty.testCase "Can reference an input that can't be spent" (testSucceeds def trace1)]
