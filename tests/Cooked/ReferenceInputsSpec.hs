{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Cooked.ReferenceInputsSpec where

import Control.Monad
import Cooked
import Data.Map qualified as Map
import Data.Set qualified as Set
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V2.Typed.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V2 qualified as Api
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PlutusTx (lookup)
import PlutusTx.Eq qualified as PlutusTx
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter qualified as PP
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

-- Foo, Bar and Baz are dummy scripts to test reference inputs. They serve no
-- purpose and make no real sense.
--
-- Foo contains a pkh in its datum. It can only be spent by ANOTHER public key.
--
-- Bar has no datum nor redeemer. Its outputs can only be spent by a public key
-- who can provide a Foo UTxO containing its pkh as reference input (that is a
-- UTxO they could not actually spend, according the the design of Foo).
--
-- Baz has no datum nor redeemer. Its outputs can only be spent when a reference
-- input is provided with a hashed datum contain the integer 10.
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

barTypedValidator :: Script.TypedValidator MockContract
barTypedValidator =
  let wrap = Script.mkUntypedValidator
   in Script.mkTypedValidator @MockContract
        $$(PlutusTx.compile [||barValidator||])
        $$(PlutusTx.compile [||wrap||])

bazValidator :: () -> () -> Api.ScriptContext -> Bool
bazValidator _ _ context =
  let info = Api.scriptContextTxInfo context
      refInputs = Api.txInfoReferenceInputs info
      txData = Api.txInfoData info
   in case refInputs of
        [myRefInput] ->
          let Api.TxOut _ _ dat _ = Api.txInInfoResolved myRefInput
           in case dat of
                (Api.OutputDatumHash hash) -> case PlutusTx.lookup hash txData of
                  Nothing -> False
                  Just (Api.Datum a) -> PlutusTx.unsafeFromBuiltinData @Integer a PlutusTx.== 10
                _ -> False
        _ -> False

bazTypedValidator :: Script.TypedValidator MockContract
bazTypedValidator =
  let wrap = Script.mkUntypedValidator
   in Script.mkTypedValidator @MockContract
        $$(PlutusTx.compile [||bazValidator||])
        $$(PlutusTx.compile [||wrap||])

trace1 :: (MonadBlockChain m) => m ()
trace1 = do
  txOutRefFoo : txOutRefBar : _ <-
    validateTxSkel'
      txSkelTemplate
        { txSkelOuts =
            [ fooTypedValidator `receives` (Value (Script.ada 4) <&&> InlineDatum (FooDatum $ walletPKHash $ wallet 3)),
              barTypedValidator `receives` (Value (Script.ada 5) <&&> VisibleHashedDatum ())
            ],
          txSkelSigners = [wallet 2]
        }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton txOutRefBar $ someTxSkelRedeemer (),
          txSkelInsReference = Set.singleton txOutRefFoo,
          txSkelOuts = [wallet 4 `receives` Value (Script.ada 5)],
          txSkelSigners = [wallet 3]
        }

trace2 :: (MonadBlockChain m) => m ()
trace2 = do
  refORef : scriptORef : _ <-
    validateTxSkel'
      ( txSkelTemplate
          { txSkelOuts =
              [ wallet 1 `receives` (Value (Script.ada 2) <&&> VisibleHashedDatum (10 :: Integer)),
                bazTypedValidator `receives` (Value (Script.ada 10) <&&> VisibleHashedDatum ())
              ],
            txSkelSigners = [wallet 2]
          }
      )
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelSigners = [wallet 1],
          txSkelIns = Map.singleton scriptORef (someTxSkelRedeemer ()),
          txSkelInsReference = Set.singleton refORef
        }

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Reference inputs"
    [ Tasty.testCase "We can reference an input that can't be spent" $ testSucceeds trace1,
      Tasty.testCase "We can decode the datum hash from a reference input" $ testSucceeds trace2
    ]
