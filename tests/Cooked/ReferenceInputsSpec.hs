{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.ReferenceInputsSpec where

import Control.Monad
import Cooked
import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Prettyprinter (Pretty)
import qualified Prettyprinter as PP
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

-- Foo and Bar are two dummy scripts to test reference inputs. They serve no
-- purpose and make no real sense.
--
-- Foo contains a pkh in its datum. It can only be spent by ANOTHER public key.
--
-- Bar has no datum nor redeemer. Its outputs can only be spent by a public key
-- who can provide a Foo UTxO containing its pkh as reference input (that is a
-- UTxO they could not actually spend, according the the design of Foo).
--
-- The datum in Foo outputs in expected to be inlined.

data Foo

newtype FooDatum = FooDatum Pl.PubKeyHash deriving (Show)

instance PrettyCooked FooDatum where
  prettyCookedOpt opts (FooDatum pkh) = "FooDatum" PP.<+> prettyCookedOpt opts pkh

instance Pl.Eq FooDatum where
  FooDatum pkh1 == FooDatum pkh2 = pkh1 == pkh2

PlutusTx.makeLift ''FooDatum
PlutusTx.unstableMakeIsData ''FooDatum

instance Pl.ValidatorTypes Foo where
  type RedeemerType Foo = ()
  type DatumType Foo = FooDatum

-- | Outputs can only be spent by pks whose hash is not the one in the datum.
fooValidator :: FooDatum -> () -> Pl.ScriptContext -> Bool
fooValidator (FooDatum pkh) _ (Pl.ScriptContext txInfo _) =
  Pl.not Pl.$ Pl.elem pkh (Pl.txInfoSignatories txInfo)

fooTypedValidator :: Pl.TypedValidator Foo
fooTypedValidator =
  let wrap = Pl.mkUntypedValidator
   in Pl.mkTypedValidator @Foo
        $$(Pl.compile [||fooValidator||])
        $$(Pl.compile [||wrap||])

data Bar

instance Pl.ValidatorTypes Bar where
  type RedeemerType Bar = ()
  type DatumType Bar = ()

-- | Outputs can only be spent by pks who provide a reference input to a Foo in
-- which they are mentioned (in an inlined datum).
barValidator :: () -> () -> Pl.ScriptContext -> Bool
barValidator _ _ (Pl.ScriptContext txInfo _) =
  (Pl.not . Pl.null) (Pl.filter f (Pl.txInfoReferenceInputs txInfo))
  where
    f :: Pl.TxInInfo -> Bool
    f
      ( Pl.TxInInfo
          _
          (Pl.TxOut address _ (Pl.OutputDatum (Pl.Datum datum)) _)
        ) =
        case Pl.fromBuiltinData @FooDatum datum of
          Nothing -> False
          Just (FooDatum pkh) -> Pl.elem pkh (Pl.txInfoSignatories txInfo)
    f _ = False

barTypedValidator :: Pl.TypedValidator Bar
barTypedValidator =
  let wrap = Pl.mkUntypedValidator
   in Pl.mkTypedValidator @Bar
        $$(Pl.compile [||barValidator||])
        $$(Pl.compile [||wrap||])

trace1 :: MonadBlockChain m => m ()
trace1 = do
  (txOutRefFoo, _) : (txOutRefBar, _) : _ <-
    utxosFromCardanoTx
      <$> validateTxSkel
        txSkelTemplate
          { txSkelOuts =
              [ paysScriptInlineDatum
                  fooTypedValidator
                  (FooDatum (walletPKHash (wallet 3)))
                  (Pl.lovelaceValueOf 4_000_000),
                paysScript
                  barTypedValidator
                  ()
                  (Pl.lovelaceValueOf 5_000_000)
              ],
            txSkelSigners = [wallet 2]
          }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton txOutRefBar $ TxSkelRedeemerForScript (),
          txSkelInsReference = Set.singleton txOutRefFoo,
          txSkelOuts =
            [ paysPK
                (walletPKHash (wallet 4))
                (Pl.lovelaceValueOf 5_000_000)
            ],
          txSkelSigners = [wallet 3]
        }

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Reference inputs"
    [Tasty.testCase "Can reference an input that can't be spent" (testSucceeds def trace1)]
