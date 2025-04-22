{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Plutus.ReferenceInputs where

import Plutus.Script.Utils.V2 qualified as Script
import PlutusLedgerApi.V2 qualified as Api
import PlutusTx
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude
import Prelude qualified as HS

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

newtype FooDatum = FooDatum Api.PubKeyHash deriving (HS.Show)

instance Eq FooDatum where
  FooDatum pkh1 == FooDatum pkh2 = pkh1 == pkh2

makeLift ''FooDatum
unstableMakeIsData ''FooDatum

instance Script.ValidatorTypes Foo where
  type RedeemerType Foo = ()
  type DatumType Foo = FooDatum

-- | Outputs can only be spent by pks whose hash is not the one in the
-- datum.
{-# INLINEABLE fooValidator #-}
fooValidator :: FooDatum -> () -> Api.ScriptContext -> Bool
fooValidator (FooDatum pkh) _ (Api.ScriptContext txInfo _) =
  pkh `notElem` Api.txInfoSignatories txInfo

fooTypedValidator :: Script.TypedValidator Foo
fooTypedValidator =
  let wrap = Script.mkUntypedValidator
   in Script.mkTypedValidator @Foo
        $$(compile [||fooValidator||])
        $$(compile [||wrap||])

-- | Outputs can only be spent by pks who provide a reference input to
-- a Foo in which they are mentioned (in an inlined datum).
{-# INLINEABLE barValidator #-}
barValidator :: () -> () -> Api.ScriptContext -> Bool
barValidator _ _ (Api.ScriptContext txInfo _) =
  any f (Api.txInfoReferenceInputs txInfo)
  where
    f :: Api.TxInInfo -> Bool
    f (Api.TxInInfo _ (Api.TxOut _ _ (Api.OutputDatum (Api.Datum datum)) _)) =
      case Api.fromBuiltinData @FooDatum datum of
        Nothing -> False
        Just (FooDatum pkh) -> pkh `elem` Api.txInfoSignatories txInfo
    f _ = False

barTypedValidator :: Script.TypedValidator ()
barTypedValidator =
  let wrap = Script.mkUntypedValidator
   in Script.mkTypedValidator
        $$(compile [||barValidator||])
        $$(compile [||wrap||])

{-# INLINEABLE bazValidator #-}
bazValidator :: () -> () -> Api.ScriptContext -> Bool
bazValidator _ _ context =
  let info = Api.scriptContextTxInfo context
      refInputs = Api.txInfoReferenceInputs info
      txData = Api.txInfoData info
   in case refInputs of
        [myRefInput] ->
          let Api.TxOut _ _ dat _ = Api.txInInfoResolved myRefInput
           in case dat of
                (Api.OutputDatumHash hash) -> case Map.lookup hash txData of
                  Nothing -> False
                  Just (Api.Datum a) -> unsafeFromBuiltinData @Integer a == 10
                _ -> False
        _ -> False

bazTypedValidator :: Script.TypedValidator ()
bazTypedValidator =
  let wrap = Script.mkUntypedValidator
   in Script.mkTypedValidator
        $$(compile [||bazValidator||])
        $$(compile [||wrap||])
