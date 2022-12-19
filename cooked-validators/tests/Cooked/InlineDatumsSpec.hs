{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.InlineDatumsSpec where

import Cooked
import Cooked.Tx.Constraints.Type
import Data.Default
import qualified Ledger.Ada as Pl
import qualified Plutus.Script.Utils.V2.Scripts as Pl
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx as Pl
import Test.Tasty

data SimpleContract

instance Pl.ValidatorTypes SimpleContract where
  type RedeemerType SimpleContract = ()
  type DatumType SimpleContract = Bool

trivialValidator :: Pl.TypedValidator SimpleContract
trivialValidator = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||tgt||]))
  where
    tgt :: Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()
    tgt _ _ _ = ()

testTrace ::
  (MonadBlockChain m, PaysScriptConstrs a, Pl.FromData (Pl.DatumType a)) =>
  Pl.TypedValidator a ->
  Pl.DatumType a ->
  Bool ->
  m [(SpendableOut, Pl.DatumType a)]
testTrace validator datum useInlineDatum = do
  _ <-
    validateTxSkel
      mempty
        { txSkelOpts = def {adjustUnbalTx = True},
          txSkelOuts =
            [ paysScript
                validator
                ( if useInlineDatum
                    then Right datum
                    else Left . Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum
                )
                (Pl.lovelaceValueOf 3_000_000)
            ]
        }
  scriptUtxosSuchThat validator (\_ _ -> True)

tests :: [TestTree]
tests = []
