{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.AttackSpec.DatumHijackingAttack (tests) where

import Control.Monad
import Cooked.Attack
import Cooked.AttackSpec.Common
import Cooked.Ltl
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger as L hiding (singleton, validatorHash)
import qualified Ledger.Ada as L
import qualified Ledger.Contexts as L
import qualified Ledger.Typed.Scripts as L
import qualified Ledger.Value as L
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Tasty
import Test.Tasty.HUnit

-- * Tests for the datum hijacking attack

-- ** Mock contract for the datum hijacking attack

-- This is a very simple contract: The first transaction locks some Ada to the
-- validator, using the datum 'FirstLock', the second transaction then re-locks
-- the same amount to the same validator, using the datum 'SecondLock'. The
-- datum hijacking attack should target the secon transaction, and substitute a
-- different recipient.

data MockDatum = FirstLock | SecondLock deriving (Show)

instance Pl.Eq MockDatum where
  {-# INLINEABLE (==) #-}
  FirstLock == FirstLock = True
  SecondLock == SecondLock = True
  _ == _ = False

Pl.makeLift ''MockDatum
Pl.unstableMakeIsData ''MockDatum

data MockContract

instance L.ValidatorTypes MockContract where
  type DatumType MockContract = MockDatum
  type RedeemerType MockContract = ()

-- ** Transactions (and 'TxSkels') for the datum hijacking attack

lockValue :: L.Value
lockValue = L.lovelaceValueOf 12345678

lockTxSkel :: SpendableOut -> L.TypedValidator MockContract -> TxSkel
lockTxSkel o v =
  txSkelOpts
    (def {adjustUnbalTx = True})
    ([SpendsPK o] :=>: [PaysScript v FirstLock lockValue])

txLock :: MonadBlockChain m => L.TypedValidator MockContract -> m ()
txLock v = do
  me <- ownPaymentPubKeyHash
  utxo : _ <- pkUtxosSuchThatValue me (`L.geq` lockValue)
  void $ validateTxSkel $ lockTxSkel utxo v

relockTxSkel :: L.TypedValidator MockContract -> SpendableOut -> TxSkel
relockTxSkel v o =
  txSkelOpts
    (def {adjustUnbalTx = True})
    ( [SpendsScript v () (o, FirstLock)]
        :=>: [PaysScript v SecondLock lockValue]
    )

txRelock ::
  MonadBlockChain m =>
  L.TypedValidator MockContract ->
  m ()
txRelock v = do
  utxo : _ <- scriptUtxosSuchThat v (\d _ -> FirstLock Pl.== d)
  void $ validateTxSkel $ relockTxSkel v (fst utxo)

-- ** Validators for the datum hijacking attack

-- | Try to extract a datum from an output.
{-# INLINEABLE outputDatum #-}
outputDatum :: L.TxInfo -> L.TxOut -> Maybe MockDatum
outputDatum txi o = do
  h <- L.txOutDatum o
  L.Datum d <- L.findDatum h txi
  Pl.fromBuiltinData d

{-# INLINEABLE mkMockValidator #-}
mkMockValidator :: (L.ScriptContext -> [L.TxOut]) -> MockDatum -> () -> L.ScriptContext -> Bool
mkMockValidator getOutputs datum _ ctx =
  let txi = L.scriptContextTxInfo ctx
   in case datum of
        FirstLock ->
          case getOutputs ctx of
            o : _ ->
              Pl.traceIfFalse
                "not in 'SecondLock'-state after re-locking"
                (outputDatum txi o Pl.== Just SecondLock)
                && Pl.traceIfFalse
                  "not re-locking the right amout"
                  (L.txOutValue o == lockValue)
            _ -> Pl.trace "there must be a output re-locked" False
        SecondLock -> False

{-# INLINEABLE mkCarefulValidator #-}
mkCarefulValidator :: MockDatum -> () -> L.ScriptContext -> Bool
mkCarefulValidator = mkMockValidator L.getContinuingOutputs

carefulValidator :: L.TypedValidator MockContract
carefulValidator =
  L.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarefulValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = L.wrapValidator @MockDatum @()

{-# INLINEABLE mkCarelessValidator #-}
mkCarelessValidator :: MockDatum -> () -> L.ScriptContext -> Bool
mkCarelessValidator = mkMockValidator (L.txInfoOutputs . L.scriptContextTxInfo)

carelessValidator :: L.TypedValidator MockContract
carelessValidator =
  L.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarelessValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = L.wrapValidator @MockDatum @()

datumHijackingTrace :: MonadBlockChain m => L.TypedValidator MockContract -> m ()
datumHijackingTrace v = do
  txLock v
  txRelock v

tests :: TestTree
tests =
  testGroup
    "datum hijacking attack"
    [ testCase "unit test on a 'TxSkel'" $
        let val1 = carelessValidator
            val2 = carefulValidator
            thief = datumHijackingTarget @MockContract
            x1 = L.lovelaceValueOf 10001
            x2 = L.lovelaceValueOf 10000
            x3 = L.lovelaceValueOf 9999
            skelIn =
              txSkel
                [ PaysScript val1 SecondLock x1,
                  PaysScript val1 SecondLock x3,
                  PaysScript val2 SecondLock x1,
                  PaysScript val1 FirstLock x2,
                  PaysScript val1 SecondLock x2
                ]
            skelOut select =
              datumHijackingAttack @MockContract
                ( \v d x ->
                    L.validatorHash val1 == L.validatorHash v
                      && SecondLock Pl.== d
                      && x2 `L.geq` x
                )
                select
                def
                skelIn
            skelExpected a b =
              txSkelLbl
                (DatumHijackingLbl $ L.validatorHash thief)
                [ PaysScript val1 SecondLock x1,
                  PaysScript a SecondLock x3,
                  PaysScript val2 SecondLock x1,
                  PaysScript val1 FirstLock x2,
                  PaysScript b SecondLock x2
                ]
         in assertTxSkelEqual (Just $ skelExpected thief val1) (skelOut (0 ==))
              .&&. assertTxSkelEqual (Just $ skelExpected val1 thief) (skelOut (1 ==))
              .&&. assertTxSkelEqual (Just $ skelExpected thief thief) (skelOut (const True)),
      testCase "careful validator" $
        testFailsFrom'
          isCekEvaluationFailure
          def
          ( somewhere
              ( datumHijackingAttack @MockContract
                  ( \v d _ ->
                      L.validatorHash v == L.validatorHash carefulValidator
                        && SecondLock Pl.== d
                  )
                  (const True)
              )
              (datumHijackingTrace carefulValidator)
          ),
      testCase "careless validator" $
        testSucceeds
          ( somewhere
              ( datumHijackingAttack @MockContract
                  ( \v d _ ->
                      L.validatorHash v == L.validatorHash carelessValidator
                        && SecondLock Pl.== d
                  )
                  (const True)
              )
              (datumHijackingTrace carelessValidator)
          )
    ]
