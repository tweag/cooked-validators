{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.AttackSpec.DatumHijacking (tests) where

import Control.Monad
import Cooked.Attack.Common
import Cooked.Attack.DatumHijacking
import Cooked.AttackSpec.Util
import Cooked.Ltl
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger as L hiding (validatorHash)
import qualified Ledger.Ada as L
import qualified Ledger.Typed.Scripts as L
import qualified Ledger.Value as L
import qualified Plutus.V1.Ledger.Scripts as L
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Tasty
import Test.Tasty.HUnit

-- * Mock contract for the datum hijacking attack

-- This is a very simple contract: The first transaction locks some Ada to the
-- validator, using the datum 'FirstLock', the second transaction then re-locks
-- the same amount to the same validator, using the datum 'SecondLock'. The
-- datum hijacking attack should target the second transaction, and substitute a
-- different recipient.

data MockDatum = FirstLock | SecondLock deriving (Show, Eq)

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
    ([SpendsPK o] :=>: [paysScript v FirstLock lockValue])

txLock :: MonadBlockChain m => L.TypedValidator MockContract -> m ()
txLock v = do
  me <- ownPaymentPubKeyHash
  utxo : _ <- pkUtxosSuchThatValue me (`L.geq` lockValue)
  void $ validateTxSkel $ lockTxSkel utxo v

relockTxSkel :: L.TypedValidator MockContract -> SpendableOut -> TxSkel
relockTxSkel v o =
  txSkelOpts
    (def {adjustUnbalTx = True})
    ( [SpendsScript v () o]
        :=>: [paysScript v SecondLock lockValue]
    )

txRelock ::
  MonadBlockChain m =>
  L.TypedValidator MockContract ->
  m ()
txRelock v = do
  utxo : _ <- scriptUtxosSuchThat v (\d _ -> FirstLock Pl.== d)
  void $ validateTxSkel $ relockTxSkel v (fst utxo)

-- * Validators for the datum hijacking attack

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
    wrap = L.mkUntypedValidator @MockDatum @()

{-# INLINEABLE mkCarelessValidator #-}
mkCarelessValidator :: MockDatum -> () -> L.ScriptContext -> Bool
mkCarelessValidator = mkMockValidator (L.txInfoOutputs . L.scriptContextTxInfo)

carelessValidator :: L.TypedValidator MockContract
carelessValidator =
  L.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarelessValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = L.mkUntypedValidator @MockDatum @()

datumHijackingTrace :: MonadBlockChain m => L.TypedValidator MockContract -> m ()
datumHijackingTrace v = do
  txLock v
  txRelock v

-- * TestTree for the datum hijacking attack

tests :: TestTree
tests =
  testGroup
    "datum hijacking attack"
    [ testGroup "unit tests on a 'TxSkel'" $
        let val1 = carelessValidator
            val2 = carefulValidator
            thief = datumHijackingTarget @MockContract
            x1 = L.lovelaceValueOf 10001
            x2 = L.lovelaceValueOf 10000
            x3 = L.lovelaceValueOf 9999
            skelIn =
              txSkel
                [ paysScript val1 SecondLock x1,
                  paysScript val1 SecondLock x3,
                  paysScript val2 SecondLock x1,
                  paysScript val1 FirstLock x2,
                  paysScript val1 SecondLock x2
                ]
            skelOut bound select =
              getAttack
                ( datumHijackingAttack @MockContract
                    ( \v d x ->
                        L.validatorHash val1 == L.validatorHash v
                          && SecondLock Pl.== d
                          && bound `L.geq` x
                    )
                    select
                )
                def
                skelIn
            skelExpected a b =
              txSkelLbl
                (DatumHijackingLbl $ L.validatorAddress thief)
                [ paysScript val1 SecondLock x1,
                  paysScript a SecondLock x3,
                  paysScript val2 SecondLock x1,
                  paysScript val1 FirstLock x2,
                  paysScript b SecondLock x2
                ]
         in [ testCase "no modified transactions if no interesting outputs to steal" $ [] @=? skelOut mempty (const True),
              testCase "one modified transaction for one interesting output" $
                [ ( skelExpected thief val1,
                    [(val1, Nothing, SecondLock, x3)]
                  )
                ]
                  @=? skelOut x2 (0 ==),
              testCase "two modified transactions for two interesting outputs" $
                [ ( skelExpected thief thief,
                    [ (val1, Nothing, SecondLock, x3),
                      (val1, Nothing, SecondLock, x2)
                    ]
                  )
                ]
                  @=? skelOut x2 (const True),
              testCase "select second interesting output to get one modified transaction" $
                [ ( skelExpected val1 thief,
                    [(val1, Nothing, SecondLock, x2)]
                  )
                ]
                  @=? skelOut x2 (1 ==)
            ],
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
