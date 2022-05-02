{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.AttackSpec (tests) where

import Control.Monad
import Cooked.Attack
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger as L
import qualified Ledger.Ada as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Tasty
import Test.Tasty.HUnit
import Type.Reflection

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
lockValue = L.lovelaceValueOf 123456789

lockTxSkel :: L.TypedValidator MockContract -> TxSkel
lockTxSkel v =
  txSkelOpts
    (def {adjustUnbalTx = True})
    (PaysScript v FirstLock lockValue)

txLock :: MonadBlockChain m => L.TypedValidator MockContract -> m ()
txLock = void . validateTxSkel . lockTxSkel

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
  [utxo] <- scriptUtxosSuchThat v (\_ _ -> True) -- (\d _ -> FirstLock Pl.== d)
  void $ validateTxSkel $ relockTxSkel v (fst utxo)

-- ** Validators for the datum hijacking attack

-- | Try to extract a datum from an output.
{-# INLINEABLE outputDatum #-}
outputDatum :: L.TxInfo -> L.TxOut -> Maybe MockDatum
outputDatum txi o = do
  h <- L.txOutDatum o
  L.Datum d <- L.findDatum h txi
  Pl.fromBuiltinData d

{-# INLINEABLE mkCarefulValidator #-}
mkCarefulValidator :: MockDatum -> () -> L.ScriptContext -> Bool
mkCarefulValidator datum _ ctx =
  let txi = L.scriptContextTxInfo ctx
   in case datum of
        FirstLock ->
          L.valueLockedBy txi (L.ownHash ctx) == lockValue
            && case L.getContinuingOutputs ctx of
              [o] -> outputDatum txi o Pl.== Just SecondLock
        SecondLock -> Pl.trace "there must be exactly one output" False

carefulValidator :: L.TypedValidator MockContract
carefulValidator =
  L.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarelessValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = L.wrapValidator @MockDatum @()

{-# INLINEABLE mkCarelessValidator #-}
mkCarelessValidator :: MockDatum -> () -> L.ScriptContext -> Bool
mkCarelessValidator datum _ ctx =
  let txi = L.scriptContextTxInfo ctx
   in case datum of
        FirstLock ->
          L.valueLockedBy txi (L.ownHash ctx) == lockValue
            && case L.txInfoOutputs txi of
              [o] -> outputDatum (L.scriptContextTxInfo ctx) o Pl.== Just SecondLock
        SecondLock -> Pl.trace "there must be exactly one output" False

carelessValidator :: L.TypedValidator MockContract
carelessValidator =
  L.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarelessValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = L.wrapValidator @MockDatum @()

-- ** Testing the datum hijacking attack on 'TxSkels'

-- relockTxSkel' :: Monad m => L.TypedValidator MockContract -> MockChainT m TxSkel
-- relockTxSkel' v = do
--   txLock v
--   [utxo] <- scriptUtxosSuchThat v (\_ _ -> True)
--   return $ relockTxSkel v (fst utxo)

-- dhTxSkelTests :: TestTree
-- dhTxSkelTests =
--   testGroup
--     "datum hijacking tests on 'TxSkel's"
--     [ testCase "" $
--         let skelOrErr :: Either MockChainError (TxSkel, UtxoState)
--             skelOrErr = runMockChain (relockTxSkel' carefulValidator)
--          in testBool $
--               case skelOrErr of
--                 Left _ -> False
--                 Right (skel, _) ->
--                   let skelExpected =
--                         over
--                           paysScriptConstraintsT
--                           ( \case
--                               PaysScriptConstraint v d x ->
--                                 case v ~*~? carefulValidator of
--                                   Just HRefl ->
--                                     PaysScriptConstraint (trivialValidator @MockContract) d x
--                                   Nothing -> error "could not set up test"
--                           )
--                           skel
--                    in datumHijackingAttack (Nothing @()) carefulValidator (\_ _ -> True) skel
--                         == Just skelExpected
--     ]

-- ** Testing the datum hijacking attack on traces

dhTrace :: MonadBlockChain m => L.TypedValidator MockContract -> m ()
dhTrace v = do
  txLock v
  txRelock v

dhTraceTests :: TestTree
dhTraceTests =
  testGroup
    "datum hijacking traces"
    [ -- testCase "careful validator can not be fooled" $
      --   testFailsFrom'
      --     isCekEvaluationFailure
      --     def
      --     ( somewhere
      --         ( datumHijackingAttack
      --             (Nothing @())
      --             carefulValidator
      --             (\d _ -> SecondLock Pl.== d)
      --         )
      --         (dhTrace carefulValidator)
      --     ),
      -- testCase "careless validator can be fooled" $
      --   testSucceeds
      --     ( somewhere
      --         ( datumHijackingAttack
      --             (Nothing @())
      --             carelessValidator
      --             (\d _ -> SecondLock Pl.== d)
      --         )
      --         (dhTrace carelessValidator)
      --     )
      testCase "at least something works" $
        testSucceeds (dhTrace carelessValidator)
    ]

tests :: [TestTree]
tests =
  [ testGroup
      "Attack DSL"
      [ -- dhTxSkelTests,
        dhTraceTests
      ]
  ]
