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
import qualified Ledger.Value as L
import Optics.Core
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Tasty
import Test.Tasty.HUnit
import Type.Reflection

-- * Tests for the token duplication attack

-- ** Minting policies to test the token duplication attack

{-# INLINEABLE mkCarefulPolicy #-}
mkCarefulPolicy :: L.TokenName -> Integer -> () -> L.ScriptContext -> Bool
mkCarefulPolicy tName allowedAmount _ ctx
  | amnt Pl.== Just allowedAmount = True
  | otherwise = False
  where
    txi = L.scriptContextTxInfo ctx
    L.Minting me = L.scriptContextPurpose ctx

    amnt :: Maybe Integer
    amnt = case L.flattenValue (L.txInfoMint txi) of
      [(cs, tn, a)] | cs Pl.== L.ownCurrencySymbol ctx && tn Pl.== tName -> Just a
      _ -> Nothing

carefulPolicy :: L.TokenName -> Integer -> L.MintingPolicy
carefulPolicy tName allowedAmount =
  L.mkMintingPolicyScript $
    $$(Pl.compile [||\n x -> L.wrapMintingPolicy (mkCarefulPolicy n x)||])
      `Pl.applyCode` Pl.liftCode tName
      `Pl.applyCode` Pl.liftCode allowedAmount

{-# INLINEABLE mkCarelessPolicy #-}
mkCarelessPolicy :: () -> L.ScriptContext -> Bool
mkCarelessPolicy _ _ = True

carelessPolicy :: L.MintingPolicy
carelessPolicy =
  L.mkMintingPolicyScript
    $$(Pl.compile [||L.wrapMintingPolicy mkCarelessPolicy||])

-- ** Transactions (and Skeletons) for the token duplication attack

mintNTxSkel :: Integer -> L.MintingPolicy -> L.TokenName -> Wallet -> TxSkel
mintNTxSkel amount pol tName recipient =
  let minted = L.assetClassValue (L.assetClass (L.scriptCurrencySymbol pol) tName) amount
   in txSkelOpts
        (def {adjustUnbalTx = True})
        ( [Mints (Nothing @()) [pol] minted]
            :=>: [paysPK (walletPKHash recipient) minted]
        )

-- mintOneTxSkel :: L.MintingPolicy -> L.TokenName -> Wallet -> TxSkel
-- mintOneTxSkel = mintNTxSkel 1

-- ** Testing the token duplication attack on 'TxSkel's
-- TODO simpler name(s)
tdTxSkelTests :: TestTree
tdTxSkelTests =
  testGroup
    "tests for the token duplication attack on 'TxSkel's"
    [ testCase "increase one minted token to two" $
        let attacker = wallet 6
            tName = L.tokenName "MockToken"
            pol = carefulPolicy tName 1
            skelIn = mintNTxSkel 1 pol tName (wallet 1)
            ac = L.assetClass (L.scriptCurrencySymbol pol) tName
            skelOut = dupTokenAttack (\_ n -> Just $ n + 1) attacker skelIn
            skelExpected =
              over outConstraintsL (paysPK (walletPKHash attacker) (L.assetClassValue ac 1) :) $
                set (mintsConstraintsT % valueL) (L.assetClassValue ac 2) skelIn
         in testBool $ skelOut == Just skelExpected
    ]

-- ** Testing the token duplication attack on traces

-- only one TestTree for tken duplicatio
-- tdTraceTests :: TestTree
-- tdTraceTests =
--   testGroup
--     "tests for the token duplication attack on traces"
--     [ testCase "careful policy can not be fooled" $
--         let attacker = wallet 6
--             tName = L.tokenName "MockToken"
--             pol = carefulPolicy tName 1
--             skelIn = mintOneTxSkel pol tName (wallet 1)
--             ac = L.assetClass (L.scriptCurrencySymbol pol) tName
--             skelOut = dupTokenAttack (Nothing @()) (\_ n -> Just $ n + 1) attacker skelIn
--             skelExpected =
--               over outConstraintsL (paysPK (walletPKHash attacker) (L.assetClassValue ac 1) :) $
--                 set (mintsConstraintsT % valueL) (L.assetClassValue ac 2) skelIn
--          in testFails (somewhere (dupTokenAttack (Nothing @()) (\_ n -> Just $ n + 1) attacker))

--     ]

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

{-# INLINEABLE mkCarefulValidator #-}
mkCarefulValidator :: MockDatum -> () -> L.ScriptContext -> Bool
mkCarefulValidator datum _ ctx =
  let txi = L.scriptContextTxInfo ctx
   in case datum of
        FirstLock ->
          case L.getContinuingOutputs ctx of
            [o] ->
              Pl.traceIfFalse
                "not in 'SecondLock'-state after re-locking"
                (outputDatum txi o Pl.== Just SecondLock)
                && Pl.traceIfFalse
                  "not re-locking the right amout"
                  (L.txOutValue o == lockValue)
            _ -> Pl.trace "there must be exactly one output re-locked" False
        SecondLock -> False

carefulValidator :: L.TypedValidator MockContract
carefulValidator =
  L.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarefulValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = L.wrapValidator @MockDatum @()

{-# INLINEABLE mkCarelessValidator #-}
mkCarelessValidator :: MockDatum -> () -> L.ScriptContext -> Bool
mkCarelessValidator datum _ ctx =
  let txi = L.scriptContextTxInfo ctx
   in case datum of
        FirstLock ->
          case L.txInfoOutputs txi of
            [o] ->
              Pl.traceIfFalse
                "not in 'SecondLock'-state after re-locking"
                (outputDatum txi o Pl.== Just SecondLock)
                && Pl.traceIfFalse
                  "not re-locking the right amout"
                  (L.txOutValue o == lockValue)
            _ -> Pl.trace "there must be exactly one output re-locked" False
        SecondLock -> False

carelessValidator :: L.TypedValidator MockContract
carelessValidator =
  L.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarelessValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = L.wrapValidator @MockDatum @()

-- ** testing the datum hijacking attack on 'TxSkels'

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
    [ testCase "careful validator can not be fooled" $
        testFailsFrom'
          isCekEvaluationFailure
          def
          ( somewhere
              ( datumHijackingAttack
                  carefulValidator
                  (\d _ -> FirstLock Pl.== d)
              )
              (dhTrace carefulValidator)
          ),
      -- testCase "careless validator can be fooled" $
      --   testSucceeds
      --     ( somewhere
      --         ( datumHijackingAttack
      --             (Nothing @())
      --             carelessValidator
      --             (\d _ -> SecondLock Pl.== d)
      --         )
      --         (dhTrace carelessValidator)
      --     ),
      testCase "at least something works" $
        testSucceeds (dhTrace carefulValidator)
    ]

tests :: [TestTree]
tests =
  [ testGroup
      "Attack DSL"
      [ tdTxSkelTests,
        -- dhTxSkelTests,
        dhTraceTests
      ]
  ]
