{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Attack.DatumHijackingSpec (tests) where

import Control.Monad
import Cooked
import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ledger as L hiding (validatorHash)
import qualified Ledger.Ada as L
import qualified Ledger.Value as L
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified Plutus.V2.Ledger.Contexts as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit

-- * Mock contract for the datum hijacking attack

-- This is a very simple contract: The first transaction locks some Ada to the
-- validator, using the datum 'FirstLock', the second transaction then re-locks
-- the same amount to the same validator, using the datum 'SecondLock'. The
-- datum hijacking attack should target the second transaction, and substitute a
-- different recipient.

data MockDatum = FirstLock | SecondLock deriving (Show, Eq)

instance Pretty MockDatum where
  pretty = viaShow

instance Pl.Eq MockDatum where
  {-# INLINEABLE (==) #-}
  FirstLock == FirstLock = True
  SecondLock == SecondLock = True
  _ == _ = False

Pl.makeLift ''MockDatum
Pl.unstableMakeIsData ''MockDatum

data MockContract

instance Pl.ValidatorTypes MockContract where
  type DatumType MockContract = MockDatum
  type RedeemerType MockContract = ()

-- ** Transactions (and 'TxSkels') for the datum hijacking attack

lockValue :: L.Value
lockValue = L.lovelaceValueOf 12345678

lockTxSkel :: Pl.TxOutRef -> Pl.TypedValidator MockContract -> TxSkel
lockTxSkel o v =
  txSkelTemplate
    { txSkelOpts = def {txOptEnsureMinAda = True},
      txSkelIns = Map.singleton o TxSkelNoRedeemerForPK,
      txSkelOuts = [paysScriptInlineDatum v FirstLock lockValue]
    }

txLock :: MonadBlockChain m => Pl.TypedValidator MockContract -> m ()
txLock v = do
  me <- ownPaymentPubKeyHash
  (oref, _) : _ <- filteredUtxos $ isPKOutputFrom me >=> isOutputWithValueSuchThat (`L.geq` lockValue)
  void $ validateTxSkel $ lockTxSkel oref v

relockTxSkel :: Pl.TypedValidator MockContract -> Pl.TxOutRef -> TxSkel
relockTxSkel v o =
  txSkelTemplate
    { txSkelOpts = def {txOptEnsureMinAda = True},
      txSkelIns = Map.singleton o $ TxSkelRedeemerForScript @MockContract (),
      txSkelOuts = [paysScriptInlineDatum v SecondLock lockValue]
    }

txRelock ::
  MonadBlockChain m =>
  Pl.TypedValidator MockContract ->
  m ()
txRelock v = do
  (oref, _) : _ <-
    filteredUtxosWithDatums $
      isScriptOutputFrom' v
        >=> isOutputWithDatumSuchThat (ResolvedOrInlineDatum FirstLock ==)
  void $ validateTxSkel $ relockTxSkel v oref

datumHijackingTrace :: MonadBlockChain m => Pl.TypedValidator MockContract -> m ()
datumHijackingTrace v = do
  txLock v
  txRelock v

-- * Validators for the datum hijacking attack

-- | Try to extract a datum from an output.
{-# INLINEABLE outputDatum #-}
outputDatum :: Pl.TxInfo -> Pl.TxOut -> Maybe MockDatum
outputDatum txi o = case Pl.txOutDatum o of
  Pl.NoOutputDatum -> Nothing
  Pl.OutputDatumHash h -> do
    L.Datum d <- Pl.findDatum h txi
    Pl.fromBuiltinData d
  Pl.OutputDatum (L.Datum d) -> Pl.fromBuiltinData d

{-# INLINEABLE mkMockValidator #-}
mkMockValidator :: (Pl.ScriptContext -> [Pl.TxOut]) -> MockDatum -> () -> Pl.ScriptContext -> Bool
mkMockValidator getOutputs datum _ ctx =
  let txi = Pl.scriptContextTxInfo ctx
   in case datum of
        FirstLock ->
          case getOutputs ctx of
            o : _ ->
              Pl.traceIfFalse
                "not in 'SecondLock'-state after re-locking"
                (outputDatum txi o Pl.== Just SecondLock)
                && Pl.traceIfFalse
                  "not re-locking the right amout"
                  (Pl.txOutValue o == lockValue)
            _ -> Pl.trace "there must be a output re-locked" False
        SecondLock -> False

{-# INLINEABLE mkCarefulValidator #-}
mkCarefulValidator :: MockDatum -> () -> Pl.ScriptContext -> Bool
mkCarefulValidator = mkMockValidator Pl.getContinuingOutputs

carefulValidator :: Pl.TypedValidator MockContract
carefulValidator =
  Pl.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarefulValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator @MockDatum @()

{-# INLINEABLE mkCarelessValidator #-}
mkCarelessValidator :: MockDatum -> () -> Pl.ScriptContext -> Bool
mkCarelessValidator = mkMockValidator (Pl.txInfoOutputs . Pl.scriptContextTxInfo)

carelessValidator :: Pl.TypedValidator MockContract
carelessValidator =
  Pl.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarelessValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator @MockDatum @()

txSkelFromOuts :: [TxSkelOut] -> TxSkel
txSkelFromOuts os = txSkelTemplate {txSkelOuts = os}

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
              txSkelFromOuts
                [ paysScriptInlineDatum val1 SecondLock x1,
                  paysScriptInlineDatum val1 SecondLock x3,
                  paysScriptInlineDatum val2 SecondLock x1,
                  paysScriptInlineDatum val1 FirstLock x2,
                  paysScriptInlineDatum val1 SecondLock x2
                ]
            skelOut bound select =
              runTweak
                ( datumHijackingAttack @MockContract
                    ( \(ConcreteOutput v _ x d) ->
                        Pl.validatorHash val1 == Pl.validatorHash v
                          && d == TxSkelOutInlineDatum SecondLock
                          && bound `L.geq` x
                    )
                    select
                )
                skelIn
            skelExpected a b =
              txSkelTemplate
                { txSkelLabel =
                    Set.singleton . TxLabel . DatumHijackingLbl $
                      Pl.validatorAddress thief,
                  txSkelOuts =
                    [ paysScriptInlineDatum val1 SecondLock x1,
                      paysScriptInlineDatum a SecondLock x3,
                      paysScriptInlineDatum val2 SecondLock x1,
                      paysScriptInlineDatum val1 FirstLock x2,
                      paysScriptInlineDatum b SecondLock x2
                    ]
                }
         in [ testCase "no modified transactions if no interesting outputs to steal" $ [] @=? skelOut mempty (const True),
              testCase "one modified transaction for one interesting output" $
                [ Right
                    ( [ConcreteOutput val1 Nothing x3 (TxSkelOutInlineDatum SecondLock)],
                      skelExpected thief val1
                    )
                ]
                  @=? skelOut x2 (0 ==),
              testCase "two modified transactions for two interesting outputs" $
                [ Right
                    ( [ ConcreteOutput val1 Nothing x3 (TxSkelOutInlineDatum SecondLock),
                        ConcreteOutput val1 Nothing x2 (TxSkelOutInlineDatum SecondLock)
                      ],
                      skelExpected thief thief
                    )
                ]
                  @=? skelOut x2 (const True),
              testCase "select second interesting output to get one modified transaction" $
                [ Right
                    ( [ConcreteOutput val1 Nothing x2 (TxSkelOutInlineDatum SecondLock)],
                      skelExpected val1 thief
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
                  ( \(ConcreteOutput v _ _ d) ->
                      Pl.validatorHash v == Pl.validatorHash carefulValidator
                        && d == TxSkelOutInlineDatum SecondLock
                  )
                  (const True)
              )
              (datumHijackingTrace carefulValidator)
          ),
      testCase "careless validator" $
        testSucceeds
          ( somewhere
              ( datumHijackingAttack @MockContract
                  ( \(ConcreteOutput v _ _ d) ->
                      Pl.validatorHash v == Pl.validatorHash carelessValidator
                        && d == TxSkelOutInlineDatum SecondLock
                  )
                  (const True)
              )
              (datumHijackingTrace carelessValidator)
          )
    ]
