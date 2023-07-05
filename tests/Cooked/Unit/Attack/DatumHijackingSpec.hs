{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Unit.Attack.DatumHijackingSpec (tests) where

import Control.Monad
import Cooked
import Cooked.Attack.DatumHijacking
import Cooked.MockChain.Staged
import Cooked.Validators.Other as Validators
import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.V1.Ledger.Value as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import Test.Tasty
import Test.Tasty.HUnit

-- Datum hijacking attacks use the 'Validators.carefulBoolD' and
-- 'carelessBoolD' and target the second transaction (with datum 'False')
-- substituting a different recipient.

-- * Transactions (and 'TxSkels') for the datum hijacking attack

lockValue :: Pl.Value
lockValue = Pl.lovelaceValueOf 12345678

lockTxSkel :: Pl.TxOutRef -> Pl.TypedValidator Validators.BoolD -> TxSkel
lockTxSkel o v =
  txSkelTemplate
    { txSkelOpts = def {txOptEnsureMinAda = True},
      txSkelIns = Map.singleton o TxSkelNoRedeemerForPK,
      txSkelOuts = [paysScriptInlineDatum v True lockValue],
      txSkelSigners = [wallet 1]
    }

txLock :: MonadBlockChain m => Pl.TypedValidator BoolD -> m ()
txLock v = do
  (oref, _) : _ <-
    runUtxoSearch $
      utxosAtSearch (walletAddress $ wallet 1)
        `filterWithPred` ((`Pl.geq` lockValue) . outputValue)
  void $ validateTxSkel $ lockTxSkel oref v

relockTxSkel :: Pl.TypedValidator BoolD -> Pl.TxOutRef -> TxSkel
relockTxSkel v o =
  txSkelTemplate
    { txSkelOpts = def {txOptEnsureMinAda = True},
      txSkelIns = Map.singleton o $ TxSkelRedeemerForScript (),
      txSkelOuts = [paysScriptInlineDatum v False lockValue],
      txSkelSigners = [wallet 1]
    }

txRelock ::
  MonadBlockChain m =>
  Pl.TypedValidator BoolD ->
  m ()
txRelock v = do
  (oref, _) : _ <-
    runUtxoSearch $
      utxosAtSearch (Pl.validatorAddress v)
        `filterWith` resolveDatum
        `filterWithPure` isOutputWithInlineDatumOfType @Bool
        `filterWithPred` ((True ==) . (^. outputDatumL))
  void $ validateTxSkel $ relockTxSkel v oref

datumHijackingTrace :: MonadBlockChain m => Pl.TypedValidator BoolD -> m ()
datumHijackingTrace v = do
  txLock v
  txRelock v

txSkelFromOuts :: [TxSkelOut] -> TxSkel
txSkelFromOuts os = txSkelTemplate {txSkelOuts = os, txSkelSigners = [wallet 1]}

-- * TestTree for the datum hijacking attack

tests :: TestTree
tests =
  testGroup
    "datum hijacking attack"
    [ testGroup "unit tests on a 'TxSkel'" $
        let val1 = Validators.carelessBoolD lockValue
            val2 = Validators.carefulBoolD lockValue
            thief = datumHijackingTarget @Validators.BoolD
            x1 = Pl.lovelaceValueOf 10001
            x2 = Pl.lovelaceValueOf 10000
            x3 = Pl.lovelaceValueOf 9999
            skelIn =
              txSkelFromOuts
                [ paysScriptInlineDatum val1 False x1,
                  paysScriptInlineDatum val1 False x3,
                  paysScriptInlineDatum val2 False x1,
                  paysScriptInlineDatum val1 True x2,
                  paysScriptInlineDatum val1 False x2
                ]
            skelOut bound select =
              runTweak
                ( datumHijackingAttack @Validators.BoolD
                    ( \(ConcreteOutput v _ x d _) ->
                        Pl.validatorHash val1
                          == Pl.validatorHash v
                          && d
                          == TxSkelOutInlineDatum False
                          && bound
                          `Pl.geq` x
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
                    [ paysScriptInlineDatum val1 False x1,
                      paysScriptInlineDatum a False x3,
                      paysScriptInlineDatum val2 False x1,
                      paysScriptInlineDatum val1 True x2,
                      paysScriptInlineDatum b False x2
                    ],
                  txSkelSigners = [wallet 1]
                }
         in [ testCase "no modified transactions if no interesting outputs to steal" $
                [] @=? skelOut mempty (const True),
              testCase "one modified transaction for one interesting output" $
                [ Right
                    ( [ConcreteOutput val1 Nothing x3 (TxSkelOutInlineDatum False) Nothing],
                      skelExpected thief val1
                    )
                ]
                  @=? skelOut x2 (0 ==),
              testCase "two modified transactions for two interesting outputs" $
                [ Right
                    ( [ ConcreteOutput val1 Nothing x3 (TxSkelOutInlineDatum False) Nothing,
                        ConcreteOutput val1 Nothing x2 (TxSkelOutInlineDatum False) Nothing
                      ],
                      skelExpected thief thief
                    )
                ]
                  @=? skelOut x2 (const True),
              testCase "select second interesting output to get one modified transaction" $
                [ Right
                    ( [ConcreteOutput val1 Nothing x2 (TxSkelOutInlineDatum False) Nothing],
                      skelExpected val1 thief
                    )
                ]
                  @=? skelOut x2 (1 ==)
            ],
      testCase "careful validator" $
        testFails
          def
          (isCekEvaluationFailure def)
          ( somewhere
              ( datumHijackingAttack @Validators.BoolD
                  ( \(ConcreteOutput v _ _ d _) ->
                      Pl.validatorHash v == Pl.validatorHash (Validators.carefulBoolD lockValue)
                        && d == TxSkelOutInlineDatum False
                  )
                  (const True)
              )
              (datumHijackingTrace $ Validators.carefulBoolD lockValue)
          ),
      testCase "careless validator" $
        testSucceeds
          def
          ( somewhere
              ( datumHijackingAttack @Validators.BoolD
                  ( \(ConcreteOutput v _ _ d _) ->
                      Pl.validatorHash v == Pl.validatorHash (Validators.carelessBoolD lockValue)
                        && d == TxSkelOutInlineDatum False
                  )
                  (const True)
              )
              (datumHijackingTrace $ Validators.carelessBoolD lockValue)
          )
    ]
