{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Behaviour.ReferenceInputsSpec where

import Control.Monad
import Cooked
import qualified Cooked.Behaviour.Validators as Validators
import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

trace1 :: MonadBlockChain m => m ()
trace1 = do
  (txOutRefFoo, _) : (txOutRefBar, _) : _ <-
    utxosFromCardanoTx
      <$> validateTxSkel
        txSkelTemplate
          { txSkelOuts =
              [ paysScriptInlineDatum
                  Validators.pkNotInDatum
                  (walletPKHash (wallet 3))
                  (Pl.lovelaceValueOf 4_000_000),
                paysScript
                  Validators.otherInputDatum
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
