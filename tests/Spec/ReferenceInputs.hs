{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.ReferenceInputs where

import Cooked
import Data.Map qualified as Map
import Data.Set qualified as Set
import Plutus.ReferenceInputs
import Plutus.Script.Utils.V2 qualified as Script
import Prettyprinter qualified as PP
import Test.Tasty qualified as Tasty

instance PrettyCooked FooDatum where
  prettyCookedOpt opts (FooDatum pkh) = "FooDatum" PP.<+> prettyHash opts pkh

trace1 :: DirectMockChain ()
trace1 = do
  (txOutRefFoo, _) : (txOutRefBar, _) : _ <-
    validateTxSkel'
      txSkelTemplate
        { txSkelOuts =
            [ fooTypedValidator `receives` Value (Script.ada 4) <&&> InlineDatum (FooDatum $ Script.toPubKeyHash $ wallet 3),
              barTypedValidator `receives` Value (Script.ada 5)
            ],
          txSkelSignatories = txSkelSignatoriesFromList [wallet 2]
        }
  validateTxSkel_
    txSkelTemplate
      { txSkelIns = Map.singleton txOutRefBar $ someTxSkelRedeemer (),
        txSkelInsReference = Set.singleton txOutRefFoo,
        txSkelOuts = [wallet 4 `receives` Value (Script.ada 5)],
        txSkelSignatories = txSkelSignatoriesFromList [wallet 3]
      }

trace2 :: DirectMockChain ()
trace2 = do
  (refORef, _) : (scriptORef, _) : _ <-
    validateTxSkel'
      ( txSkelTemplate
          { txSkelOuts =
              [ wallet 1 `receives` Value (Script.ada 2) <&&> VisibleHashedDatum (10 :: Integer),
                bazTypedValidator `receives` Value (Script.ada 10)
              ],
            txSkelSignatories = txSkelSignatoriesFromList [wallet 2]
          }
      )
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [wallet 1],
        txSkelIns = Map.singleton scriptORef (someTxSkelRedeemer ()),
        txSkelInsReference = Set.singleton refORef
      }

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Reference inputs"
    [ testCooked "We can reference an input that can't be spent" $ mustSucceedTest trace1,
      testCooked "We can decode the datum hash from a reference input" $ mustSucceedTest trace2
    ]
