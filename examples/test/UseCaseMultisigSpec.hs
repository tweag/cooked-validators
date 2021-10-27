{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude#-}

module Main where

import Data.Default

import qualified Ledger
import qualified Ledger.TimeSlot           as TimeSlot
import qualified Ledger.Ada                as Ada
import PlutusTx.Prelude
import qualified Plutus.Contracts.Currency as Currency
import qualified Ledger.Value              as Value
import qualified Ledger.Contexts           as Validation
import qualified PlutusTx.AssocMap         as AssocMap
import qualified PlutusTx
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts     as TScripts

import Cooked.MockChain
import Cooked.Traces
import Cooked.Tx.Constraints
import Cooked.Tx.Generator

import Plutus.Contracts.MultiSig

allowedSigners = map (walletPKHash . wallet) [1,4,5,7]
param = MultiSig allowedSigners 3

multiVal = typedValidator param

run1 =
  runMockChain $ do
    -- Everyone deposits 1000
    (validateTxFromSkeleton . TxSkel
      (wallet 1) . mconcat) =<< sequence
      [ spentByPK (walletPKHash $ wallet 1) (Ada.lovelaceValueOf 1000)
      , spentByPK (walletPKHash $ wallet 4) (Ada.lovelaceValueOf 1000)
      , spentByPK (walletPKHash $ wallet 5) (Ada.lovelaceValueOf 1000)
      , spentByPK (walletPKHash $ wallet 6) (Ada.lovelaceValueOf 1000)
      , return
          [PaysScript multiVal [((), Ada.lovelaceValueOf 4000)]
          ]
      ]
    -- [(out, dat)] <- scriptUtxosSuchThat multiVal (\_ _ -> True)
    -- We then pay wallet 2 with this money
    -- validateTxFromSkeleton $ TxSkel
    --   (wallet 1)
    --   [ SpendsScript multiVal () (out, dat)
    --   , PaysPK (walletPKHash $ wallet 2) (Ada.lovelaceValueOf 2500)
    --   ]