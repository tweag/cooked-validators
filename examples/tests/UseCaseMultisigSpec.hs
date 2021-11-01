{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude#-}

module UseCaseMultisigSpec where

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

import qualified Prelude as Haskell

import Cooked.MockChain
import Cooked.Traces
import Cooked.Tx.Constraints
import Cooked.Tx.Generator

import Test.Hspec

import Plutus.Contracts.MultiSig

allowedSigners = map (walletPKHash . wallet) [1,4,5,7]
param = MultiSig allowedSigners 3

multiVal = typedValidator param

run1 =
  runMockChain $ do
    -- Everyone deposits 1000
    (validateTxFromSkeleton . TxSkel
      (wallet 1) . mconcat) =<< Haskell.sequence
      [ spentByPK (walletPKHash $ wallet 4) (Ada.lovelaceValueOf 1000)
      , spentByPK (walletPKHash $ wallet 5) (Ada.lovelaceValueOf 1000)
      , spentByPK (walletPKHash $ wallet 7) (Ada.lovelaceValueOf 1000)
      , return
          [ PaysScript multiVal [((), Ada.lovelaceValueOf 4000)]
          , SignedBy (map wallet [1,4,5,7])
          ]
      ]
    -- We then pay wallet 2 with this money
    [(out, dat)] <- scriptUtxosSuchThat multiVal (\_ _ -> True)
    validateTxFromSkeleton $ TxSkel
      (wallet 1)
      [ SpendsScript multiVal () (out, dat)
      , PaysPK (walletPKHash $ wallet 2) (Ada.lovelaceValueOf 2500)
      , PaysScript multiVal [((), Ada.lovelaceValueOf 1500)]
      , SignedBy (map wallet [1,2,4,7]) -- Wallet 2 was not allowed to sign, but they did it no matter
      ]

-- Test spec
spec :: Spec
spec = do
  it "succeeds on the example run" $ do
    run1 `shouldSatisfy` isRight
