{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude#-}

module UseCasesAuctionSpec where

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
import qualified Plutus.Contract.StateMachine.ThreadToken as ThreadToken

import Cooked.MockChain
import Cooked.Traces
import Cooked.Tx.Constraints
import Cooked.Tx.Generator

import UseCaseAuction

import Test.Hspec


txORef :: Validation.TxOutRef
Right ([(txORef,_)],_) = runMockChain $ pkUtxos' (walletPKHash $ wallet 1)

-- The thread token.
threadPolicy = ThreadToken.curPolicy txORef
threadCurr = Validation.scriptCurrencySymbol threadPolicy
threadToken = ThreadToken.ThreadToken txORef threadCurr


params :: AuctionParams
params = AuctionParams (walletPKHash $ wallet 1) mempty (Ledger.POSIXTime 1596059191000)

auctionVal = typedValidator (threadToken, params)

threadAsset = case TScripts.validatorHash auctionVal of
  Scripts.ValidatorHash vH -> Value.assetClass threadCurr (Value.TokenName vH)

run1 =
  runMockChain $ do
    return ()

-- Test spec
spec :: Spec
spec = do
  it "succeeds on the example run" $ do
    run1 `shouldSatisfy` isRight