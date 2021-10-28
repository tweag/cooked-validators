{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude#-}

module ForgeSpec where

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

import Forge hiding (bigBossNFT, authToken, smithedToken)

import Test.Hspec

-- The NFT to control everything.
bigBossTok = Value.TokenName "BigBossNFT"
bigBossPolicy = Currency.curPolicy $ Currency.OneShotCurrency (h,i) (AssocMap.fromList [(bigBossTok, 1)])
  where
    Right ((h,i),_) = runMockChain $ do
      [(Validation.TxOutRef h i, _)] <- pkUtxos' (walletPKHash $ wallet 1)
      return (h,i)
bigBossCurr = Validation.scriptCurrencySymbol bigBossPolicy
bigBossNFT = Value.assetClass bigBossCurr bigBossTok

-- An auth token can only be minted or destroyed if the BigBossNFT is used.
-- It is important to note that one has to create a function taking a 'Pl.AssetClass' argument,
-- because if we directly put 'bigBossNFT', then we obtain a 'CekEvaluationFailure',
-- due to a non-inlinable thing in its definition.
{-# INLINEABLE mkAuthTokenPolicy #-}
mkAuthTokenPolicy :: Value.AssetClass -> () -> Ledger.ScriptContext -> Bool
mkAuthTokenPolicy nft _ ctx =
  traceIfFalse "NFT missing" (Value.assetClassValueOf (Validation.valueSpent info) nft == 1)
  where
    info :: Validation.TxInfo
    info = Validation.scriptContextTxInfo ctx

authTokenPolicy :: TScripts.MintingPolicy
authTokenPolicy =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| TScripts.wrapMintingPolicy . mkAuthTokenPolicy ||])
      `PlutusTx.applyCode` PlutusTx.liftCode bigBossNFT

authTokenCurrency :: Value.CurrencySymbol
authTokenCurrency = Validation.scriptCurrencySymbol authTokenPolicy

authTokenTok = Value.TokenName "ForgeAuth"
authToken = Value.assetClass authTokenCurrency authTokenTok

-- The smithed token can only be forged if the auth token is used.

{-# INLINEABLE mkSmithingPolicy #-}
mkSmithingPolicy :: Value.AssetClass -> () -> Ledger.ScriptContext -> Bool
mkSmithingPolicy authTok _ ctx =
  traceIfFalse "AuthToken missing" (Value.assetClassValueOf (Validation.valueSpent info) authTok == 1)
  where
    info :: Validation.TxInfo
    info = Validation.scriptContextTxInfo ctx

smithingPolicy :: TScripts.MintingPolicy
smithingPolicy =
  Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| TScripts.wrapMintingPolicy . mkSmithingPolicy ||])
      `PlutusTx.applyCode` PlutusTx.liftCode authToken

smithingCurrency :: Value.CurrencySymbol
smithingCurrency = Validation.scriptCurrencySymbol smithingPolicy

smithingToken = Value.TokenName "Token"
smithed = Value.assetClass smithingCurrency smithingToken

params :: Params
params = Params bigBossNFT authToken smithed

bigBossVal = bigBossTypedValidator params
smithVal = smithTypedValidator params

run1 =
  runMockChain $ do
    -- We start with the creation of the NFT
    validateTxFromSkeleton $ TxSkel
      (wallet 1)
      [ Mints [bigBossPolicy] oneBBNFT
      , PaysScript bigBossVal [(BigBoss [], oneBBNFT)]
      ]
    -- We then open a forge
    [(outBB, datBB@(BigBoss l))] <- scriptUtxosSuchThat bigBossVal (\_ _ -> True)
    validateTxFromSkeleton $ TxSkel
      (wallet 3)
      [ SpendsScript bigBossVal Open (outBB, datBB)
      , Mints [authTokenPolicy] oneAuthToken
      , PaysScript bigBossVal [(BigBoss [w3PKH], oneBBNFT)]
      , PaysScript smithVal [(Forge w3PKH 0, oneAuthToken)]
      ]
    -- We use this forge to mint 3 tokens
    [(outSmith, datSmith@(Forge owner forged))] <- scriptUtxosSuchThat smithVal (\_ _ -> True)
    validateTxFromSkeleton $ TxSkel
      (wallet 3)
      [ SpendsScript smithVal Adjust (outSmith, datSmith)
      , Mints [smithingPolicy] (Value.assetClassValue smithed 30)
      , PaysScript smithVal [(Forge w3PKH 30, oneAuthToken <> Ada.lovelaceValueOf 500)]
      , PaysPK w3PKH (Value.assetClassValue smithed 30)
      ]
  where
    oneBBNFT = Value.assetClassValue bigBossNFT 1
    oneAuthToken = Value.assetClassValue authToken 1
    w3PKH = walletPKHash $ wallet 3

-- Test spec
spec :: Spec
spec = do
  it "succeeds on the example run" $ do
    run1 `shouldSatisfy` isRight
