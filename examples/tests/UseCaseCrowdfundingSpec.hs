{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UseCaseCrowdfundingSpec where

import Control.Monad
import Control.Monad.Writer
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import Data.Either (isRight)
import Debug.Trace
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Contexts as Validation
import qualified Ledger.TimeSlot as TimeSlot
import qualified Ledger.Typed.Scripts as TScripts
import qualified Ledger.Value as Value
import qualified Plutus.Contract.StateMachine.ThreadToken as ThreadToken
import Plutus.Contracts.Crowdfunding
import qualified Plutus.Contracts.Currency as Currency
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusTx (compile)
import Test.Hspec
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import Text.Heredoc

-- | We copy the validator since its not exported from Crowdfunding
typedValidator :: Campaign -> TScripts.TypedValidator Crowdfunding
typedValidator =
  TScripts.mkTypedValidatorParam @Crowdfunding
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = TScripts.wrapValidator

data Crowdfunding

instance TScripts.ValidatorTypes Crowdfunding where
  type RedeemerType Crowdfunding = CampaignAction
  type DatumType Crowdfunding = Ledger.PaymentPubKeyHash

deriving instance Show CampaignAction

-- | Generates an aribtrary campaign with a the collection deadline set as
-- a delta on top of the payment deadline.
genCampaign :: (MonadBlockChain m) => Integer -> Ledger.POSIXTime -> Wallet -> m (Ledger.POSIXTime, Campaign)
genCampaign collectDelta deadline owner = do
  startTime <- currentTime
  let collectDeadline = deadline + Ledger.POSIXTime collectDelta
  return . (startTime,) $
    Campaign
      { campaignDeadline = startTime + deadline,
        campaignCollectionDeadline = startTime + collectDeadline,
        campaignOwner = Ledger.PaymentPubKeyHash $ walletPKHash owner
      }

-- | Provides some funds to the campaign
paysCampaign :: (MonadMockChain m) => Campaign -> Wallet -> Ledger.Value -> m ()
paysCampaign c w val =
  void $
    signs w $
      validateTxConstrOpts
        (def {autoSlotIncrease = False})
        [PaysScript (typedValidator c) [(Ledger.PaymentPubKeyHash $ walletPKHash w, val)]]

-- | Retrieve funds as being the owner
retrieveFunds :: (MonadMockChain m) => Ledger.POSIXTime -> Campaign -> Wallet -> m ()
retrieveFunds t c owner = do
  funds <- scriptUtxosSuchThat (typedValidator c) (\_ _ -> True)
  void $
    signs owner $
      validateTxConstrOpts
        (def {autoSlotIncrease = False})
        ( map (SpendsScript (typedValidator c) Collect) funds
            ++ [ PaysPK (walletPKHash owner) (mconcat $ map (sOutValue . fst) funds),
                 ValidateIn $ collectionRange c
               ]
        )

-- * Tests

tests :: TestTree
tests =
  localOption (QuickCheckTests 25) $
    testGroup
      "UseCaseCrowdfunding"
      [ownerCanRetrieveFunds, ownerCanRetrieveFundsQC]

ownerCanRetrieveFunds :: TestTree
ownerCanRetrieveFunds =
  testCase "Funds can be retrieved" $ isRight (runMockChain mtrace) @? "Trace failed"
  where
    mtrace :: MonadMockChain m => m ()
    mtrace = do
      t <- currentTime
      -- A simple campaign starting now;
      -- The campaign lasts 20 seconds and the funds can be redeemed between 20 and 30 seconds.
      let c = theCampaign t
      paysCampaign c (wallet 3) (Ada.adaValueOf 3)
      paysCampaign c (wallet 4) (Ada.adaValueOf 4)
      waitNMilliSeconds 25000
      retrieveFunds t c (wallet 1)

-- Generates an arbitrary campaign, then generates arbitrary payments that execute
-- all within the \hs{campaignDeadline}, then collect the funds within \hs{campaignCollectionDeadline}.
-- This should invariably succeed.
ownerCanRetrieveFundsQC :: TestTree
ownerCanRetrieveFundsQC =
  testProperty "Funds can be retrieved (QuickCheck)" $
    QC.forAll
      genParms
      ( \MyParms {..} -> testSucceeds @QC.Property $ do
          -- generates a campaign with at least d milliseconds of available time between
          -- deadline and collection deadline
          (t0, c) <- genCampaign d deadline owner

          -- Generates and the payments:
          forM_ payments $ \(w, amount) -> do
            paysCampaign c w (Ada.lovelaceValueOf $ amount * 1000)

          -- Now we must wait for the campaign deadline to pass, otherwise the
          -- funds won't be collectible
          awaitTime (campaignDeadline c)
          retrieveFunds t0 c owner

          -- Finally, return the amount of money gathered by the owner
          return $ sum $ map snd payments
      )
  where
    genParms =
      MyParms <$> QC.choose (10000, 20000)
        <*> (wallet <$> QC.choose (1, 10))
        <*> (Ledger.POSIXTime <$> QC.choose (20000, 40000))
        <*> QC.resize 6 (QC.listOf1 ((,) <$> (wallet <$> QC.choose (1, 10)) <*> QC.choose (2000, 10000)))

data MyParms = MyParms
  { d :: Integer,
    owner :: Wallet,
    deadline :: Ledger.POSIXTime,
    payments :: [(Wallet, Integer)]
  }
  deriving (Show)
