{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UseCaseCrowdfundingSpec where

import Control.Monad
import Control.Monad.Writer
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
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
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude hiding (show, trace)
import Test.Hspec
import Prelude (show)
import qualified Prelude as Haskell

-- One has to duplicate some functions of the contract,
-- since they are not exported.

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
  type DatumType Crowdfunding = Ledger.PubKeyHash

deriving instance Haskell.Show CampaignAction

-- run1 :: Either MockChainError ((), UtxoState)
run1 =
  head $
    runWriterT $
      runMockChainT $
        interpret $ do
          t <- currentTime
          -- A simple campaign starting now;
          -- The campaign lasts 20 seconds and the funds can be redeemed between 20 and 30 seconds.
          let camp = theCampaign t
              crowdVal = typedValidator camp

          void $
            validateTxFromSkeleton $
              txSkel
                (wallet 3)
                [ PaysScript crowdVal [(walletPKHash (wallet 3), Ada.lovelaceValueOf 4000)]
                ]
          void $
            validateTxFromSkeleton $
              txSkel
                (wallet 4)
                [ PaysScript crowdVal [(walletPKHash (wallet 4), Ada.lovelaceValueOf 6000)]
                ]
          -- We now set the time to be 25 seconds after the beginning; because
          -- each validateTxFromSkeleton increases the slot counter, we actually
          -- say 20000ms in here, since there were two slots that passed
          _ <- waitNMilliSeconds 20000
          funds <- scriptUtxosSuchThat crowdVal (\_ _ -> True)
          void $
            validateTxFromSkeleton $
              txSkel
                (wallet 1)
                ( map (SpendsScript crowdVal Collect) funds
                    ++ [ PaysPK (walletPKHash $ wallet 1) (Ada.lovelaceValueOf 10000),
                         ValidateIn $ Ledger.interval (t + Ledger.POSIXTime 20000) (t + Ledger.POSIXTime 27000)
                       ]
                )

-- Test spec
spec :: Spec
spec = do
  it "succeeds on the example run" $ do
    run1 `shouldSatisfy` (isRight . fst)
