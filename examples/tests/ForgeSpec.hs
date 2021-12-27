{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module ForgeSpec where

import Control.Monad.Writer hiding ((<>))
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import Forge hiding (authToken, bigBossNFT, smithedToken)
import Forge.ExampleTokens
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Contexts as Validation
import qualified Ledger.TimeSlot as TimeSlot
import qualified Ledger.Typed.Scripts as TScripts
import qualified Ledger.Value as Value
import qualified Plutus.Contracts.Currency as Currency
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import Test.Tasty
import Test.Tasty.HUnit

minAda :: Ledger.Value
minAda = Ada.lovelaceValueOf 2000000

-- | Grabs the first UTxO belonging to the given wallet, uses it to initialize the
--  main NFT; then mints said nft and creates a 'BigBoss' datum.
initBigBoss :: (MonadBlockChain m) => Wallet -> m BigBossId
initBigBoss w = do
  (Validation.TxOutRef h i, _) : _ <- pkUtxos' (walletPKHash w)
  let bbId = (h, i)
  let oneBBNFT = Value.assetClassValue (bigBossNFT bbId) 1
  void $
    validateTxSkel $
      txSkelLbl
        InitBigBoss
        w
        [ mints [bigBossPolicy bbId] oneBBNFT,
          PaysScript (bigBossVal bbId) [(BigBoss [], oneBBNFT <> minAda)]
        ]
  return bbId

data InitBigBoss = InitBigBoss deriving (Show)

-- | Opens up a new forge belonging to a given wallet
openForge :: (MonadBlockChain m) => BigBossId -> Wallet -> m ()
openForge bbId w = do
  [(outBB, datBB@(BigBoss l))] <- scriptUtxosSuchThat (bigBossVal bbId) (\_ _ -> True)
  let oneBBNFT = Value.assetClassValue (bigBossNFT bbId) 1
  let oneAuthToken = Value.assetClassValue (authToken bbId) 1
  let wPKH = walletPKHash w
  void $
    validateTxSkel $
      txSkelLbl
        OpenForge
        w
        [ SpendsScript (bigBossVal bbId) Open (outBB, datBB),
          mints [authTokenPolicy bbId] oneAuthToken,
          PaysScript (bigBossVal bbId) [(BigBoss [wPKH], oneBBNFT <> minAda)],
          PaysScript (smithVal bbId) [(Forge wPKH 0, oneAuthToken <> minAda)]
        ]

data OpenForge = OpenForge deriving (Show)

-- | Smiths from the first forge that came out of the given 'BigBossId' and belongs to
--  the given wallet
smiths :: (MonadBlockChain m) => BigBossId -> Wallet -> Integer -> m ()
smiths bbId w val = do
  (outSmith, datSmith@(Forge owner forged)) : _ <- scriptUtxosSuchThat (smithVal bbId) (\d _ -> d `belongsTo` w)
  void $
    validateTxSkel $
      txSkelLbl
        (Smiths val)
        w
        [ SpendsScript (smithVal bbId) Adjust (outSmith, datSmith),
          mints [smithingPolicy bbId] (Value.assetClassValue (smithed bbId) val),
          PaysScript (smithVal bbId) [(Forge owner (forged + val), sOutValue outSmith)],
          PaysPK (walletPKHash w) (Value.assetClassValue (smithed bbId) val <> minAda)
        ]
  where
    belongsTo (Forge owner _) w = owner == walletPKHash w

newtype Smiths = Smiths Integer deriving (Show)

tests :: TestTree
tests =
  testGroup
    "ForgeSpec"
    [ testCase "Can create a forge and smith tokens" $
        assertSucceeds $ do
          bbId <- initBigBoss (wallet 1)
          openForge bbId (wallet 3)
          smiths bbId (wallet 3) 100
    ]
