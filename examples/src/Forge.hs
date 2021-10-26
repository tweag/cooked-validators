{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Forge where

-- Here are the necessary imports. It is VERY IMPORTANT to refrain from using
-- anything that is not a simple accessor from Ledger; this will not
-- compile to PlutusCore.
import           Data.Aeson            (FromJSON, ToJSON)
import           GHC.Generics          (Generic)
import qualified Ledger
import qualified Ledger.Ada            as Ada
import qualified Ledger.Contexts       as Validation
import qualified Ledger.Typed.Scripts  as Scripts
import qualified Ledger.Value          as Value

-- The PlutusTx and its prelude provide the functions we can use for on-chain computations.
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (Applicative (..))
import qualified Plutus.V2.Ledger.Api  as Api

import           Schema                (ToSchema)

import qualified Prelude               as Haskell

data Params = Params
  { bigBossNFT :: Value.AssetClass
  , authToken :: Value.AssetClass
  , smithedToken :: Value.AssetClass
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
PlutusTx.makeLift ''Params

newtype DatumBigBoss = BigBoss [Api.PubKeyHash]
  deriving stock (Haskell.Show)
PlutusTx.unstableMakeIsData ''DatumBigBoss

instance Eq DatumBigBoss where
  {-# INLINABLE (==) #-}
  BigBoss l1 == BigBoss l2 = l1 == l2

data RedeemerBigBoss = Open | CloseBB
PlutusTx.unstableMakeIsData ''RedeemerBigBoss

instance Eq RedeemerBigBoss where
  {-# INLINABLE (==) #-}
  Open == Open = True
  CloseBB == CloseBB = True
  _ == _ = False

data DatumSmith = Forge {
  owner  :: Api.PubKeyHash
, forged :: Integer
}
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''DatumSmith

instance Eq DatumSmith where
  {-# INLINABLE (==) #-}
  Forge o1 f1 == Forge o2 f2 = o1 == o2 && f1 == f2

data RedeemerSmith = Adjust | CloseSmith
PlutusTx.unstableMakeIsData ''RedeemerSmith

instance Eq RedeemerSmith where
  {-# INLINABLE (==) #-}
  Adjust == Adjust = True
  CloseSmith == CloseSmith = True
  _ == _ = False

{-# INLINEABLE hasAsset #-}
hasAsset :: Value.AssetClass -> Ledger.TxOut -> Bool
hasAsset asset txO = Value.assetClassValueOf (Ledger.txOutValue txO) asset > 0

{-# INLINEABLE findTheInputWithAsset #-}
findTheInputWithAsset :: Value.AssetClass -> Validation.TxInfo -> Ledger.TxOut
findTheInputWithAsset asset info =
  case filter (hasAsset asset) $ Validation.txInInfoResolved <$> Validation.txInfoInputs info of
    [o] -> o
    _ -> traceError "Expected exactly one output"

{-# INLINEABLE findTheOutputWithAsset #-}
findTheOutputWithAsset :: Value.AssetClass -> Validation.TxInfo -> Ledger.TxOut
findTheOutputWithAsset asset info =
  case filter (hasAsset asset) $ Validation.txInfoOutputs info of
    [o] -> o
    _ -> traceError "Expected exactly one output"

{-# INLINEABLE isInExactlyOneInput #-}
isInExactlyOneInput :: Value.AssetClass -> Validation.TxInfo -> Bool
isInExactlyOneInput asset info =
  case filter (hasAsset asset) $ Validation.txInfoOutputs info of
    [_] -> True
    _ -> False

{-# INLINEABLE findDatumFromOutput #-}
findDatumFromOutput :: (PlutusTx.FromData a) => Validation.TxInfo -> Ledger.TxOut -> Maybe a
findDatumFromOutput info output = do
  datHash <- Validation.txOutDatumHash output
  dat <- Validation.findDatum datHash info
  PlutusTx.fromBuiltinData $ Ledger.getDatum dat

{-# INLINABLE validateBigBoss #-}
validateBigBoss :: Params -> DatumBigBoss -> RedeemerBigBoss -> Validation.ScriptContext -> Bool
validateBigBoss Params {..} (BigBoss sigs) Open ctx =
  traceIfFalse "Smith already owns a forge" (not (elem a sigs)) &&
  traceIfFalse "Missing Big Boss NFT" (isInExactlyOneInput bigBossNFT info) &&
  traceIfFalse "Initial Forge must be empty" (forged newForge == 0) &&
  traceIfFalse "One cannot open a forge for another one" (elem a transacSigners) &&
  traceIfFalse "Datum of the Big Boss output is wrong" (newBigBossDatum == BigBoss (a:sigs))
  where
    info = Validation.scriptContextTxInfo ctx
    a = owner newForge
    Just newForge = findDatumFromOutput info $ findTheOutputWithAsset authToken info
    Just newBigBossDatum = findDatumFromOutput info $ findTheOutputWithAsset bigBossNFT info
    transacSigners = Validation.txInfoSignatories info

validateBigBoss Params {..} (BigBoss sigs) CloseBB ctx =
  traceIfFalse "Closing non authentic forge" (isInExactlyOneInput authToken info) &&
  traceIfFalse "Datum of the BigBoss output is wrong" (newBigBossDatum == remove a sigs) &&
  traceIfFalse "Must destroy exactly one authentification token" (Validation.txInfoMint info == Value.assetClassValue authToken (-1))
  where
    info = Validation.scriptContextTxInfo ctx
    a = owner oldForge
    Just oldForge = findDatumFromOutput info $ findTheInputWithAsset authToken info
    Just newBigBossDatum = findDatumFromOutput info $ findTheOutputWithAsset bigBossNFT info
    remove :: (Eq b) => b -> [b] -> [b]
    remove _ [] = []
    remove b (x:tl) =
      if x == b
      then remove b tl
      else x:remove b tl


{-# INLINABLE validateSmith #-}
validateSmith :: Params -> DatumSmith -> RedeemerSmith -> Validation.ScriptContext -> Bool
validateSmith Params {..} Forge {owner=a} CloseSmith ctx =
  traceIfFalse "Missing Big Boss NFT" (isInExactlyOneInput bigBossNFT info) &&
  traceIfFalse "One cannot close a forge for another one" (elem a transacSigners)
  where
    info = Validation.scriptContextTxInfo ctx
    transacSigners = Validation.txInfoSignatories info

validateSmith Params {..} oldForge@Forge {owner=a} Adjust ctx =
  traceIfFalse "One cannot modify a forge for another one" (elem a transacSigners) &&
  traceIfFalse "The owner is the same" (a == owner newForge) &&
  traceIfFalse "The forged value cannot be negative" (forged newForge >= 0) &&
  traceIfFalse "One must deposit at least 10 Adas per forged token" (Ada.getLovelace (Ada.fromValue (Validation.txOutValue out)) >= 10 * forged newForge) &&
  traceIfFalse "Minting must be correct" (Validation.txInfoMint info == Value.assetClassValue smithedToken (forged newForge - forged oldForge))
  where
    info = Validation.scriptContextTxInfo ctx
    out = findTheOutputWithAsset authToken info
    Just newForge = findDatumFromOutput info out
    transacSigners = Validation.txInfoSignatories info

data BigBoss
instance Scripts.ValidatorTypes BigBoss where
  type instance RedeemerType BigBoss = RedeemerBigBoss
  type instance DatumType    BigBoss = DatumBigBoss

bigBossTypedValidator :: Params -> Scripts.TypedValidator BigBoss
bigBossTypedValidator = Scripts.mkTypedValidatorParam @BigBoss
              $$(PlutusTx.compile [|| validateBigBoss ||])
              $$(PlutusTx.compile [|| wrap ||])
  where wrap = Scripts.wrapValidator @DatumBigBoss @RedeemerBigBoss

data Smith
instance Scripts.ValidatorTypes Smith where
  type instance RedeemerType Smith = RedeemerSmith
  type instance DatumType    Smith = DatumSmith

smithTypedValidator :: Params -> Scripts.TypedValidator Smith
smithTypedValidator = Scripts.mkTypedValidatorParam @Smith
              $$(PlutusTx.compile [|| validateSmith ||])
              $$(PlutusTx.compile [|| wrap ||])
  where wrap = Scripts.wrapValidator @DatumSmith @RedeemerSmith
