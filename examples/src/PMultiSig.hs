{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
module PMultiSig where

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Maybe                   (fromJust)
import           GHC.Generics                 (Generic)
import           Ledger                       (POSIXTime, PubKey, Signature, PubKeyHash, pubKeyHash)
import qualified Ledger
import           Ledger.Contexts              (ScriptContext (..), TxInfo (..))
import qualified Ledger.Contexts              as Validation
import qualified Ledger.Typed.Scripts         as Scripts
import qualified Ledger.Oracle                as Oracle
import           Ledger.Value                 (Value)

import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..))

import           Schema                       (ToSchema)

import qualified Prelude                      as Haskell
import Ledger.Typed.Scripts (TypedValidator)
import Ledger.Typed.Scripts.Validators (mkTypedValidatorParam)
import Ledger.Typed.Scripts.Validators (wrapValidator)
import Ledger.Typed.Scripts (validatorScript)

data Params = Params
  { pmspSignatories  :: [PubKey]
  , pmspRequiredSigs :: Integer
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
PlutusTx.makeLift ''Params

data Payment = Payment
  { paymentAmount    :: Value
  , paymentRecipient :: PubKeyHash
  , paymentDeadline  :: POSIXTime
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''Payment

instance Eq Payment where
  {-# INLINABLE (==) #-}
  Payment a1 r1 d1 == Payment a2 r2 d2 = a1 == a2
                                      && r1 == r2
                                      && d1 == d2

data Datum
  = Proposal Payment
  | Sign PubKeyHash Signature
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''Datum

instance Eq Datum where
  {-# INLINABLE (==) #-}
  Proposal p1 == Proposal p2 = p1 == p2
  Sign pkh1 s1 == Sign pkh2 s2 = pkh1 == pkh2
                              && s1 == s2
  _ == _ = False

type Redeemer = ()

{-# INLINABLE findDatum #-}
findDatum :: TxInfo -> Validation.TxInInfo -> Maybe Datum
findDatum txInfo inInfo = do
  hash <- Validation.txOutDatumHash (Validation.txInInfoResolved inInfo)
  Ledger.Datum d <- Validation.findDatum hash txInfo
  PlutusTx.fromBuiltinData d

{-# INLINABLE validatePayment #-}
validatePayment :: Params -> Datum -> Redeemer -> ScriptContext -> Bool
validatePayment Params {..} (Proposal p) _ ctx = length correctInputs >= pmspRequiredSigs
  where
    thisInput = fromJust $ Validation.findOwnInput ctx
    txInfo = scriptContextTxInfo ctx
    relevantInputs = filter (/= thisInput) $ txInfoInputs txInfo
    correctInputs = filter validateSignature $ nub $ mapMaybe (findDatum txInfo) relevantInputs

    validateSignature :: Datum -> Bool
    validateSignature (Proposal _) = False
    validateSignature (Sign pkh s)
      | [pk] <- filter ((== pkh) . pubKeyHash) pmspSignatories = verifySig s pk (Ledger.datumHash $ Ledger.Datum $ PlutusTx.toBuiltinData $ Proposal p)
      | otherwise = False
validatePayment _           (Sign _ _) _ ctx = any (isProposal . findDatum txInfo) $ txInfoInputs txInfo
  where
    txInfo = scriptContextTxInfo ctx
    isProposal (Just (Proposal _)) = True
    isProposal _ = False

{-# INLINABLE verifySig #-}
verifySig :: Signature -> PubKey -> Ledger.DatumHash -> Bool
verifySig s pk dh = isRight $ Oracle.checkSignature dh pk s

data PMultiSig
instance Scripts.ValidatorTypes PMultiSig where
  type instance RedeemerType PMultiSig = Redeemer
  type instance DatumType    PMultiSig = Datum

pmultisig :: Params -> TypedValidator PMultiSig
pmultisig = mkTypedValidatorParam @PMultiSig
              $$(PlutusTx.compile [|| validatePayment ||])
              $$(PlutusTx.compile [|| wrap ||])
  where wrap = wrapValidator @Datum @Redeemer

pmultisigAddr :: Params -> Ledger.Address
pmultisigAddr = Ledger.scriptAddress . validatorScript . pmultisig
