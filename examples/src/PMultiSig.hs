{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- |
-- * Parallel multi-sig contract
--
-- This is an example contract where we collect @n@ signatures
-- over a proposal, then issue a given payment. Lots of simplifying
-- assumptions have been made to make this as simple as possible
-- without being trivial.
module PMultiSig where

-- Here are the necessary imports. It is VERY IMPORTANT to refrain from using
-- anything that is not a simple accessor from Ledger; this will not
-- compile to PlutusCore.
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Ada as Ada
import Ledger.Contexts (ScriptContext (..), TxInfo (..))
import qualified Ledger.Contexts as Validation
import qualified Ledger.Typed.Scripts as Scripts

-- The PlutusTx and its prelude provide the functions we can use for on-chain computations.

import qualified Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))
import Schema (ToSchema)
import qualified Prelude as Haskell

-- | This multisig script will receive as a parameter the list of elligible signers
--  and the threshold number of signatures.
data Params = Params
  { pmspSignatories :: [Ledger.PubKey]
  , pmspRequiredSigs :: Integer
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Params

-- | A Payment is a simple amount of Ada to be paid to a public key.
data Payment = Payment
  { paymentAmount :: Integer
  , paymentRecipient :: Ledger.PubKeyHash
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Payment

instance Eq Payment where
  {-# INLINEABLE (==) #-}
  Payment a1 r1 == Payment a2 r2 = a1 == a2 && r1 == r2

-- | The datums we will hold consist in a proposal and a signature of
--  said proposal.
data Datum
  = Proposal Payment
  | Sign {pk :: Ledger.PubKey, sig :: Ledger.Signature}
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Datum

instance Eq Datum where
  {-# INLINEABLE (==) #-}
  Proposal p1 == Proposal p2 = p1 == p2
  Sign pkh1 s1 == Sign pkh2 s2 = pkh1 == pkh2 && s1 == s2
  _ == _ = False

-- | Finally, there is only one redeemer
type Redeemer = ()

-- We can't just use Data.Maybe; those functions are not inlinable
{-# INLINE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = traceError "fromJust: Nothing"

-- For some reason, we cannot find a builtin way of serializing values into
-- a BuiltinByteString; henre, we'll build it ourselves.
{-# INLINEABLE packInteger #-}
packInteger :: Integer -> BuiltinByteString
packInteger k = if k < 0 then consByteString 1 (go (negate k) emptyByteString) else consByteString 0 (go k emptyByteString)
  where
    go n s
      | n == 0 = s
      | otherwise = go (n `divide` 256) (consByteString (n `modulo` 256) s)

{-# INLINEABLE packPayment #-}
packPayment :: Payment -> BuiltinByteString
packPayment (Payment amm bob) = consByteString 42 $ appendByteString (packInteger amm) (Ledger.getPubKeyHash bob)

{-# INLINEABLE findDatum #-}
findDatum :: TxInfo -> Validation.TxInInfo -> Maybe Datum
findDatum txInfo inInfo = do
  hash <- Validation.txOutDatumHash (Validation.txInInfoResolved inInfo)
  Ledger.Datum d <- Validation.findDatum hash txInfo
  PlutusTx.fromBuiltinData d

-- The semantics of our contract is simple; whenever one tries to redeem the output
-- containing a 'Proposal'; we'll make sure to check the existence of enough other
-- inputs containing signatures.
{-# INLINEABLE validatePayment #-}
validatePayment :: Params -> Datum -> Redeemer -> ScriptContext -> Bool
validatePayment Params {..} (Proposal p) _ ctx =
  and
    [ traceIfFalse
        "Not enough correct inputs"
        (length correctInputs >= pmspRequiredSigs)
    , traceIfFalse
        "Does not pay recipient"
        (any validPayment $ txInfoOutputs txInfo)
    ]
  where
    -- returns the input redeeming the output containing the `Proposal` datum
    -- we're evaluating validatePayment for,
    thisInput = fromJust $ Validation.findOwnInput ctx

    txInfo = scriptContextTxInfo ctx

    -- Now look for the inputs which are not thisInput
    relevantInputs = filter (/= thisInput) $ txInfoInputs txInfo

    -- The "correct" inputs are those that contain a valid signature for the proposal
    correctInputs = filter validateSignature $ nub $ mapMaybe (findDatum txInfo) relevantInputs
    validateSignature :: Datum -> Bool
    validateSignature (Proposal _) = False
    validateSignature (Sign pk s)
      | pk `elem` pmspSignatories = verifySig pk (sha2_256 $ packPayment p) s
      | otherwise = False

    -- Also, the transaction must execute the payment, so we'll have a predicate over TxOut's
    validPayment :: Api.TxOut -> Bool
    validPayment (Api.TxOut addr val _) =
      addr == Api.Address (Api.PubKeyCredential (paymentRecipient p)) Nothing
        && val == Ada.lovelaceValueOf (paymentAmount p)

-- When redeeming a 'Sign' datum, we just care that there is a proposal also being redeemed
-- in the same transaction.
validatePayment _ (Sign _ _) _ ctx =
  traceIfFalse "No Proposal present" $
    any (isProposal . findDatum txInfo) (txInfoInputs txInfo)
  where
    txInfo = scriptContextTxInfo ctx
    isProposal (Just (Proposal _)) = True
    isProposal _ = False

-- Here's a wrapper to verify a signature. It is important that the parameters to verifySignature
-- are, in order: pk, msg then signature.
{-# INLINEABLE verifySig #-}
verifySig :: Ledger.PubKey -> BuiltinByteString -> Ledger.Signature -> Bool
verifySig pk msg s = verifySignature (Api.getLedgerBytes $ Ledger.getPubKey pk) msg (Ledger.getSignature s)

-- Finally, we wrap everything up and make the script available.

data PMultiSig

instance Scripts.ValidatorTypes PMultiSig where
  type RedeemerType PMultiSig = Redeemer
  type DatumType PMultiSig = Datum

pmultisig :: Params -> Scripts.TypedValidator PMultiSig
pmultisig =
  Scripts.mkTypedValidatorParam @PMultiSig
    $$(PlutusTx.compile [||validatePayment||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Datum @Redeemer

pmultisigAddr :: Params -> Ledger.Address
pmultisigAddr = Ledger.scriptAddress . Scripts.validatorScript . pmultisig
