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
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

-- |
-- * Parallel stateful multi-sig contract
--
-- This is an example contract where we collect @n@ signatures
-- over a proposal, perhaps in several transactions,
-- then issue a given payment.
module PMultiSigStateful where

-- Here are the necessary imports. It is VERY IMPORTANT to refrain from using
-- anything that is not a simple accessor from Ledger; this will not
-- compile to PlutusCore.
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Ada as Ada
import Ledger.Contexts hiding (findDatum)
import qualified Ledger.Contexts as Validation
import qualified Ledger.Typed.Scripts as Scripts

-- The PlutusTx and its prelude provide the functions we can use for on-chain computations.

import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))
import Schema (ToSchema)
import qualified Prelude as Haskell

-- | This multisig script will receive as a parameter the list of elligible signers
--  and the threshold number of signatures. Moreover, we pass the threadToken NFT
--  that will identify legitimate datums as a parameter, as this will only be known at run-time.
data Params = Params
  { pmspSignatories :: [Ledger.PubKey]
  , pmspRequiredSigs :: Integer
  , pmspThreadToken :: Value.AssetClass
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

-- | The datum is either a signature of a payment or
--  an accumulator collecting signatures for a given payment.
data Datum
  = Accumulator {payment :: Payment, signees :: [Ledger.PubKey]}
  | Sign {signPk :: Ledger.PubKey, signSignature :: Ledger.Signature}
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Datum

instance Eq Datum where
  {-# INLINEABLE (==) #-}
  Accumulator p1 ss1 == Accumulator p2 ss2 = p1 == p2 && ss1 == ss2
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

{-# INLINEABLE findDatumByHash #-}
findDatumByHash :: TxInfo -> Api.DatumHash -> Maybe Datum
findDatumByHash txInfo hash = do
  Ledger.Datum d <- Validation.findDatum hash txInfo
  PlutusTx.fromBuiltinData d

{-# INLINEABLE findDatum #-}
findDatum :: TxInfo -> Validation.TxInInfo -> Maybe Datum
findDatum txInfo inInfo = Validation.txOutDatumHash (Validation.txInInfoResolved inInfo) >>= findDatumByHash txInfo

{-# INLINEABLE findAccumulators #-}
findAccumulators :: TxInfo -> [(Datum, Api.Value)]
findAccumulators txInfo = mapMaybe toAcc $ txInfoOutputs txInfo
  where
    toAcc (Api.TxOut _ outVal (Just dh))
      | Just acc@Accumulator {} <- findDatumByHash txInfo dh = Just (acc, outVal)
    toAcc _ = Nothing

{-# INLINEABLE findPaymentsToAddr #-}
findPaymentsToAddr :: Api.Address -> TxInfo -> [Api.TxOut]
findPaymentsToAddr addr = filter (\(Api.TxOut outAddr _ _) -> outAddr == addr) . txInfoOutputs

{-# INLINEABLE validatePayment #-}
validatePayment :: Params -> Datum -> Redeemer -> ScriptContext -> Bool

-- | When adding a signature we need to ensure that there is a (single) Accumulator input.
--  Its validator will do the rest.
--  We also don't verify that the Sign's payment matches Accumulator's,
--  since that will be verified by the Accumulator's validator as well --
--  we just need to make sure it'll be run (which is precisely when we have an Accumulator input).
validatePayment _ Sign {} _ ctx =
  traceIfFalse "Should have only one Accumulator input" $
    length [() | Accumulator {} <- inputDatums] == 1
  where
    txInfo = scriptContextTxInfo ctx
    inputDatums = mapMaybe (findDatum txInfo) $ txInfoInputs txInfo
validatePayment Params {..} (Accumulator payment signees) _ ctx
  | length signees >= pmspRequiredSigs = validatePayout
  | otherwise = validateAcc
  where
    (provTokSym, provTokName) = Value.unAssetClass pmspThreadToken

    txInfo = scriptContextTxInfo ctx

    validatePayout
      | [Api.TxOut _ outVal Nothing] <- findPaymentsToAddr paymentAddr txInfo =
        outVal == Ada.lovelaceValueOf (paymentAmount payment)
          -- /\ This also ensures there is no token in the output
          && verifyInAccThreadToken True
      | otherwise = False
      where
        paymentAddr = Api.Address (Api.PubKeyCredential $ paymentRecipient payment) Nothing

    validateAcc
      | [(Accumulator payment' signees', outVal)] <- findAccumulators txInfo =
        Value.valueOf outVal Api.adaSymbol Api.adaToken == paymentAmount payment
          && Value.valueOf outVal provTokSym provTokName == 1
          && verifyInAccThreadToken (not $ null signees')
          && payment == payment'
          && verifySignees signees'
      | otherwise = False

    verifyInAccThreadToken shallExist = inputTokens == if shallExist then 1 else 0
      where
        inputTokens = sum $ tokenVal <$> txInfoInputs txInfo
        tokenVal :: Api.TxInInfo -> Integer
        tokenVal (Api.TxInInfo _ (Api.TxOut _ outVal _)) = Value.valueOf outVal provTokSym provTokName

    verifySignees signees' =
      uniqueSignees' == signees' -- no duplicates in the output
        && uniqueSignees' == nub (signees <> newSignees) -- the new signatures set is the union of the existing sigs and the added ones
        && allInputsRelevant -- no irrelevant inputs (like other accumulators or double signatures) are being consumed
      where
        uniqueSignees' = nub signees'

        otherInputs = mapMaybe (findDatum txInfo) $ filter (/= fromJust (Validation.findOwnInput ctx)) $ txInfoInputs txInfo
        newSignees = mapMaybe extractSig otherInputs
        allInputsRelevant = length newSignees == length otherInputs

        extractSig Accumulator {} = Nothing
        extractSig (Sign signPk s)
          | signPk `notElem` pmspSignatories = Nothing -- Not a signatory -- wrong
          | signPk `elem` signees = Nothing -- Already signed this payment -- wrong
          | not $ verifySig signPk (sha2_256 $ packPayment payment) s = Nothing -- Sig verification failed -- wrong
          -- Note that if it succeeds, than the payment this Sign is for necessarily matches
          -- the payment in the Accumulator, since `payment` comes from the Accumulator.
          | otherwise = Just signPk -- The above didn't get triggered, so presumably it's right

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

{-# INLINEABLE pmultisig #-}
pmultisig :: Params -> Scripts.TypedValidator PMultiSig
pmultisig =
  Scripts.mkTypedValidatorParam @PMultiSig
    $$(PlutusTx.compile [||validatePayment||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Datum @Redeemer

{-# INLINEABLE pmultisigAddr #-}
pmultisigAddr :: Params -> Ledger.Address
pmultisigAddr = Ledger.scriptAddress . Scripts.validatorScript . pmultisig

-- * Minting Policy

-- $mintingpolicy
--
-- 'mkPolicy' below is a minting policy for an NFT. This was taken from the plutus-pioneer-program
-- in: https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week5.html#nfts
--
-- The general idea is to parameterize the policy with an 'Api.TxOutRef', and make
-- it so that this currency can only be minted in a transaction consuming a given 'Api.TxOutRef'.
-- Since a 'TxOut' can only be consumed once, no other transaction will be able to
-- consume the same TxOutRef and hence no more tokens can be minted.
--
-- TODO: modify to allow burning of the token; a transaction that consumes and output

{-# INLINEABLE mkPolicy #-}
mkPolicy :: (Api.TxOutRef, Value.TokenName) -> () -> ScriptContext -> Bool
mkPolicy (oref, tn) _ ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
      [(cs, tn', amt)] -> cs == ownCurrencySymbol ctx && tn' == tn && amt == 1
      _ -> False

{-# INLINEABLE threadTokenSymbol #-}
threadTokenSymbol :: Api.TxOutRef -> Value.TokenName -> Api.CurrencySymbol
threadTokenSymbol oref = Validation.scriptCurrencySymbol . threadTokenPolicy oref

{-# INLINEABLE threadTokenName #-}
threadTokenName :: Value.TokenName
threadTokenName = Value.tokenName "threadToken"

{-# INLINEABLE threadTokenAssetClass #-}
threadTokenAssetClass :: Api.TxOutRef -> Value.AssetClass
threadTokenAssetClass oref = Value.assetClass (threadTokenSymbol oref threadTokenName) threadTokenName

{-# INLINEABLE threadTokenPolicy #-}
threadTokenPolicy :: Api.TxOutRef -> Value.TokenName -> Scripts.MintingPolicy
threadTokenPolicy oref tok =
  Api.mkMintingPolicyScript $
    $$(PlutusTx.compile [||\x y -> Scripts.wrapMintingPolicy $ mkPolicy (x, y)||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode tok
