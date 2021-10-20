{-# LANGUAGE BangPatterns #-}
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
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fno-specialise               #-}
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
import           Data.Aeson            (FromJSON, ToJSON)
import           GHC.Generics          (Generic)
import qualified Ledger
import qualified Ledger.Ada            as Ada
import           Ledger.Contexts       (ScriptContext (..), TxInfo (..))
import qualified Ledger.Contexts       as Validation
import qualified Ledger.Typed.Scripts  as Scripts

-- The PlutusTx and its prelude provide the functions we can use for on-chain computations.
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (Applicative (..))
import qualified Plutus.V2.Ledger.Api  as Api

import           Schema                (ToSchema)

import qualified Prelude               as Haskell

-- |This multisig script will receive as a parameter the list of elligible signers
-- and the threshold number of signatures.
data Params = Params
  { pmspSignatories  :: [Ledger.PubKey]
  , pmspRequiredSigs :: Integer
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
PlutusTx.makeLift ''Params

-- |A Payment is a simple amount of Ada to be paid to a public key.
data Payment = Payment
  { paymentAmount    :: Integer
  , paymentRecipient :: Ledger.PubKeyHash
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''Payment

instance Eq Payment where
  {-# INLINABLE (==) #-}
  Payment a1 r1 == Payment a2 r2 = a1 == a2 && r1 == r2

-- |The datum is either a signature of a payment or
-- an accumulator collecting signatures for a given payment.
data Datum
  = Accumulator { payment :: Payment, signees :: [Ledger.PubKey] }
  | Sign { pk :: Ledger.PubKey, sig :: Ledger.Signature }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''Datum

instance Eq Datum where
  {-# INLINABLE (==) #-}
  Accumulator p1 ss1 == Accumulator p2 ss2 = p1 == p2 && ss1 == ss2
  Sign pkh1 s1 == Sign pkh2 s2             = pkh1 == pkh2 && s1 == s2
  _ == _ = False

-- |Finally, there is only one redeemer
type Redeemer = ()

-- We can't just use Data.Maybe; those functions are not inlinable
{-# INLINE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = traceError "fromJust: Nothing"

-- For some reason, we cannot find a builtin way of serializing values into
-- a BuiltinByteString; henre, we'll build it ourselves.
{-# INLINABLE packInteger #-}
packInteger :: Integer -> BuiltinByteString
packInteger k = if k < 0 then consByteString 1 (go (negate k) emptyByteString) else consByteString 0 (go k emptyByteString)
  where
    go n s
      | n == 0            = s
      | otherwise         = go (n `divide` 256) (consByteString (n `modulo` 256) s)

{-# INLINABLE packPayment #-}
packPayment :: Payment -> BuiltinByteString
packPayment (Payment amm bob) = consByteString 42 $ appendByteString (packInteger amm) (Ledger.getPubKeyHash bob)

{-# INLINABLE findDatumByHash #-}
findDatumByHash :: TxInfo -> Api.DatumHash -> Maybe Datum
findDatumByHash txInfo hash = do
  Ledger.Datum d <- Validation.findDatum hash txInfo
  PlutusTx.fromBuiltinData d

{-# INLINABLE findDatum #-}
findDatum :: TxInfo -> Validation.TxInInfo -> Maybe Datum
findDatum txInfo inInfo = Validation.txOutDatumHash (Validation.txInInfoResolved inInfo) >>= findDatumByHash txInfo

{-# INLINABLE validatePayment #-}
validatePayment :: Params -> Datum -> Redeemer -> ScriptContext -> Bool
-- |When adding a signature we need to ensure that there is a (single) Accumulator input.
-- Its validator will do the rest.
-- We also don't verify that the Sign's payment matches Accumulator's,
-- since that will be verified by the Accumulator's validator as well —
-- we just need to make sure it'll be run (which is precisely when we have an Accumulator input).
validatePayment _           Sign {} _ ctx = traceIfFalse "Should have only one Accumulator input"
                                              $ length [ () | Accumulator {} <- inputDatums ] == 1
  where
    txInfo = scriptContextTxInfo ctx
    inputDatums = mapMaybe (findDatum txInfo) $ txInfoInputs txInfo
-- |When validating the (singleton) Accumulator input, we need to make sure that
-- one of the two options hold:
-- 1. If there's not enough signatures:
--    the (single) output of the transaction doesn't transfer anything,
--    and the signatures are correctly appended to the accumulated list.
-- 2. If there are enough signatures:
--    the single output contains no datum (so the accumulated signatures are consumed),
--    and it transfers exactly as much as the Payment proclaims.
validatePayment Params {..} (Accumulator payment signees) _ ctx
  | length signees >= pmspRequiredSigs = validatePayout
  | otherwise = validateAcc
  where
    txInfo = scriptContextTxInfo ctx

    validatePayout
      | [Api.TxOut _ outVal Nothing] <- txInfoOutputs txInfo = outVal == Ada.lovelaceValueOf (paymentAmount payment)
      | otherwise = False

    validateAcc
      | [Api.TxOut outAddr outVal (Just dh)] <- txInfoOutputs txInfo
      , Just (Accumulator payment' signees') <- findDatumByHash txInfo dh = outVal == Ada.lovelaceValueOf 0
                                                                         && outAddr == Api.Address (Api.PubKeyCredential $ paymentRecipient payment) Nothing
                                                                         && payment == payment'
                                                                         && verifySignees signees'
      | otherwise = False

    verifySignees signees' = uniqueSignees' == signees'                     -- no duplicates in the output
                          && uniqueSignees' == nub (signees <> newSignees)  -- the new signatures set is the union of the existing sigs and the added ones
                          && allInputsRelevant                              -- no irrelevant inputs (like other accumulators or double signatures) are being consumed
      where
        uniqueSignees' = nub signees'

        otherInputs = mapMaybe (findDatum txInfo) $ filter (/= fromJust (Validation.findOwnInput ctx)) $ txInfoInputs txInfo
        newSignees = mapMaybe extractSig otherInputs
        allInputsRelevant = length newSignees == length otherInputs

        -- |Returns a signature from the datum if the signature is a valid one for this payment.
        extractSig Accumulator {} = Nothing
        extractSig (Sign pk s)
          | pk `notElem` pmspSignatories = Nothing                           -- Not a signatory — wrong
          | pk `elem` signees = Nothing                                      -- Already signed this payment — wrong
          | not $ verifySig pk (sha2_256 $ packPayment payment) s = Nothing  -- Sig verification failed — wrong
                                                                             -- Note that if it succeeds, than the payment this Sign is for necessarily matches
                                                                             -- the payment in the Accumulator, since `payment` comes from the Accumulator.
          | otherwise = Just pk                                              -- The above didn't get triggered, so presumably it's right

-- Here's a wrapper to verify a signature. It is important that the parameters to verifySignature
-- are, in order: pk, msg then signature.
{-# INLINABLE verifySig #-}
verifySig :: Ledger.PubKey -> BuiltinByteString -> Ledger.Signature -> Bool
verifySig pk msg s = verifySignature (Api.getLedgerBytes $ Ledger.getPubKey pk) msg (Ledger.getSignature s)

-- Finally, we wrap everything up and make the script available.

data PMultiSig
instance Scripts.ValidatorTypes PMultiSig where
  type instance RedeemerType PMultiSig = Redeemer
  type instance DatumType    PMultiSig = Datum

pmultisig :: Params -> Scripts.TypedValidator PMultiSig
pmultisig = Scripts.mkTypedValidatorParam @PMultiSig
              $$(PlutusTx.compile [|| validatePayment ||])
              $$(PlutusTx.compile [|| wrap ||])
  where wrap = Scripts.wrapValidator @Datum @Redeemer

pmultisigAddr :: Params -> Ledger.Address
pmultisigAddr = Ledger.scriptAddress . Scripts.validatorScript . pmultisig
