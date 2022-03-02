{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Cooked.Tx.Constraints
  ( module Cooked.Tx.Constraints.Type,
    module Cooked.Tx.Constraints.Pretty,
    extractDatumStrFromTxSpec,
    txFromTxSpec,
  )
where

import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (singleton, unspentOutputs)
import qualified Ledger.Constraints as Pl
import qualified Ledger.Constraints.OffChain as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (validatorScript)
import qualified PlutusTx as Pl

-- | Map from datum hashes to string representation of all the datums carried.
-- We use this in order to display data to the use when testing. Its often
-- easier to read the original datatype that was placed into a UTxO
-- instead of its respective @toBuilinData@ image.
extractDatumStrFromTxSpec :: TxSpec -> M.Map Pl.DatumHash String
extractDatumStrFromTxSpec TxSpec {..} =
  M.unions
    [ M.unions (extractDatumStrFromSpending <$> txSpendings),
      M.unions (extractDatumStrFromPayment <$> txPayments),
      M.unions (extractDatumStrFromMinting <$> txMinting)
    ]
  where
    extractDatumStrFromMinting :: Minting -> M.Map Pl.DatumHash String
    extractDatumStrFromMinting (Mints datum _ _) =
      M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData datum) (show datum)

    extractDatumStrFromSpending :: Spending -> M.Map Pl.DatumHash String
    extractDatumStrFromSpending (SpendsScript _validator _redeemer (_out, datum)) =
      M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData datum) (show datum)
    extractDatumStrFromSpending _ = M.empty

    extractDatumStrFromPayment :: Payment -> M.Map Pl.DatumHash String
    extractDatumStrFromPayment (PaysScript _validator datum _v) =
      M.singleton (Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum) (show datum)
    extractDatumStrFromPayment (PaysPKWithDatum _pk _stak mdat _v) =
      maybe M.empty (\d -> M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData d) (show d)) mdat

spendingTxIn :: Spending -> Pl.TxIn
spendingTxIn (SpendsPK (txOutRef, _chainIndexTxOut)) =
  Pl.TxIn txOutRef (Just Pl.ConsumePublicKeyAddress)
spendingTxIn (SpendsScript validator redeemer ((txOutRef, _chainIndexTxOut), datum)) =
  Pl.TxIn
    txOutRef
    ( Just
        ( Pl.ConsumeScriptAddress
            (Pl.validatorScript validator)
            (Pl.Redeemer (Pl.toBuiltinData redeemer))
            (Pl.Datum (Pl.toBuiltinData datum))
        )
    )

paymentTxOut :: Payment -> Pl.TxOut
paymentTxOut (PaysScript validator datum value) =
  Pl.TxOut
    { Pl.txOutAddress =
        Pl.Address
          { Pl.addressCredential =
              Pl.ScriptCredential . Pl.validatorHash . Pl.validatorScript $ validator,
            -- NOTE scripts can have staking credential
            -- It is ok to leave it for now until we learn more about whether it is used in contracts
            Pl.addressStakingCredential = Nothing
          },
      Pl.txOutValue = value,
      Pl.txOutDatumHash = Just . Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum
    }
paymentTxOut (PaysPKWithDatum pkh mStakePkh mDatum value) =
  Pl.TxOut
    { Pl.txOutAddress =
        Pl.Address
          { Pl.addressCredential = Pl.PubKeyCredential pkh,
            -- TO CHECK Is this the expected pkh here?
            Pl.addressStakingCredential = Pl.StakingHash . Pl.PubKeyCredential . Pl.unStakePubKeyHash <$> mStakePkh
          },
      Pl.txOutValue = value,
      Pl.txOutDatumHash = Pl.datumHash . Pl.Datum . Pl.toBuiltinData <$> mDatum
    }

datumFromPayment :: Payment -> Maybe (Pl.DatumHash, Pl.Datum)
-- TODO There may be a datum in SpendsPK now
datumFromPayment (PaysPKWithDatum _ _ mTypedDatum _) = do
  datum <- Pl.Datum . Pl.toBuiltinData <$> mTypedDatum
  return (Pl.datumHash datum, datum)
datumFromPayment (PaysScript _ typedDatum _) =
  let datum = Pl.Datum (Pl.toBuiltinData typedDatum)
   in Just (Pl.datumHash datum, datum)

datumFromSpending :: Spending -> Maybe (Pl.DatumHash, Pl.Datum)
-- TODO There may be a datum in SpendsPK now
datumFromSpending (SpendsPK _) = Nothing
datumFromSpending (SpendsScript _ _ (_, typedDatum)) =
  let datum = Pl.Datum (Pl.toBuiltinData typedDatum)
   in Just (Pl.datumHash datum, datum)

redeemerFromMinting :: Integer -> Minting -> Maybe (Pl.RedeemerPtr, Pl.Redeemer)
redeemerFromMinting _ (Mints Nothing _ _) = Nothing
redeemerFromMinting index (Mints (Just redeemer) _ _) =
  Just
    ( Pl.RedeemerPtr Pl.Mint index,
      Pl.Redeemer (Pl.toBuiltinData redeemer)
    )

mintScriptsFromMinting :: Minting -> Set Pl.MintingPolicy
mintScriptsFromMinting (Mints _ mintingPolicies _) = Set.fromList mintingPolicies

valueFromMinting :: Minting -> Pl.Value
valueFromMinting (Mints _ _ value) = value

timeConstraintRange :: TimeConstraint -> Pl.POSIXTimeRange
timeConstraintRange (Before t) = Pl.to t
timeConstraintRange (After t) = Pl.from t
timeConstraintRange (ValidateIn trange) = trange

txFromTxSpec :: TxSpec -> Pl.UnbalancedTx
txFromTxSpec TxSpec {..} =
  Pl.UnbalancedTx
    { Pl.unBalancedTxTx =
        Pl.Tx
          { Pl.txInputs = Set.fromList (spendingTxIn <$> txSpendings),
            Pl.txCollateral = Set.empty,
            Pl.txOutputs = paymentTxOut <$> txPayments,
            Pl.txMint = mconcat (valueFromMinting <$> txMinting),
            Pl.txFee = mempty,
            Pl.txValidRange = Pl.always,
            Pl.txMintScripts = Set.unions $ mintScriptsFromMinting <$> txMinting,
            Pl.txSignatures = M.empty,
            Pl.txRedeemers =
              -- TODO Not sure how to compute the indexes
              -- This only works for redeemer-less minting
              M.singleton (Pl.RedeemerPtr Pl.Mint 0) (Pl.Redeemer . Pl.toBuiltinData $ ()),
            Pl.txData =
              M.fromList $
                mapMaybe datumFromSpending txSpendings <> mapMaybe datumFromPayment txPayments
          },
      -- TODO when is "PaymentPubKey" required?
      Pl.unBalancedTxRequiredSignatories = M.fromList $ (,Nothing) . Pl.PaymentPubKeyHash <$> txSignatories,
      -- TODO put all the utxos involved in the transaction?
      -- Either an error or all the utxos
      Pl.unBalancedTxUtxoIndex = undefined,
      Pl.unBalancedTxValidityTimeRange = maybe Pl.always timeConstraintRange txTimeConstraint
    }
