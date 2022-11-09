{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cooked.Tx.Constraints where

import Cooked.Tx.Constraints.Type
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (singleton, unspentOutputs)
import qualified Ledger.Constraints as Pl
import qualified Ledger.Constraints.TxConstraints as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Scripts as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, validatorScript)
import qualified Ledger.Value as Pl hiding (singleton)
import qualified Plutus.Script.Utils.V1.Scripts as Pl
import qualified PlutusTx as Pl

-- * Converting 'Constraint's to 'Pl.ScriptLookups', 'Pl.TxConstraints'

type LedgerConstraint a =
  (Pl.ScriptLookups a, Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a))

-- | Convenience class for common operations on what can be converted to
-- 'LedgerConstraint' (in other words native Plutus `Pl.TxConstraints`). This
-- covers output constraints 'OutConstraint', miscelaneous constraints
-- 'MiscConstraint', and combinations of both within 'Constraints'.
--
-- As a user, you should not have to deal with or worry about this class and
-- stick with the 'ConstraintsSpec' instances to specify constraints in
-- transaction skeletons 'TxSkel'.
class ToLedgerConstraint constraint where
  -- | Map from datum hashes to string representation of all the datums carried.
  -- We use this in order to display data to the use when testing. Its often
  -- easier to read the original datatype that was placed into a UTxO
  -- instead of its respective @toBuilinData@ image.
  extractDatumStr :: constraint -> M.Map Pl.DatumHash String

  -- | Converts our constraint into a 'LedgerConstraint',
  --  which later can be used to generate a transaction. The universally
  --  quantified type-variable is there on purpose, to enable us to
  --  easily spend from multiple scripts at the same time.
  toLedgerConstraint :: constraint -> LedgerConstraint a

instance ToLedgerConstraint MintsConstraint where
  extractDatumStr _ = M.empty

  toLedgerConstraint (MintsConstraint Nothing pol tName amount) = (lkups, constr)
    where
      lkups = Pl.mintingPolicy pol
      constr =
        Pl.mustMintValue $
          Pl.assetClassValue (Pl.assetClass (Pl.mpsSymbol . Pl.mintingPolicyHash $ pol) tName) amount
  toLedgerConstraint (MintsConstraint (Just red) pol tName amount) = (lkups, constr)
    where
      lkups = Pl.mintingPolicy pol
      constr =
        Pl.mustMintValueWithRedeemer (Pl.Redeemer (Pl.toBuiltinData red)) $
          Pl.assetClassValue (Pl.assetClass (Pl.mpsSymbol . Pl.mintingPolicyHash $ pol) tName) amount

instance ToLedgerConstraint InConstraint where
  extractDatumStr (SpendsScript _ _ spOut) = case spOutDatum spOut of
    Just d -> M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData d) (show d)
    Nothing -> M.empty
  extractDatumStr SpendsPK {} = M.empty -- is this accurate? There might be a datum on PK outputs! TODO

  toLedgerConstraint (SpendsScript v r spOut) = (lkups, constr)
    where
      oref = spOutTxOutRef spOut
      o = spOutCITxOut spOut
      lkups =
        Pl.otherScript (Pl.validatorScript v)
          <> Pl.unspentOutputs (M.singleton oref o)
      constr = Pl.mustSpendScriptOutput oref (Pl.Redeemer $ Pl.toBuiltinData r)
  toLedgerConstraint (SpendsPK spOut) = (lkups, constr)
    where
      oref = spOutTxOutRef spOut
      o = spOutCITxOut spOut
      lkups = Pl.unspentOutputs (M.singleton oref o)
      constr = Pl.mustSpendPubKeyOutput oref

instance ToLedgerConstraint OutConstraint where
  extractDatumStr (PaysScript _validator _stak datum _value) =
    M.singleton (Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum) (show datum)
  extractDatumStr (PaysPK _pk _stak mdat _v) =
    maybe M.empty (\d -> M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData d) (show d)) mdat

  toLedgerConstraint (PaysPK p stak dat v) = (lkups, constr)
    where
      mData = fmap (Pl.Datum . Pl.toBuiltinData) dat

      lkups =
        maybe mempty Pl.otherData mData
          -- TODO: do we want to add ownStakePubKeyHash on 'PaysPKWithDatum'? Would we rather have
          -- a different 'WithOwnStakePubKeyHash' constraint?
          <> maybe mempty Pl.ownStakePubKeyHash stak
      constr = Pl.singleton $ Pl.MustPayToPubKeyAddress (Pl.PaymentPubKeyHash p) stak mData v
  toLedgerConstraint (PaysScript v msc datum value) = (lkups, constr)
    where
      lkups = Pl.otherScript (Pl.validatorScript v)
      constr =
        Pl.singleton
          ( Pl.MustPayToOtherScript
              (Pl.validatorHash $ Pl.validatorScript v)
              (msc >>= getStakeValidatorHash)
              (Pl.Datum $ Pl.toBuiltinData datum)
              value
          )
          <> Pl.singleton (Pl.MustIncludeDatum $ Pl.Datum $ Pl.toBuiltinData datum)
      -- Retrieve StakeValidatorHash from StakingCredential. This is similar to what plutus-apps does.
      -- See lines 141-146 in plutus-apps/plutus-ledger-constraints/test/Spec.hs (commit hash 02ae267)
      getStakeValidatorHash :: Pl.StakingCredential -> Maybe Pl.StakeValidatorHash
      getStakeValidatorHash (Pl.StakingHash (Pl.ScriptCredential (Pl.ValidatorHash svh))) =
        Just $ Pl.StakeValidatorHash svh
      getStakeValidatorHash _ = Nothing

instance ToLedgerConstraint TxSkel where
  extractDatumStr (TxSkel _label _opts _mints _validityRange ins outs) =
    foldMap extractDatumStr ins <> foldMap extractDatumStr outs

  toLedgerConstraint (TxSkel _label _opts mints validityRange ins outs) =
    (mconcat lkups, mconcat constrs)
    where
      (lkups, constrs) =
        unzip $
          (toLedgerConstraint <$> Set.toList ins)
            <> (toLedgerConstraint <$> outs)
            <> (toLedgerConstraint <$> Set.toList mints)
            <> [(mempty, Pl.mustValidateIn validityRange)]

-- -- | Generate the 'Pl.TxOut' transaction output associated to a given output
-- -- constraint 'OutConstraint'.
-- outConstraintToTxOut :: OutConstraint -> Pl.TxOut
-- outConstraintToTxOut (PaysPKWithDatum pkh mStakePkh mDatum value) =
--   Pl.TxOut
--     { Pl.txOutAddress =
--         Pl.Address
--           (Pl.PubKeyCredential pkh)
--           (Pl.StakingHash . Pl.PubKeyCredential . Pl.unStakePubKeyHash <$> mStakePkh),
--       Pl.txOutValue = value,
--       Pl.txOutDatumHash = Pl.datumHash . Pl.Datum . Pl.toBuiltinData <$> mDatum
--     }
-- outConstraintToTxOut (PaysScript validator msc datum value) =
--   let outAddr = appendStakingCredential msc $ Pl.scriptHashAddress $ Pl.validatorHash $ Pl.validatorScript validator
--    in Pl.TxOut
--         { Pl.txOutAddress = outAddr,
--           Pl.txOutValue = value,
--           Pl.txOutDatumHash = Just . Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum
--         }
--   where
--     appendStakingCredential :: Maybe Pl.StakingCredential -> Pl.Address -> Pl.Address
--     appendStakingCredential Nothing addr = addr
--     appendStakingCredential (Just sc) addr = addr {Pl.addressStakingCredential = Just sc}

-- -- | Reorders the outputs of a transaction according to the ordered list of
-- -- output constraints that generate them. Fails in case of mismatch. The
-- -- reordered outputs are put at the beginning of the list.
-- -- orderTxOutputs :: MonadFail m => [OutConstraint] -> [Pl.TxOut] -> m [Pl.TxOut]
-- -- -- TODO Check staking credentials
-- -- orderTxOutputs [] txOuts = return txOuts
-- -- orderTxOutputs (oc : ocs) txOuts =
-- --   case findGeneratedTxOutput oc txOuts of
-- --     Nothing ->
-- --       fail $
-- --         "Could not locate output corresponding to constraint "
-- --           <> show (prettyOutConstraint oc)
-- --     Just txOut -> (txOut :) <$> orderTxOutputs ocs (List.delete txOut txOuts)
-- orderTxOutputs :: [OutConstraint] -> [Pl.TxOut] -> [Pl.TxOut]
-- orderTxOutputs expected given =
--   let res = map outConstraintToTxOut expected
--    in -- TODO: this should just be `res ++ (given List.\\ res)`. However, there is a bug
--       -- in plutus-apps where the StakingCredential is erased. Thus, we need to do this
--       -- custom subtraction.
--       res ++ List.deleteFirstsBy ((==) `on` Pl.addressCredential . Pl.txOutAddress) given res

-- -- | @signedByWallets ws == SignedBy $ map walletPKHash ws@
-- signedByWallets :: [Wallet] -> MiscConstraint
-- signedByWallets = SignedBy . map walletPKHash
