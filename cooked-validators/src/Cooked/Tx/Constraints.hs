{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cooked.Tx.Constraints
  ( module Cooked.Tx.Constraints.Type,
    module Cooked.Tx.Constraints.Pretty,
    LedgerConstraint,
    extractDatumStr,
    signedByWallets,
    toLedgerConstraint,
    orderTxOutputs,
  )
where

import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Ledger as Pl hiding (singleton, unspentOutputs)
import qualified Ledger.Constraints as Pl
import qualified Ledger.Constraints.TxConstraints as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, validatorScript)
import qualified PlutusTx as Pl

-- * Converting 'Constraint's to 'Pl.ScriptLookups', 'Pl.TxConstraints'

type LedgerConstraint a =
  (Pl.ScriptLookups a, Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a))

-- | Class for common operations on output and misc constraints.
class Constraint constraint where
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

instance Constraint MiscConstraint where
  extractDatumStr (SpendsScript _validator _redeemer (_out, datum)) =
    M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData datum) (show datum)
  extractDatumStr _ = M.empty

  toLedgerConstraint (SpendsScript v r ((oref, o), _a)) = (lkups, constr)
    where
      lkups =
        Pl.otherScript (Pl.validatorScript v)
          <> Pl.unspentOutputs (M.singleton oref o)
      constr = Pl.mustSpendScriptOutput oref (Pl.Redeemer $ Pl.toBuiltinData r)
  toLedgerConstraint (SpendsPK (oref, o)) = (lkups, constr)
    where
      lkups = Pl.unspentOutputs (M.singleton oref o)
      constr = Pl.mustSpendPubKeyOutput oref
  toLedgerConstraint (Mints Nothing pols v) = (lkups, constr)
    where
      lkups = foldMap Pl.mintingPolicy pols
      constr = Pl.mustMintValue v
  toLedgerConstraint (Mints (Just r) pols v) = (lkups, constr)
    where
      lkups = foldMap Pl.mintingPolicy pols
      constr = Pl.mustMintValueWithRedeemer (Pl.Redeemer (Pl.toBuiltinData r)) v
  toLedgerConstraint (Before t) = (mempty, constr)
    where
      constr = Pl.mustValidateIn (Pl.to t)
  toLedgerConstraint (After t) = (mempty, constr)
    where
      constr = Pl.mustValidateIn (Pl.from t)
  toLedgerConstraint (ValidateIn r) = (mempty, Pl.mustValidateIn r)
  toLedgerConstraint (SignedBy hashes) = (mempty, foldMap (Pl.mustBeSignedBy . Pl.PaymentPubKeyHash) hashes)

instance Constraint OutConstraint where
  extractDatumStr (PaysScript _validator datumsAndValues) =
    M.fromList
      . map ((\d -> (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData d, show d)) . fst)
      $ datumsAndValues
  extractDatumStr (PaysPKWithDatum _pk _stak mdat _v) =
    maybe M.empty (\d -> M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData d) (show d)) mdat

  toLedgerConstraint (PaysPKWithDatum p stak dat v) = (lkups, constr)
    where
      mData = fmap (Pl.Datum . Pl.toBuiltinData) dat

      lkups =
        maybe mempty Pl.otherData mData
          -- TODO: do we want to akk ownStakePubKeyHash on 'PaysPKWithDatum'? Would we rather have
          -- a different 'WithOwnStakePubKeyHash' constraint?
          <> maybe mempty Pl.ownStakePubKeyHash stak
      constr = Pl.singleton $ Pl.MustPayToPubKeyAddress (Pl.PaymentPubKeyHash p) stak mData v
  toLedgerConstraint (PaysScript v outs) = (lkups, constr)
    where
      lkups = Pl.otherScript (Pl.validatorScript v)
      constr = mconcat $
        flip map outs $ \(d, val) ->
          Pl.mustPayToOtherScript
            (Pl.validatorHash $ Pl.validatorScript v)
            (Pl.Datum $ Pl.toBuiltinData d)
            val

instance Constraint Constraints where
  extractDatumStr (miscConstraints :=>: outConstraints) =
    M.union
      (M.unions (extractDatumStr <$> miscConstraints))
      (M.unions (extractDatumStr <$> outConstraints))

  toLedgerConstraint (miscConstraints :=>: outConstraints) =
    (mconcat lkups, mconcat constrs)
    where
      (lkups, constrs) =
        unzip $
          (toLedgerConstraint <$> miscConstraints)
            <> (toLedgerConstraint <$> outConstraints)

-- | Look for the 'Pl.TxOut' corresponding to an output constraint
-- 'OutConstraint' among the outputs of a transaction.
findGeneratedTxOutput :: OutConstraint -> [Pl.TxOut] -> Maybe Pl.TxOut
-- TODO Also check staking credentials
findGeneratedTxOutput (PaysPKWithDatum targetPkh _mTargetStakePkh mTargetDatum targetVal) =
  List.find aux
  where
    aux :: Pl.TxOut -> Bool
    aux (Pl.TxOut (Pl.Address (Pl.PubKeyCredential pkh) _mStakeCredential) val mDatumHash) =
      pkh == targetPkh
        && val == targetVal
        && mDatumHash == (Pl.datumHash . Pl.Datum . Pl.toBuiltinData <$> mTargetDatum)
    aux _ = False
findGeneratedTxOutput (PaysScript targetTypedValidator [(targetTypedDatum, targetVal)]) =
  List.find aux
  where
    aux :: Pl.TxOut -> Bool
    aux (Pl.TxOut (Pl.Address (Pl.ScriptCredential validatorHash) _) val (Just datumHash)) =
      validatorHash == Pl.validatorHash (Pl.validatorScript targetTypedValidator)
        && val == targetVal
        && datumHash == (Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ targetTypedDatum)
    aux _ = False
findGeneratedTxOutput (PaysScript _ _) = const Nothing

-- | Reorders the outputs of a transaction according to the ordered list of
-- output constraints that generate them. Fails in case of mismatch. The
-- reordered outputs are put at the beginning of the list.
orderTxOutputs :: MonadFail m => [OutConstraint] -> [Pl.TxOut] -> m [Pl.TxOut]
-- TODO Check staking credentials
orderTxOutputs [] txOuts = return txOuts
orderTxOutputs (oc : ocs) txOuts =
  case findGeneratedTxOutput oc txOuts of
    Nothing ->
      fail $
        "Could not locate output corresponding to constraint "
          <> show (prettyOutConstraint oc)
    Just txOut -> (txOut :) <$> orderTxOutputs ocs (List.delete txOut txOuts)

-- | @signedByWallets ws == SignedBy $ map walletPKHash ws@
signedByWallets :: [Wallet] -> MiscConstraint
signedByWallets = SignedBy . map walletPKHash
