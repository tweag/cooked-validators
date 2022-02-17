{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cooked.Tx.Constraints
  ( module Cooked.Tx.Constraints.Type,
    module Cooked.Tx.Constraints.Pretty,
    LedgerConstraint,
    extractDatumStrFromConstraint,
    signedByWallets,
    toLedgerConstraint,
    toLedgerConstraints,
  )
where

import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import qualified Data.Map.Strict as M
import qualified Ledger as Pl hiding (singleton, unspentOutputs)
import qualified Ledger.Constraints as Pl
import qualified Ledger.Constraints.TxConstraints as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, validatorScript)
import qualified PlutusTx as Pl

-- * Converting 'Constraint's to 'Pl.ScriptLookups', 'Pl.TxConstraints'

type LedgerConstraint a =
  (Pl.ScriptLookups a, Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a))

-- | Map from datum hashes to string representation of all the datums carried.
-- We use this in order to display data to the use when testing. Its often
-- easier to read the original datatype that was placed into a UTxO
-- instead of its respective @toBuilinData@ image.
extractDatumStrFromConstraint :: Constraint -> M.Map Pl.DatumHash String
extractDatumStrFromConstraint (PaysScript _validator datumsAndValues) =
  M.fromList
    . map ((\d -> (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData d, show d)) . fst)
    $ datumsAndValues
extractDatumStrFromConstraint (SpendsScript _validator _redeemer (_out, datum)) =
  M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData datum) (show datum)
extractDatumStrFromConstraint (PaysPKWithDatum _pk _stak mdat _v) =
  maybe M.empty (\d -> M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData d) (show d)) mdat
extractDatumStrFromConstraint _ = M.empty

-- | Converts our constraint into a 'LedgerConstraint',
--  which later can be used to generate a transaction. The universally
--  quantified type-variable is there on purpose, to enable us to
--  easily spend from multiple scripts at the same time.
toLedgerConstraint :: Constraint -> LedgerConstraint a
toLedgerConstraint (SpendsScript v r ((oref, o), _a)) = (lkups, constr)
  where
    lkups =
      Pl.otherScript (Pl.validatorScript v)
        <> Pl.unspentOutputs (M.singleton oref o)
    constr = Pl.mustSpendScriptOutput oref (Pl.Redeemer $ Pl.toBuiltinData r)
toLedgerConstraint (PaysScript v outs) = (lkups, constr)
  where
    lkups = Pl.otherScript (Pl.validatorScript v)
    constr = mconcat $
      flip map outs $ \(d, val) ->
        Pl.mustPayToOtherScript
          (Pl.validatorHash $ Pl.validatorScript v)
          (Pl.Datum $ Pl.toBuiltinData d)
          val
toLedgerConstraint (PaysPKWithDatum p stak dat v) = (lkups, constr)
  where
    mData = fmap (Pl.Datum . Pl.toBuiltinData) dat

    lkups =
      maybe mempty Pl.otherData mData
        -- TODO: do we want to akk ownStakePubKeyHash on 'PaysPKWithDatum'? Would we rather have
        -- a different 'WithOwnStakePubKeyHash' constraint?
        <> maybe mempty Pl.ownStakePubKeyHash stak
    constr = Pl.singleton $ Pl.MustPayToPubKeyAddress (Pl.PaymentPubKeyHash p) stak mData v
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

-- | Converts a list of constraints into a 'LedgerConstraint'
toLedgerConstraints :: [Constraint] -> LedgerConstraint a
toLedgerConstraints cs = (mconcat lkups, mconcat constrs)
  where
    (lkups, constrs) = unzip $ map toLedgerConstraint cs

-- | @signedByWallets ws == SignedBy $ map walletPKHash ws@
signedByWallets :: [Wallet] -> Constraint
signedByWallets = SignedBy . map walletPKHash
