{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Tx.Constraints
  ( module Cooked.Tx.Constraints.Type,
    module Cooked.Tx.Constraints.Pretty,
    LedgerConstraint,
    extractDatumStrFromConstraint,
    toLedgerConstraint,
    toLedgerConstraints,
    generateUnbalTx,
  )
where

import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map.Strict as M
import Data.Void
import qualified Ledger as Pl hiding (unspentOutputs)
import qualified Ledger.Constraints as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, validatorScript)
import qualified PlutusTx as Pl

-- * Converting 'Constraint's to 'Pl.ScriptLookups', 'Pl.TxConstraints' and '[Wallet]'

type LedgerConstraint a =
  (Pl.ScriptLookups a, Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a), [Wallet])

-- | Map from datum hashes to string representation of all the datum carried
extractDatumStrFromConstraint :: Constraint -> M.Map Pl.DatumHash String
extractDatumStrFromConstraint (PaysScript _validator datumsAndValues) =
  M.fromList
    . map ((\d -> (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData d, show d)) . fst)
    $ datumsAndValues
extractDatumStrFromConstraint (SpendsScript _validator _redeemer (_out, datum)) =
  M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData datum) (show datum)
extractDatumStrFromConstraint _ = M.empty

-- | Converts our constraint into a 'LedgerConstraint',
--  which later can be used to generate a transaction.
toLedgerConstraint :: Constraint -> LedgerConstraint a
toLedgerConstraint (SpendsScript v r ((oref, o), _a)) = (lkups, constr, mempty)
  where
    lkups =
      Pl.otherScript (Pl.validatorScript v)
        <> Pl.unspentOutputs (M.singleton oref o)
    constr = Pl.mustSpendScriptOutput oref (Pl.Redeemer $ Pl.toBuiltinData r)
toLedgerConstraint (PaysScript v outs) = (lkups, constr, mempty)
  where
    lkups = Pl.otherScript (Pl.validatorScript v)
    constr = mconcat $
      flip map outs $ \(d, val) ->
        Pl.mustPayToOtherScript
          (Pl.validatorHash $ Pl.validatorScript v)
          (Pl.Datum $ Pl.toBuiltinData d)
          val
toLedgerConstraint (PaysPK p v) = (mempty, Pl.mustPayToPubKey p v, mempty)
toLedgerConstraint (SpendsPK (oref, o)) = (lkups, constr, mempty)
  where
    lkups = Pl.unspentOutputs (M.singleton oref o)
    constr = Pl.mustSpendPubKeyOutput oref
toLedgerConstraint (Mints pols v) = (lkups, constr, mempty)
  where
    lkups = foldMap Pl.mintingPolicy pols
    constr = Pl.mustMintValue v
toLedgerConstraint (Before t) = (mempty, constr, mempty)
  where
    constr = Pl.mustValidateIn (Pl.to t)
toLedgerConstraint (After t) = (mempty, constr, mempty)
  where
    constr = Pl.mustValidateIn (Pl.from t)
toLedgerConstraint (ValidateIn r) = (mempty, Pl.mustValidateIn r, mempty)
toLedgerConstraint (SignedBy wals) = (mempty, mempty, wals)

toLedgerConstraints :: [Constraint] -> LedgerConstraint a
toLedgerConstraints cs = (mconcat lkups, mconcat constrs, mconcat wals)
  where
    (lkups, constrs, wals) = unzip3 $ map toLedgerConstraint cs

-- | Generates an unbalanced transaction from a skeleton; A
--  transaction is unbalanced whenever @inputs + mints != outputs + fees@.
--  In order to submit a transaction, it must be balanced, otherwise
--  we will see a @ValueNotPreserved@ error.
--
--  See "Cooked.Tx.Balance" for balancing capabilities or stick to
--  'generateTx', which generates /and/ balances a transaction.
generateUnbalTx :: TxSkel -> Either Pl.MkTxError (Pl.UnbalancedTx, [Wallet])
generateUnbalTx sk =
  let (lkups, constrs, wals) = toLedgerConstraints @Void $ txConstraints sk
   in let txUnsigned = Pl.mkTx lkups constrs
       in second (,txMainSigner sk : wals) txUnsigned
