{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Tx.Constraints where

import Cooked.MockChain
import Cooked.Tx.Balance
import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map.Strict as M
import Data.Void
import qualified Ledger as Pl hiding (unspentOutputs)
import qualified Ledger.Constraints as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, TypedValidator, validatorScript)
import qualified PlutusTx as Pl

-- | Our own first class constraint type. The advantage over the regular plutus constraint
--  type is that we get to add whatever we need and we hide away the type variables in existentials.
data Constraint where
  PaysScript ::
    (Pl.ToData (Pl.DatumType a), Show (Pl.DatumType a)) =>
    Pl.TypedValidator a ->
    [(Pl.DatumType a, Pl.Value)] ->
    Constraint
  SpendsScript ::
    (Pl.ToData (Pl.DatumType a), Pl.ToData (Pl.RedeemerType a), Show (Pl.DatumType a)) =>
    Pl.TypedValidator a ->
    Pl.RedeemerType a ->
    (SpendableOut, Pl.DatumType a) ->
    Constraint
  -- TODO: something like stepscript below could be nice!
  -- StepsScript  :: (Pl.ToData a, Pl.ToData redeemer)
  --              => Pl.Validator -> redeemer -> (SpendableOut, a) -> (a -> a) -> Constraint

  PaysPK :: Pl.PubKeyHash -> Pl.Value -> Constraint
  SpendsPK :: SpendableOut -> Constraint
  Mints :: [Pl.MintingPolicy] -> Pl.Value -> Constraint
  Before :: Pl.POSIXTime -> Constraint
  After :: Pl.POSIXTime -> Constraint
  ValidateIn :: Pl.POSIXTimeRange -> Constraint
  SignedBy :: [Wallet] -> Constraint

-- TODO: add more constraints

spentByPK :: Monad m => Pl.PubKeyHash -> Pl.Value -> MockChainT m [Constraint]
spentByPK pkh val = do
  allOuts <- pkUtxos pkh
  let (toSpend, leftOver) = spendValueFrom val $ map (second Pl.toTxOut) allOuts
  (PaysPK pkh leftOver :) . map SpendsPK <$> mapM spendableRef toSpend

-- * Converting 'Constraint's to 'Pl.ScriptLookups', 'Pl.TxConstraints' and '[Wallet]'

type LedgerConstraint a = (Pl.ScriptLookups a, Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a), [Wallet])

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
toLedgerConstraint (SpendsScript v r ((oref, o), a)) = (lkups, constr, mempty)
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

-- A Transaction skeleton is a set of our constraints, and
-- one of our wallet which will sign the generated transaction.
data TxSkel = TxSkel
  { txMainSigner :: Wallet,
    txConstraints :: [Constraint]
  }

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
