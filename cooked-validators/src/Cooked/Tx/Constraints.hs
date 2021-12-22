{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cooked.Tx.Constraints
  ( module Cooked.Tx.Constraints.Type,
    module Cooked.Tx.Constraints.Pretty,
    LedgerConstraint,
    extractDatumStrFromConstraint,
    toLedgerConstraint,
    toLedgerConstraints,
    WalletStorage (..),
  )
where

import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import qualified Data.Map.Strict as M
import qualified Ledger as Pl hiding (unspentOutputs)
import qualified Ledger.Constraints as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, validatorScript)
import qualified PlutusTx as Pl

-- * Converting 'Constraint's to 'Pl.ScriptLookups', 'Pl.TxConstraints' and '[Wallet]'

data WalletStorage b where
  NoWallets :: WalletStorage 'False
  WithWallets :: [Wallet] -> WalletStorage 'True

instance Semigroup (WalletStorage fs) where
  NoWallets <> NoWallets = NoWallets
  WithWallets ws1 <> WithWallets ws2 = WithWallets $ ws1 <> ws2

instance Monoid (WalletStorage 'False) where
  mempty = NoWallets

instance Monoid (WalletStorage 'True) where
  mempty = WithWallets []

type LedgerConstraint fs a =
  (Pl.ScriptLookups a, Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a), WalletStorage ('Multisign `Elem` fs))

-- | Map from datum hashes to string representation of all the datum carried
extractDatumStrFromConstraint :: Constraint fs -> M.Map Pl.DatumHash String
extractDatumStrFromConstraint (PaysScript _validator datumsAndValues) =
  M.fromList
    . map ((\d -> (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData d, show d)) . fst)
    $ datumsAndValues
extractDatumStrFromConstraint (SpendsScript _validator _redeemer (_out, datum)) =
  M.singleton (Pl.datumHash . Pl.Datum $ Pl.toBuiltinData datum) (show datum)
extractDatumStrFromConstraint _ = M.empty

-- | Converts our constraint into a 'LedgerConstraint',
--  which later can be used to generate a transaction.
toLedgerConstraint :: Monoid (WalletStorage ('Multisign `Elem` fs)) => Constraint fs -> LedgerConstraint fs a
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
toLedgerConstraint (Mints Nothing pols v) = (lkups, constr, mempty)
  where
    lkups = foldMap Pl.mintingPolicy pols
    constr = Pl.mustMintValue v
toLedgerConstraint (Mints (Just r) pols v) = (lkups, constr, mempty)
  where
    lkups = foldMap Pl.mintingPolicy pols
    constr = Pl.mustMintValueWithRedeemer (Pl.Redeemer (Pl.toBuiltinData r)) v
toLedgerConstraint (Before t) = (mempty, constr, mempty)
  where
    constr = Pl.mustValidateIn (Pl.to t)
toLedgerConstraint (After t) = (mempty, constr, mempty)
  where
    constr = Pl.mustValidateIn (Pl.from t)
toLedgerConstraint (ValidateIn r) = (mempty, Pl.mustValidateIn r, mempty)
toLedgerConstraint (SignedBy wals) = (mempty, mempty, WithWallets wals)

toLedgerConstraints :: forall a fs. Monoid (WalletStorage ('Multisign `Elem` fs)) => [Constraint fs] -> LedgerConstraint fs a
toLedgerConstraints cs = (mconcat lkups, mconcat constrs, mconcat wals)
  where
    (lkups, constrs, wals) = unzip3 $ map toLedgerConstraint cs
