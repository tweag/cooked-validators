{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module Cooked.Tx.Constraints where

import           Data.Void
import qualified Data.Map as M

import qualified Ledger as Pl hiding (unspentOutputs)
import qualified Ledger.Constraints as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, TypedValidator, validatorScript)
import qualified PlutusTx as Pl

import Cooked.MockChain.Wallet

-- |A 'SpendableOut' is an outref that is ready to be spend; with its
-- underlying 'Pl.ChainIndexTxOut'.
type SpendableOut = (Pl.TxOutRef, Pl.ChainIndexTxOut)

-- |Our own first class constraint type. The advantage over the regular plutus constraint
-- type is that we get to add whatever we need and we hide away the type variables in existentials.
data Constraint where
  PaysScript   :: (Pl.ToData (Pl.DatumType a))
               => Pl.TypedValidator a -> [(Pl.DatumType a, Pl.Value)] -> Constraint

  SpendsScript :: (Pl.ToData (Pl.DatumType a), Pl.ToData (Pl.RedeemerType a))
               => Pl.TypedValidator a -> Pl.RedeemerType a -> (SpendableOut, Pl.DatumType a) -> Constraint

  -- TODO: something like stepscript below could be nice!
  -- StepsScript  :: (Pl.ToData a, Pl.ToData redeemer)
  --              => Pl.Validator -> redeemer -> (SpendableOut, a) -> (a -> a) -> Constraint

  PaysPK   :: Pl.PubKeyHash -> Pl.Value -> Constraint
  SpendsPK :: SpendableOut -> Constraint

  -- TODO: add more constraints

-- |Converts our constraint into a Plutus 'Pl.ScriptLookups' and 'Pl.TxConstraints',
-- which later can be used to generate a transaction. We're making the conscious choice
-- of making this function type check for arbitrary choices of @a, i@ and @o@,
-- enabling us to hide these from the users of /Cooked/.
toLedgerConstraint :: Constraint -> (Pl.ScriptLookups a, Pl.TxConstraints i o)
toLedgerConstraint (SpendsScript v r ((oref, o), a)) = (lkups, constr)
  where
    lkups  = Pl.otherScript (Pl.validatorScript v)
          <> Pl.otherData (Pl.Datum $ Pl.toBuiltinData a)
          <> Pl.unspentOutputs (M.singleton oref o)
    constr = Pl.mustSpendScriptOutput oref (Pl.Redeemer $ Pl.toBuiltinData r)

toLedgerConstraint (PaysScript v outs) = (lkups, constr)
  where
    lkups  = Pl.otherScript (Pl.validatorScript v)
    constr = mconcat $ flip map outs $ \(d, val)
               -> Pl.mustPayToOtherScript (Pl.validatorHash $ Pl.validatorScript v)
                                          (Pl.Datum $ Pl.toBuiltinData d)
                                          val
toLedgerConstraint (PaysPK p v)         = (mempty, Pl.mustPayToPubKey p v)
toLedgerConstraint (SpendsPK (oref, o)) = (lkups, constr)
  where
    lkups  = Pl.unspentOutputs (M.singleton oref o)
    constr = Pl.mustSpendPubKeyOutput oref

-- * Converting 'Constraint's to 'Pl.ScriptLookups' and 'Pl.TxConstraints'

type LedgerConstraint a = (Pl.ScriptLookups a , Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a))

toLedgerConstraints :: [Constraint] -> LedgerConstraint a
toLedgerConstraints cs = (mconcat lkups, mconcat constrs)
  where
    (lkups, constrs) = unzip $ map toLedgerConstraint cs

-- A Transaction sekeleton is a set of our constraints, and
-- a set of our wallets, which will sign the generated transaction.
data TxSkel = TxSkel
  { txSigners     :: Wallet
  , txConstraints :: [Constraint]
  }

-- |Generates an unbalanced transaction from a skeleton; A
-- transaction is unbalanced whenever @inputs + mints != outputs + fees@.
-- In order to submit a transaction, it must be balanced, otherwise
-- we will see a @ValueNotPreserved@ error.
--
-- See "Cooked.Tx.Balance" for balancing capabilities or stick to
-- 'generateTx', which generates /and/ balances a transaction.
generateUnbalTx :: TxSkel -> Either Pl.MkTxError Pl.UnbalancedTx
generateUnbalTx = uncurry Pl.mkTx . toLedgerConstraints @Void . txConstraints
