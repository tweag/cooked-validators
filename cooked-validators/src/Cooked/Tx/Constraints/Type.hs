{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Tx.Constraints.Type where

import Data.Default
import qualified Ledger as Pl hiding (unspentOutputs)
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, TypedValidator)
import qualified PlutusTx as Pl
import Type.Reflection

-- | A 'SpendableOut' is an outref that is ready to be spend; with its
--  underlying 'Pl.ChainIndexTxOut'.
type SpendableOut = (Pl.TxOutRef, Pl.ChainIndexTxOut)

-- | Accesses the 'Pl.Value' within a 'SpendableOut'
sOutValue :: SpendableOut -> Pl.Value
sOutValue = Pl.txOutValue . Pl.toTxOut . snd

-- | Accesses the 'Pl.Address' within a 'SpendableOut'
sOutAddress :: SpendableOut -> Pl.Address
sOutAddress = Pl.txOutAddress . Pl.toTxOut . snd

-- | Accesses a potential 'Pl.DatumHash' within a 'SpendableOut'; note that
--  the existence (or not) of a datum hash /DOES NOT/ indicate the 'SpendableOut'
--  belongs to a script or a public key, you must pattern match on the result of
--  'sOutAddress' or use one of 'sBelongsToPubKey' or 'sBelongsToScript' to distinguish that.
sOutDatumHash :: SpendableOut -> Maybe Pl.DatumHash
sOutDatumHash = Pl.txOutDatum . Pl.toTxOut . snd

-- | If a 'SpendableOut' belongs to a public key, return its hash.
sBelongsToPubKey :: SpendableOut -> Maybe Pl.PubKeyHash
sBelongsToPubKey s = case Pl.addressCredential (sOutAddress s) of
  Pl.PubKeyCredential pkh -> Just pkh
  _ -> Nothing

-- | If a 'SpendableOut' belongs to a validator, return its hash.
sBelongsToScript :: SpendableOut -> Maybe Pl.ValidatorHash
sBelongsToScript s = case Pl.addressCredential (sOutAddress s) of
  Pl.ScriptCredential sh -> Just sh
  _ -> Nothing

type SpendsConstrs a =
  ( Pl.ToData (Pl.DatumType a),
    Pl.ToData (Pl.RedeemerType a),
    Show (Pl.DatumType a),
    Show (Pl.RedeemerType a),
    Typeable a
  )

-- | Our own first-class constraint type. The advantage over the regular plutus constraint
--  type is that we get to add whatever we need and we hide away the type variables in existentials.
data Constraint where
  PaysScript ::
    (Pl.ToData (Pl.DatumType a), Show (Pl.DatumType a), Typeable a) =>
    Pl.TypedValidator a ->
    [(Pl.DatumType a, Pl.Value)] ->
    Constraint
  SpendsScript ::
    (SpendsConstrs a) =>
    Pl.TypedValidator a ->
    Pl.RedeemerType a ->
    (SpendableOut, Pl.DatumType a) ->
    Constraint
  -- TODO: something like stepscript below could be nice!
  -- StepsScript  :: (Pl.ToData a, Pl.ToData redeemer)
  --              => Pl.Validator -> redeemer -> (SpendableOut, a) -> (a -> a) -> Constraint

  PaysPK :: Pl.PubKeyHash -> Pl.Value -> Constraint
  SpendsPK :: SpendableOut -> Constraint
  Mints ::
    (Pl.ToData a, Show a) =>
    Maybe a ->
    [Pl.MintingPolicy] ->
    Pl.Value ->
    Constraint
  Before :: Pl.POSIXTime -> Constraint
  After :: Pl.POSIXTime -> Constraint
  ValidateIn :: Pl.POSIXTimeRange -> Constraint
  SignedBy :: [Pl.PubKeyHash] -> Constraint

mints :: [Pl.MintingPolicy] -> Pl.Value -> Constraint
mints = Mints @() Nothing

-- | A Transaction skeleton is a set of our constraints,
-- and an optional showable label, which is useful when displaying
-- traces. The label was encoded as an existential to enable us to
-- create ad-hoc datatypes and pass their values around, for instance:
--
-- > data ProposePaymentLbl = ProposePayment Value deriving Show
-- > f = ... (txSkelLbl (ProposePayment val) ...) ...
--
-- This way we can (A) modify the show behavior if we want and (B)
-- not worry about constructing consistent strings when constructing transactions
--
-- A TxSkel does /NOT/ include a Wallet since wallets only exist in mock mode.
data TxSkel where
  TxSkel ::
    (Show x) =>
    { txLabel :: Maybe x,
      -- | Set of options to use when generating this transaction.
      txOpts :: TxOpts,
      txConstraints :: [Constraint]
    } ->
    TxSkel

-- | Constructs a skeleton without a default label and with default 'TxOpts'
txSkel :: [Constraint] -> TxSkel
txSkel = txSkelOpts def

-- | Constructs a skeleton without a default label, but with custom options
txSkelOpts :: TxOpts -> [Constraint] -> TxSkel
txSkelOpts = TxSkel @() Nothing

-- | Constructs a skeleton with a label
txSkelLbl :: (Show x) => x -> [Constraint] -> TxSkel
txSkelLbl x = TxSkel (Just x) def

-- | Set of options to modify the behavior of generating and validating some transaction. Some of these
-- options only have an effect when running in the 'Plutus.Contract.Contract', some only have an effect when
-- running in 'MockChainT'. If nothing is explicitely stated, the option has an effect independently of the
-- running context.
data TxOpts = TxOpts
  { -- | Performs an adjustment to unbalanced txs, making sure every UTxO that is produced
    --  has the necessary minimum amount of Ada.
    --
    -- By default, this is set to @False@, given this is the default behavior in Plutus:
    -- https://github.com/input-output-hk/plutus-apps/issues/143#issuecomment-1013012744
    adjustUnbalTx :: Bool,
    -- | When submitting a transaction for real (i.e., running in the 'Plutus.Contract.Contract' monad),
    --  it is common to call 'Plutus.Contract.Request.awaitTxConfirmed' after 'Plutus.Contract.Request.submitTxConstraints'.
    --  If you /do NOT/ wish to do so, please set this to @False@.
    --
    --  /This has NO effect when running outside of 'Plutus.Contract.Contract'/.
    --  By default, this is set to @True@.
    awaitTxConfirmed :: Bool,
    -- | Whether to increase the slot counter automatically on this submission.
    -- This is useful for modelling transactions that could be submitted in parallel in reality, so there
    -- should be no explicit ordering of what comes first. One good example is in the Crowdfunding use case contract.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    --  By default, this is set to @True@.
    autoSlotIncrease :: Bool,
    -- | Applies a modification to a transaction after it has been pottentially adjusted ('adjustUnbalTx')
    -- and balanced. This is prefixed with /unsafe/ to draw attention that modifying a transaction at
    -- that stage might make it invalid. Still, this offers a hook for being able to alter a transaction
    -- in unforeseen ways. It is mostly used to test contracts that have been written for custom PABs.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to 'Id'.
    unsafeModTx :: RawModTx,
    -- | Whether or not to skip balancing the transaction altogether.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to @False@.
    noBalance :: Bool
  }
  deriving (Eq, Show)

-- IMPORTANT INTERNAL: If you add or remove fields from 'TxOpts', make sure
-- to update the internal @fields@ value from 'Cooked.Tx.Constraints.Pretty'

-- | Wraps a function that can be applied to a transaction right before submitting it.
--  We have a distinguished datatype to be able to provide a little more info on
--  the show instance.
data RawModTx = Id | RawModTx (Pl.Tx -> Pl.Tx)

applyRawModTx :: RawModTx -> Pl.Tx -> Pl.Tx
applyRawModTx Id tx = tx
applyRawModTx (RawModTx f) tx = f tx

instance Eq RawModTx where
  Id == Id = True
  _ == _ = False

instance Show RawModTx where
  show Id = "Id"
  show (RawModTx _) = "RawModTx"

instance Default TxOpts where
  def =
    TxOpts
      { adjustUnbalTx = False,
        awaitTxConfirmed = True,
        autoSlotIncrease = True,
        unsafeModTx = Id,
        noBalance = False
      }
