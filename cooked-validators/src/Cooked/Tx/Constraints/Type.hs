{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- Our own first-class constraint types. The advantage over the regular plutus constraint
-- type is that we get to add whatever we need and we hide away the type variables in existentials.

-- | The main constraints datatype. It combines output constraints
-- ('OutConstraint') in a specific order that is respected by the generated
-- transaction, and miscellaneous constraints 'MiscConstraint' including input
-- constraints, time constraints, or signature requirements.
data Constraints = [MiscConstraint] :=>: [OutConstraint]

instance Semigroup Constraints where
  (c1 :=>: oc1) <> (c2 :=>: oc2) = (c1 <> c2) :=>: (oc1 <> oc2)

instance Monoid Constraints where
  mempty = [] :=>: []

-- | Constraints which do not specify new transaction outputs
data MiscConstraint where
  SpendsScript ::
    (SpendsConstrs a) =>
    Pl.TypedValidator a ->
    Pl.RedeemerType a ->
    (SpendableOut, Pl.DatumType a) ->
    MiscConstraint
  SpendsPK :: SpendableOut -> MiscConstraint
  Mints ::
    (Pl.ToData a, Show a) =>
    Maybe a ->
    [Pl.MintingPolicy] ->
    Pl.Value ->
    MiscConstraint
  Before :: Pl.POSIXTime -> MiscConstraint
  After :: Pl.POSIXTime -> MiscConstraint
  ValidateIn :: Pl.POSIXTimeRange -> MiscConstraint
  SignedBy :: [Pl.PubKeyHash] -> MiscConstraint

-- | Constraints which specify new transaction outputs
data OutConstraint where
  PaysScript ::
    (Pl.ToData (Pl.DatumType a), Show (Pl.DatumType a), Typeable a) =>
    Pl.TypedValidator a ->
    Pl.DatumType a ->
    Pl.Value ->
    OutConstraint
  -- | Creates a UTxO to a specific 'Pl.PubKeyHash' with a potential 'Pl.StakePubKeyHash'.
  -- and datum. If the stake pk is present, it will call 'Ledger.Constraints.OffChain.ownStakePubKeyHash'
  -- to update the script lookups, hence, generating a transaction with /two/ 'PaysPKWithDatum' with
  -- two different staking keys will cause the first staking key to be overriden by calling @ownStakePubKeyHash@
  -- a second time. If this is an issue for you, please do submit an issue with an explanation on GitHub.
  PaysPKWithDatum ::
    (Pl.ToData a, Show a) =>
    Pl.PubKeyHash ->
    Maybe Pl.StakePubKeyHash ->
    Maybe a ->
    Pl.Value ->
    OutConstraint

-- | This typeclass provides user-friendly convenience to overload the
-- 'txConstraints' field of 'TxSkel'. For instance, this enables us to ommit the '(:=>:)'
-- constructor whenever we're using only one kind of constraints.
-- It also opens up future alternative of constraint specification.
class ConstraintsSpec a where
  toConstraints :: a -> Constraints

instance ConstraintsSpec Constraints where
  toConstraints = id

instance ConstraintsSpec [MiscConstraint] where
  toConstraints = (:=>: [])

instance ConstraintsSpec MiscConstraint where
  toConstraints = toConstraints . (: [])

instance ConstraintsSpec [OutConstraint] where
  toConstraints = ([] :=>:)

instance ConstraintsSpec OutConstraint where
  toConstraints = toConstraints . (: [])

paysPK :: Pl.PubKeyHash -> Pl.Value -> OutConstraint
paysPK pkh = PaysPKWithDatum @() pkh Nothing Nothing

mints :: [Pl.MintingPolicy] -> Pl.Value -> MiscConstraint
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
    (Show x, ConstraintsSpec constraints) =>
    { txLabel :: Maybe x,
      -- | Set of options to use when generating this transaction.
      txOpts :: TxOpts,
      txConstraints :: constraints
    } ->
    TxSkel

-- | Constructs a skeleton without a default label and with default 'TxOpts'
txSkel :: ConstraintsSpec constraints => constraints -> TxSkel
txSkel = txSkelOpts def

-- | Constructs a skeleton without a default label, but with custom options
txSkelOpts :: ConstraintsSpec constraints => TxOpts -> constraints -> TxSkel
txSkelOpts = TxSkel @() Nothing

-- | Constructs a skeleton with a label
txSkelLbl :: (Show x, ConstraintsSpec constraints) => x -> constraints -> TxSkel
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
    -- | Reorders the transaction outputs to fit the ordering of output
    -- constraints. Those outputs are put at the very beginning of the list.
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    --  By default, this is set to @True@.
    forceOutputOrdering :: Bool,
    -- | Applies an arbitrary modification to a transaction after it has been pottentially adjusted ('adjustUnbalTx')
    -- and balanced. This is prefixed with /unsafe/ to draw attention that modifying a transaction at
    -- that stage might make it invalid. Still, this offers a hook for being able to alter a transaction
    -- in unforeseen ways. It is mostly used to test contracts that have been written for custom PABs.
    --
    -- One interesting use of this function is to observe a transaction just before it is being
    -- sent for validation, with @unsafeModTx = RawModTx Debug.Trace.traceShowId@.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to 'Id'.
    unsafeModTx :: RawModTx,
    -- | Whether to balance the transaction or not. Balancing is the process of ensuring that
    --  @input + mint = output + fees@, if you decide to set @balance = false@ you will have trouble
    -- satisfying that equation by hand because @fees@ are variable. You will likely see a @ValueNotPreserved@ error
    -- and should adjust the fees accordingly. For now, there is no option to skip the fee computation because
    -- without it, validation through "Ledger.Validation" would fail with @InsufficientFees@.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to @True@.
    balance :: Bool,
    -- | Which collateral utxo to use for this transaction. A collateral UTxO must be an Ada-only utxo
    -- and can be specified manually, or it can be chosen automatically, if any is available.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to @CollateralAuto@.
    collateral :: Collateral
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

data Collateral
  = CollateralAuto
  | CollateralNone
  | CollateralUtxos [Pl.TxOutRef]
  deriving (Eq, Show)

instance Default TxOpts where
  def =
    TxOpts
      { adjustUnbalTx = False,
        awaitTxConfirmed = True,
        autoSlotIncrease = True,
        forceOutputOrdering = True,
        unsafeModTx = Id,
        balance = True,
        collateral = CollateralAuto
      }
