{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cooked.Tx.Constraints.Type where

import qualified Control.Lens as Lens ((%~))
import Data.Default
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ledger as Pl
import qualified Ledger.Constraints.OffChain as Pl
import qualified Ledger.Typed.Scripts as Pl
import Optics.Core
import Optics.TH
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx.Prelude as Pl
import Type.Reflection

-- For template Haskell reasons, the most intersting thing in this module (the
-- definition of 'TxSkel') is at the very bottom of this file

-- * 'SpendableOut': The type of UTxOs

-- | A 'SpendableOut' is an outref that is ready to be spend; with its
--  underlying 'Pl.ChainIndexTxOut'.
data SpendableOut = SpendableOut
  { _spOutTxOutRef :: Pl.TxOutRef,
    _spOutCITxOut :: Pl.ChainIndexTxOut
  }
  deriving (Eq)

makeLenses ''SpendableOut

instance Ord SpendableOut where
  -- TODO: Is this sufficient, i.e. can there be well-formed 'SpendableOut's
  -- that have the same 'TxOutRef', but different 'ChainIndexTxOut's?
  (<=) = (<=) `on` (^. spOutTxOutRef)

-- -- | If a 'SpendableOut' belongs to a public key, return its hash.
-- sBelongsToPubKey :: SpendableOut -> Maybe Pl.PubKeyHash
-- sBelongsToPubKey s = case Pl.addressCredential (sOutAddress s) of
--   Pl.PubKeyCredential pkh -> Just pkh
--   _ -> Nothing

-- -- | If a 'SpendableOut' belongs to a validator, return its hash.
-- sBelongsToScript :: SpendableOut -> Maybe Pl.ValidatorHash
-- sBelongsToScript s = case Pl.addressCredential (sOutAddress s) of
--   Pl.ScriptCredential sh -> Just sh
--   _ -> Nothing

spOutDatumOrHash :: AffineTraversal' SpendableOut (Either Pl.DatumHash Pl.Datum)
spOutDatumOrHash = spOutCITxOut % singular (traversalVL Pl.ciTxOutDatum)

spOutDatum :: AffineTraversal' SpendableOut Pl.Datum
spOutDatum = spOutDatumOrHash % _Right

spOutDatumHash :: AffineTraversal' SpendableOut Pl.DatumHash
spOutDatumHash = spOutDatumOrHash % _Left

spOutValue :: Lens' SpendableOut Pl.Value
spOutValue = spOutCITxOut % lensVL Pl.ciTxOutValue

spOutAddress :: Lens' SpendableOut Pl.Address
spOutAddress = spOutCITxOut % lensVL Pl.ciTxOutAddress

-- * Transaction labels

type LabelConstrs x = (Show x, Typeable x, Eq x, Ord x)

data TxLabel where
  TxLabel :: LabelConstrs x => x -> TxLabel

instance Eq TxLabel where
  a == x = compare a x == EQ

{- note: How to write 'Ord' instances for types with existential type variables in
 their constructors?

The idea of the 'Ord' instances for 'TxLabel', 'MintsConstraint', and
'InConstraint' is illustrated by the instance for 'TxLabel' below: Sort by the
type representation of the existential type variable first, and then by the
concrete value within each of the possible instances of the existential.

This means:

- If the type(representation) of a is strictly smaller than the type of b, then
  a<b.

- If the types of a and b are the same, compare a and b normally.

- If the type of a is strictly greater than the type of b, then a>b.

-}

instance Ord TxLabel where
  compare (TxLabel a) (TxLabel x) =
    case compare (SomeTypeRep (typeOf a)) (SomeTypeRep (typeOf x)) of
      LT -> LT
      GT -> GT
      EQ -> case typeOf a `eqTypeRep` typeOf x of
        Just HRefl -> compare a x
        -- This can never happen, since 'eqTypeRep' is implemented in terms of '==' on the type representation:
        Nothing -> error "Type representations compare as EQ, but are not eqTypeRep"

-- * Transaction options

-- | Specifies how to select the collateral input
data Collateral
  = -- | Will select the first Ada-only UTxO we find belonging to 'ownPaymentPubKeyHash'
    CollateralAuto
  | -- | Will use the 'Pl.TxOutRef's given in the set. The set can be empty, in
    --  which case no collateral will be used whatsoever.
    CollateralUtxos (Set Pl.TxOutRef)
  deriving (Eq, Show)

instance Default Collateral where
  def = CollateralAuto

-- | Any manual adjustment should be kept, and if there are several sets of
-- potential collateral UTxOs, they should combine.
instance Semigroup Collateral where
  CollateralAuto <> a = a
  a <> CollateralAuto = a
  CollateralUtxos l <> CollateralUtxos r = CollateralUtxos (l <> r)

-- | Whether to adjust existing public key outputs during
-- transaction balancing.
data BalanceOutputPolicy
  = -- | Try to adjust an existing public key output with the change. If no
    --   suitable output can be found, create a new change output.
    AdjustExistingOutput
  | -- | Do not change the existing outputs, always create a new change
    --   output.
    DontAdjustExistingOutput
  deriving (Eq, Ord, Show)

instance Default BalanceOutputPolicy where
  def = AdjustExistingOutput

-- | This instance always takes the non-default value, if either of the
-- arguments is non-default.
instance Semigroup BalanceOutputPolicy where
  a <> b = fromMaybe def (find (/= def) [a, b])

-- | Wraps a function that can be applied to a transaction right before submitting it.
--  We have a distinguished datatype to be able to provide a little more info on
--  the show instance.
data RawModTx
  = -- | Apply modification on transaction after balancing is performed
    RawModTxAfterBalancing (Pl.Tx -> Pl.Tx)
  | -- | Apply modification on transaction before balancing and transaction fee computation
    --   are performed.
    RawModTxBeforeBalancing (Pl.Tx -> Pl.Tx)

instance Eq RawModTx where
  _ == _ = False

instance Show RawModTx where
  show (RawModTxAfterBalancing _) = "RawModTxAfterBalancing"
  show (RawModTxBeforeBalancing _) = "RawModTxBeforeBalancing"

-- | only applies modification for RawModTxAfterBalancing
applyRawModOnBalancedTx :: [RawModTx] -> Pl.Tx -> Pl.Tx
applyRawModOnBalancedTx [] tx = tx
applyRawModOnBalancedTx (RawModTxAfterBalancing f : fs) tx = applyRawModOnBalancedTx fs . f $ tx
applyRawModOnBalancedTx (RawModTxBeforeBalancing _ : fs) tx = applyRawModOnBalancedTx fs tx

-- | only applies modification for RawModTxBeforeBalancing
applyRawModOnUnbalancedTx :: [RawModTx] -> Pl.UnbalancedTx -> Pl.UnbalancedTx
applyRawModOnUnbalancedTx [] tx = tx
applyRawModOnUnbalancedTx (RawModTxAfterBalancing _ : fs) tx = applyRawModOnUnbalancedTx fs tx
applyRawModOnUnbalancedTx (RawModTxBeforeBalancing f : fs) tx = applyRawModOnUnbalancedTx fs . (Pl.tx Lens.%~ f) $ tx

-- | Set of options to modify the behavior of generating and validating some transaction. Some of these
-- options only have an effect when running in the 'Plutus.Contract.Contract', some only have an effect when
-- running in 'MockChainT'. If nothing is explicitely stated, the option has an effect independently of the
-- running context.
--
-- IMPORTANT INTERNAL: If you add or remove fields from 'TxOpts', make sure
-- to update the internal @fields@ value from 'Cooked.Tx.Constraints.Pretty'
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
    -- | Applies an arbitrary modification to a transaction after it has been
    -- potentially adjusted ('adjustUnbalTx') and balanced. This is prefixed
    -- with /unsafe/ to draw attention to the fact that modifying a transaction
    -- at that stage might make it invalid. Still, this offers a hook for being
    -- able to alter a transaction in unforeseen ways. It is mostly used to test
    -- contracts that have been written for custom PABs.
    --
    -- One interesting use of this function is to observe a transaction just
    -- before it is being sent for validation, with @unsafeModTx =
    -- RawModTxAfterBalancing Debug.Trace.traceShowId@.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.  By
    -- default, this is set to 'Id'.
    unsafeModTx :: [RawModTx],
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
    collateral :: Collateral,
    -- | The 'BalanceOutputPolicy' to apply when balancing the transaction.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to @AdjustExistingOutput@.
    balanceOutputPolicy :: BalanceOutputPolicy
  }
  deriving (Eq)

instance Default TxOpts where
  def =
    TxOpts
      { adjustUnbalTx = False,
        awaitTxConfirmed = True,
        autoSlotIncrease = True,
        forceOutputOrdering = True,
        unsafeModTx = [],
        balance = True,
        collateral = def,
        balanceOutputPolicy = def
      }

-- | This instance always takes the non-default value on booleans, if either of
-- the arguments is non-default.
instance Semigroup TxOpts where
  ( TxOpts
      adjustUnbalTx1
      awaitTxConfirmed1
      autoSlotIncrease1
      forceOutputOrdering1
      unsafeModTx1
      balance1
      collateral1
      balanceOutputPolicy1
    )
    <> ( TxOpts
           adjustUnbalTx2
           awaitTxConfirmed2
           autoSlotIncrease2
           forceOutputOrdering2
           unsafeModTx2
           balance2
           collateral2
           balanceOutputPolicy2
         ) =
      TxOpts
        (takeNonDefault (adjustUnbalTx def) adjustUnbalTx1 adjustUnbalTx2)
        (takeNonDefault (awaitTxConfirmed def) awaitTxConfirmed1 awaitTxConfirmed2)
        (takeNonDefault (autoSlotIncrease def) autoSlotIncrease1 autoSlotIncrease2)
        (takeNonDefault (forceOutputOrdering def) forceOutputOrdering1 forceOutputOrdering2)
        (unsafeModTx1 ++ unsafeModTx2) -- this will apply the left modifications first. See the definitions of 'applyRawModOnUnbalancedTx' and 'applyRawModOnBalancedTx'
        (takeNonDefault (balance def) balance1 balance2)
        (collateral1 <> collateral2)
        (balanceOutputPolicy1 <> balanceOutputPolicy2)
      where
        takeNonDefault d a b = if any (/= d) [a, b] then not d else d

instance Monoid TxOpts where
  mempty = def

-- * Minting Constraints

type MintsConstrs a =
  ( Pl.ToData a,
    Pl.Eq a,
    -- Show a,
    Typeable a
  )

data MintsConstraint where
  MintsConstraint ::
    MintsConstrs a =>
    { _mintsRedeemer :: Maybe a,
      _mintsPolicy :: Pl.MintingPolicy,
      _mintsTokenName :: Pl.TokenName,
      _mintsAmount :: Integer
    } ->
    MintsConstraint

makeLenses ''MintsConstraint

instance Eq MintsConstraint where
  m1 == m2 = compare m1 m2 == EQ

instance Ord MintsConstraint where
  (MintsConstraint r1 p1 t1 n1) <= (MintsConstraint r2 p2 t2 n2) =
    SomeTypeRep (typeOf r1) <= SomeTypeRep (typeOf r2)
      && Pl.toData r1 <= Pl.toData r2
      && p1 <= p2
      && t1 <= t2
      && n1 <= n2

-- * Input Constraints

type SpendsScriptConstrs a =
  ( -- Pl.ToData (Pl.DatumType a),
    Pl.ToData (Pl.RedeemerType a),
    -- Pl.UnsafeFromData (Pl.DatumType a),
    -- Show (Pl.DatumType a),
    -- Show (Pl.RedeemerType a),
    -- Pl.Eq (Pl.DatumType a),
    Pl.Eq (Pl.RedeemerType a),
    Typeable a
  )

data InConstraint where
  SpendsScript ::
    SpendsScriptConstrs a =>
    { _spendingValidator :: Pl.TypedValidator a,
      _redeemer :: Pl.RedeemerType a,
      _input :: SpendableOut
    } ->
    InConstraint
  SpendsPK :: {_input :: SpendableOut} -> InConstraint

makeLenses ''InConstraint

instance Eq InConstraint where
  c1 == c2 = compare c1 c2 == EQ

instance Ord InConstraint where
  (SpendsScript v1 r1 o1) <= (SpendsScript v2 r2 o2) =
    SomeTypeRep (typeOf v1) < SomeTypeRep (typeOf v2)
      || case typeOf v1 `eqTypeRep` typeOf v2 of
        Just HRefl ->
          Pl.validatorHash v1 <= Pl.validatorHash v2 -- TODO will this suffice, or do we need to compare more components of the typed validator?
            && Pl.toData r1 <= Pl.toData r2
            && o1 <= o2
        Nothing -> False
  (SpendsPK o1) <= (SpendsPK o2) = o1 <= o2
  SpendsPK {} <= SpendsScript {} = True
  SpendsScript {} <= SpendsPK {} = False

-- * Output Constraints

type PaysScriptConstrs a =
  ( Pl.ToData (Pl.DatumType a),
    Show (Pl.DatumType a),
    Pl.Eq (Pl.DatumType a),
    Typeable a
  )

type PaysPKConstrs a =
  ( Pl.ToData a,
    Show a,
    Pl.Eq a,
    Typeable a
  )

data OutConstraint where
  PaysScript ::
    PaysScriptConstrs a =>
    { _recipientValidator :: Pl.TypedValidator a,
      _mStakeCred :: Maybe Pl.StakingCredential,
      _datum :: Pl.DatumType a,
      _outValue :: Pl.Value
    } ->
    OutConstraint
  PaysPK ::
    PaysPKConstrs a =>
    { _recipientPubKeyHash :: Pl.PubKeyHash,
      _mStakePubKeyHash :: Maybe Pl.StakePubKeyHash,
      _mDatum :: Maybe a,
      _outValue :: Pl.Value
    } ->
    OutConstraint

makeLenses ''OutConstraint

instance Eq OutConstraint where
  (PaysScript v1 sc1 d1 x1) == (PaysScript v2 sc2 d2 x2) =
    case typeOf v1 `eqTypeRep` typeOf v2 of
      Just HRefl -> d1 Pl.== d2 && (v1, sc1, x1) == (v2, sc2, x2)
      Nothing -> False
  (PaysPK h1 sc1 d1 x1) == (PaysPK h2 sc2 d2 x2) =
    case typeOf d1 `eqTypeRep` typeOf d2 of
      Just HRefl -> d1 Pl.== d2 && (h1, sc1, x1) == (h2, sc2, x2)
      Nothing -> False
  _ == _ = False

-- * Transaction skeletons

data TxSkel where
  TxSkel ::
    { _txSkelLabel :: Set TxLabel,
      _txSkelOpts :: TxOpts,
      _txSkelMints :: Set MintsConstraint,
      _txSkelValidityRange :: Pl.POSIXTimeRange,
      _txSkelIns :: Set InConstraint,
      _txSkelOuts :: [OutConstraint]
    } ->
    TxSkel
  -- This equality instance should reflect semantic equality; If two 'TxSkel's
  -- are equal in the sense of '==', they specify the same transaction(s).
  deriving (Eq)

makeLenses ''TxSkel

-- | The idea behind this 'Semigroup' instance is that for two 'TxSkel's @a@ and
-- @b@, the transaction(s) described by @a <> b@ should satisfy all requirements
-- contained in @a@ and all requirements contained in @b@. There are a few
-- wrinkles with regard to this:
--
-- - commutativity: @a <> b@ and @b <> a@ describe different transactions in
--   general. In particular,
--
--   - The output constraints of the right argument are appended to the end of
--     the list of transaction outputs. This matters because some transactions
--     rely on the ordering of inputs.
--
--   - The 'unsafeModTx' contained in the '_txSkelOpts' is also combined
--     non-commutatively. The modifications in the left argument will be applied
--     first.
--
-- - preference for non-defaults: All of the boolean options in '_txSkelOpts',
--   as well as 'collateral' and 'balanceOutputPolicy' combine in a way that if
--   either of the arguments has a non-default value, that value will be
--   kept. In the case of 'collateral', the sets of Collateral UTxOs will be
--   combined (see the 'Semigroup' definitions for 'Collateral' and
--   'BalanceOutputPolicy').
--
-- One property that should hold (TODO: write tests) is that
--
-- > a == x && b == y `implies` a <> b == x <> y
instance Semigroup TxSkel where
  (TxSkel l1 p1 m1 r1 i1 o1) <> (TxSkel l2 p2 m2 r2 i2 o2) =
    TxSkel
      (l1 <> l2)
      (p1 <> p2)
      (m1 <> m2)
      (r1 `Pl.intersection` r2)
      (i1 <> i2)
      (o1 ++ o2)

instance Monoid TxSkel where
  mempty = TxSkel Set.empty mempty Set.empty Pl.always Set.empty []

-- -- | Constructs a skeleton without a default label and with default 'TxOpts'
-- txSkel :: ConstraintsSpec constraints => constraints -> TxSkel
-- txSkel = txSkelOpts def

-- -- | Constructs a skeleton without a default label, but with custom options
-- txSkelOpts :: ConstraintsSpec constraints => TxOpts -> constraints -> TxSkel
-- txSkelOpts opts cs = TxSkel @() Nothing opts (toConstraints cs)

-- -- | Constructs a skeleton with a label
-- txSkelLbl :: (LabelConstrs x, ConstraintsSpec constraints) => x -> constraints -> TxSkel
-- txSkelLbl x = TxSkel (Just x) def . toConstraints

-- -- | Constructs a skeleton with the given labtl, options, and constraints
-- txSkelLblOpts :: (LabelConstrs x, ConstraintsSpec constraints) => x -> TxOpts -> constraints -> TxSkel
-- txSkelLblOpts x os cs = TxSkel (Just x) os (toConstraints cs)

-- paysPK :: Pl.PubKeyHash -> Pl.Value -> OutConstraint
-- paysPK pkh = PaysPKWithDatum @() pkh Nothing Nothing

-- paysScript :: (PaysScriptConstrs a) => Pl.TypedValidator a -> Pl.DatumType a -> Pl.Value -> OutConstraint
-- paysScript tv = PaysScript tv Nothing

-- mints :: [Pl.MintingPolicy] -> Pl.Value -> MiscConstraint
-- mints = Mints @() Nothing
