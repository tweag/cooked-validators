{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

module Cooked.Tx.Constraints.Type where

import Control.Lens
import Cooked.MockChain.Misc
import Data.Default
import Data.Either
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Ledger as Pl hiding (unspentOutputs)
import qualified Ledger.Constraints as Pl
import qualified Ledger.Constraints.OffChain as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, TypedValidator)
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Type.Reflection

-- | A 'SpendableOut' is an outref that is ready to be spend; with its
--  underlying 'Pl.ChainIndexTxOut'.
type SpendableOut = (Pl.TxOutRef, Pl.ChainIndexTxOut)

-- | Accesses the 'Pl.Value' within a 'SpendableOut'
sOutValue :: SpendableOut -> Pl.Value
sOutValue = Pl.txOutValue . fromRight undefined . Pl.toTxOut theNetworkId . snd -- TODO PORT shall this be in Either?

-- | Accesses the 'Pl.Address' within a 'SpendableOut'
sOutAddress :: SpendableOut -> Pl.Address
sOutAddress = Pl.txOutAddress . fromRight undefined . Pl.toTxOut theNetworkId . snd

{- PORT This doesn't seem to be used anywhere
-- | Accesses a potential 'Pl.DatumHash' within a 'SpendableOut'; note that
--  the existence (or not) of a datum hash /DOES NOT/ indicate the 'SpendableOut'
--  belongs to a script or a public key; you must pattern match on the result of
--  'sOutAddress' or use one of 'sBelongsToPubKey' or 'sBelongsToScript' to distinguish that.
sOutDatumHash :: SpendableOut -> Maybe Pl.DatumHash
sOutDatumHash = Pl.txOutDatum . fromRight undefined . Pl.toTxOut theNetworkId . snd

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
-}

type SpendsConstrs a =
  ( Pl.ToData (Pl.DatumType a),
    Pl.ToData (Pl.RedeemerType a),
    Pl.UnsafeFromData (Pl.DatumType a),
    Show (Pl.DatumType a),
    Show (Pl.RedeemerType a),
    Pl.Eq (Pl.DatumType a),
    Pl.Eq (Pl.RedeemerType a),
    Typeable a
  )

type PaysScriptConstrs a =
  ( Pl.ToData (Pl.DatumType a),
    Show (Pl.DatumType a),
    Pl.Eq (Pl.DatumType a),
    Typeable a
  )

type MintsConstrs a =
  ( Pl.ToData a,
    Pl.Eq a,
    Show a,
    Typeable a
  )

-- Our own first-class constraint types. The advantage over the regular plutus constraint
-- type is that we get to add whatever we need and we hide away the type variables in existentials.

-- | The main constraints datatype. It combines output constraints
-- ('OutConstraint') in a specific order that is respected by the generated
-- transaction, and miscellaneous constraints 'MiscConstraint' including input
-- constraints, time constraints, or signature requirements.
data Constraints = [MiscConstraint] :=>: [OutConstraint]
  deriving (Eq)

instance Semigroup Constraints where
  (c1 :=>: oc1) <> (c2 :=>: oc2) = (c1 <> c2) :=>: (oc1 <> oc2)

instance Monoid Constraints where
  mempty = [] :=>: []

-- | Check whether two constraints are the same, up to
--
-- - reordering of inputs,
--
-- - substitutions of time constraints ('After', 'Before', 'ValidateIn') that
--   preserve the validity interval, and
--
-- - differences in 'Mints' constraints that leave the specified value that
--   should be minted for each redeemer the same.
--
-- This relation is coarser than equality as defined on 'Constraint', and more
-- closely reflects the notion of "these constraints specify the same
-- transaction".
sameConstraints :: Constraints -> Constraints -> Bool
sameConstraints (is :=>: os) (is' :=>: os') =
  sameSets (filter isSpendsScriptOrSpendsPK is) (filter isSpendsScriptOrSpendsPK is')
    && (os == os')
    && (validityRange is == validityRange is')
    && (sort (signers is) == sort (signers is'))
    && sameMintedValuesWithRedeemers is is'
  where
    sameSets :: Eq a => [a] -> [a] -> Bool
    sameSets l r = length l == length r && subset l r && subset r l

    subset :: Eq a => [a] -> [a] -> Bool
    subset l r = all (`elem` r) l

    isSpendsScriptOrSpendsPK :: MiscConstraint -> Bool
    isSpendsScriptOrSpendsPK SpendsScript {} = True
    isSpendsScriptOrSpendsPK SpendsPK {} = True
    isSpendsScriptOrSpendsPK _ = False

    validityRange :: [MiscConstraint] -> Pl.POSIXTimeRange
    validityRange = foldr (Pl.intersection . toTimeRange) Pl.always
      where
        toTimeRange = \case
          Before b -> Pl.to b
          After a -> Pl.from a
          ValidateIn i -> i
          _ -> Pl.always

    signers :: [MiscConstraint] -> [Pl.PubKeyHash]
    signers = foldr (union . toSignerList) []
      where
        toSignerList (SignedBy s) = s
        toSignerList _ = []

    sameMintedValuesWithRedeemers :: [MiscConstraint] -> [MiscConstraint] -> Bool
    sameMintedValuesWithRedeemers cs cs' = mintedWithRedeemer cs == mintedWithRedeemer cs'
      where
        mintedWithRedeemer :: [MiscConstraint] -> Map MintsRedeemer Pl.Value
        mintedWithRedeemer = foldr extendWithValue M.empty
          where
            extendWithValue :: MiscConstraint -> Map MintsRedeemer Pl.Value -> Map MintsRedeemer Pl.Value
            extendWithValue (Mints r _ v) m = M.insertWith (<>) (MintsRedeemer r) v m
            extendWithValue _ m = m

-- | Helper type for 'sameConstraints', used to wrap the redeemers in 'Mints'
-- constraints.
data MintsRedeemer where
  MintsRedeemer :: MintsConstrs a => Maybe a -> MintsRedeemer

instance Eq MintsRedeemer where
  MintsRedeemer a == MintsRedeemer x = case typeOf a `eqTypeRep` typeOf x of
    Just HRefl -> a Pl.== x
    Nothing -> False

instance Ord MintsRedeemer where
  MintsRedeemer a <= MintsRedeemer x = Pl.toData a <= Pl.toData x

-- | Constraints which do not specify new transaction outputs
data MiscConstraint where
  -- | Ensure that the given 'Pl.TypedValidator' spends a specific UTxO (which
  -- must belong to the validator). That is: Unlock the UTxO described by the
  -- given 'SpendableOut', passing the given redeemer to the validator script.
  SpendsScript ::
    (SpendsConstrs a) =>
    Pl.TypedValidator a ->
    Pl.RedeemerType a ->
    -- | Utxo to spend.
    -- WARNING: A "SpendableOut" contains a "ChainIndexTxOut" that contains
    -- either a datum or its hash. Spending a script "SpendableOut" which does
    -- not contain the datum explicitly causes the generated transaction to
    -- fail (datum not found).
    --
    -- This does not occur in practice when spending UTxOs obtained by
    -- searching through the chain with "scriptUtxosSuchThat", or those
    -- extracted from "CardanoTx" by using "spOutsFromCardanoTx".
    SpendableOut ->
    MiscConstraint
  -- | Ensure that a 'Pl.PubKeyHash' spends a specific UTxO. The hash is not an
  -- argument since it can be read off the given 'SpendableOut'.
  SpendsPK :: SpendableOut -> MiscConstraint
  -- | Ensure that the transaction mints the given 'Pl.Value'. For each
  -- 'Pl.CurrencySymbol' in the value that is to be minted, the given list of
  -- 'Pl.MintingPolicy's has to include the policy governing that currency
  -- symbol. The @Maybe a@-argument is the optional redeemer passed to the
  -- policies: If you pass @Nothing@, the constraint is translated using
  -- 'Pl.mustMintValue', which uses no redeemer; if you pass @Just r@,
  -- 'Pl.mustMintValueWithRedeemer' is used to pass @r@ as a redeemer to the
  -- policies.
  Mints ::
    MintsConstrs a =>
    Maybe a ->
    [Pl.MintingPolicy] ->
    Pl.Value ->
    MiscConstraint
  -- | Ensure that the transaction happens no later than the given time (the end
  -- time is included in the allowed range).
  Before :: Pl.POSIXTime -> MiscConstraint
  -- | Ensure that the transaction happens no earlier than the given time (the
  -- start time is included in the allowed range).
  After :: Pl.POSIXTime -> MiscConstraint
  -- | Ensure that the transaction happens in the given time range.
  ValidateIn :: Pl.POSIXTimeRange -> MiscConstraint
  -- | Ensure that the transaction is signed by the given 'Pl.PubKeyHash'es.
  SignedBy :: [Pl.PubKeyHash] -> MiscConstraint

-- NB don't forget to update the Eq instance when adding new constructors

(~*~?) :: forall ty a1 a2. (Typeable a1, Typeable a2) => ty a1 -> ty a2 -> Maybe (a1 :~~: a2)
_ ~*~? _ = typeRep @a1 `eqTypeRep` typeRep @a2

instance Eq MiscConstraint where
  SpendsScript s1 r1 so1 == SpendsScript s2 r2 so2 =
    case s1 ~*~? s2 of
      Just HRefl -> (s1, so1) == (s2, so2) && r1 Pl.== r2
      Nothing -> False
  SpendsPK so1 == SpendsPK so2 = so1 == so2
  Mints d1 p1 v1 == Mints d2 p2 v2 =
    case d1 ~*~? d2 of
      Just HRefl -> (p1, v1) == (p2, v2) && d1 Pl.== d2
      Nothing -> False
  Before t1 == Before t2 = t1 == t2
  After t1 == After t2 = t1 == t2
  ValidateIn r1 == ValidateIn r2 = r1 == r2
  SignedBy hs1 == SignedBy hs2 = hs1 == hs2
  _ == _ = False

-- | Constraints which specify new transaction outputs
data OutConstraint where
  -- | Creates an UTxO to the given validator, with an optional
  -- staking credential, and with the given datum and the given
  -- value. That is, lets the script lock the given value.
  PaysScript ::
    (PaysScriptConstrs a) =>
    Pl.TypedValidator a ->
    Maybe Pl.StakingCredential ->
    Pl.DatumType a ->
    Pl.Value ->
    OutConstraint
  -- | Creates a UTxO to a specific 'Pl.PubKeyHash' with a potential 'Pl.StakePubKeyHash'.
  -- and datum. If the stake pk is present, it will call 'Ledger.Constraints.OffChain.ownStakePubKeyHash'
  -- to update the script lookups, hence, generating a transaction with /two/ 'PaysPKWithDatum' with
  -- two different staking keys will cause the first staking key to be overriden by calling @ownStakePubKeyHash@
  -- a second time. If this is an issue for you, please do submit an issue with an explanation on GitHub.
  PaysPKWithDatum ::
    (Pl.ToData a, Pl.Eq a, Show a, Typeable a) =>
    Pl.PubKeyHash ->
    Maybe Pl.StakePubKeyHash ->
    Maybe a ->
    Pl.Value ->
    OutConstraint

-- NB don't forget to update the Eq instance when adding new constructors

instance Eq OutConstraint where
  PaysScript s1 sc1 d1 v1 == PaysScript s2 sc2 d2 v2 =
    case s1 ~*~? s2 of
      Just HRefl -> (s1, v1) == (s2, v2) && sc1 Pl.== sc2 && d1 Pl.== d2
      Nothing -> False
  PaysPKWithDatum pk1 stake1 d1 v1 == PaysPKWithDatum pk2 stake2 d2 v2 =
    case d1 ~*~? d2 of
      Just HRefl -> (pk1, stake1, v1) == (pk2, stake2, v2) && d1 Pl.== d2
      Nothing -> False
  _ == _ = False

-- | This typeclass provides user-friendly convenience to overload the
-- 'txConstraints' field of 'TxSkel'. For instance, this enables us to ommit the '(:=>:)'
-- constructor whenever we're using only one kind of constraints.
-- It also opens up future alternative of constraint specification.
class (Eq a, Typeable a) => ConstraintsSpec a where
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

paysScript :: (PaysScriptConstrs a) => Pl.TypedValidator a -> Pl.DatumType a -> Pl.Value -> OutConstraint
paysScript tv = PaysScript tv Nothing

mints :: [Pl.MintingPolicy] -> Pl.Value -> MiscConstraint
mints = Mints @() Nothing

type LabelConstrs x = (Show x, Typeable x, Eq x)

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
    (LabelConstrs x) =>
    { txLabel :: Maybe x,
      -- | Set of options to use when generating this transaction.
      txOpts :: TxOpts,
      txConstraints :: Constraints
    } ->
    TxSkel

instance Eq TxSkel where
  TxSkel l1 o1 c1 == TxSkel l2 o2 c2 =
    case l1 ~*~? l2 of
      Just HRefl -> (l1, o1, c1) == (l2, o2, c2)
      _ -> False

-- | Constructs a skeleton without a default label and with default 'TxOpts'
txSkel :: ConstraintsSpec constraints => constraints -> TxSkel
txSkel = txSkelOpts def

-- | Constructs a skeleton without a default label, but with custom options
txSkelOpts :: ConstraintsSpec constraints => TxOpts -> constraints -> TxSkel
txSkelOpts opts cs = TxSkel @() Nothing opts (toConstraints cs)

-- | Constructs a skeleton with a label
txSkelLbl :: (LabelConstrs x, ConstraintsSpec constraints) => x -> constraints -> TxSkel
txSkelLbl x = TxSkel (Just x) def . toConstraints

-- | Constructs a skeleton with the given labtl, options, and constraints
txSkelLblOpts :: (LabelConstrs x, ConstraintsSpec constraints) => x -> TxOpts -> constraints -> TxSkel
txSkelLblOpts x os cs = TxSkel (Just x) os (toConstraints cs)

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
    collateral :: Collateral,
    -- | The 'BalanceOutputPolicy' to apply when balancing the transaction.
    --
    -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
    -- By default, this is set to @AdjustExistingOutput@.
    balanceOutputPolicy :: BalanceOutputPolicy
  }
  deriving (Eq, Show)

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

-- IMPORTANT INTERNAL: If you add or remove fields from 'TxOpts', make sure
-- to update the internal @fields@ value from 'Cooked.Tx.Constraints.Pretty'

-- | Wraps a function that can be applied to a transaction right before submitting it.
--  We have a distinguished datatype to be able to provide a little more info on
--  the show instance.
data RawModTx
  = -- | no effect modifier
    Id
  | -- | Apply modification on transaction after balancing is performed
    RawModTxAfterBalancing (Pl.Tx -> Pl.Tx)
  | -- | Apply modification on transaction before balancing and transaction fee computation
    --   are performed.
    RawModTxBeforeBalancing (Pl.Tx -> Pl.Tx)

-- | only applies modification for RawModTxAfterBalancing
applyRawModOnBalancedTx :: RawModTx -> Pl.Tx -> Pl.Tx
applyRawModOnBalancedTx Id tx = tx
applyRawModOnBalancedTx (RawModTxAfterBalancing f) tx = f tx
applyRawModOnBalancedTx (RawModTxBeforeBalancing _) tx = tx

-- | only applies modification for RawModTxBeforeBalancing
applyRawModOnUnbalancedTx :: RawModTx -> Pl.UnbalancedTx -> Pl.UnbalancedTx
applyRawModOnUnbalancedTx Id tx = tx
applyRawModOnUnbalancedTx (RawModTxAfterBalancing _) tx = tx
applyRawModOnUnbalancedTx (RawModTxBeforeBalancing f) tx = (Pl.tx %~ f) tx

instance Eq RawModTx where
  Id == Id = True
  _ == _ = False

instance Show RawModTx where
  show Id = "Id"
  show (RawModTxAfterBalancing _) = "RawModTxAfterBalancing"
  show (RawModTxBeforeBalancing _) = "RawModTxBeforeBalancing"

-- | Specifies how to select the collateral input
data Collateral
  = -- | Will select the first Ada-only UTxO we find belonging to 'ownPaymentPubKeyHash'
    CollateralAuto
  | -- | Will use the 'Pl.TxOutRef's given in the list. This list can be empty, in which case
    --  no collateral will be used whatsoever.
    CollateralUtxos [Pl.TxOutRef]
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
        collateral = CollateralAuto,
        balanceOutputPolicy = AdjustExistingOutput
      }
