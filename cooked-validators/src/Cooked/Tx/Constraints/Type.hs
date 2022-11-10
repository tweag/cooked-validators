{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cooked.Tx.Constraints.Type where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ledger as Pl
import qualified Ledger.Typed.Scripts as Pl
import Optics.TH
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx.Prelude as Pl
import Type.Reflection

-- for template Haskell reasons, the most intersting thing in this module (the
-- definition of 'TxSkel') is at the very bottom of this file

-- * 'SpendableOut': The type of UTxOs

-- | A 'SpendableOut' is an outref that is ready to be spend; with its
--  underlying 'Pl.ChainIndexTxOut'.
-- data SpendableOut = SpendableOut Pl.TxOutRef Pl.ChainIndexTxOut deriving (Eq, Ord)
data SpendableOut deriving (Eq, Ord) -- TODO undefined for now

-- -- | Accesses the 'Pl.Value' within a 'SpendableOut'
-- sOutValue :: SpendableOut -> Pl.Value
-- sOutValue = Pl.txOutValue . Pl.toTxOut . snd

-- -- | Accesses the 'Pl.Address' within a 'SpendableOut'
-- sOutAddress :: SpendableOut -> Pl.Address
-- sOutAddress = Pl.txOutAddress . Pl.toTxOut . snd

-- -- | Accesses a potential 'Pl.DatumHash' within a 'SpendableOut'; note that
-- --  the existence (or not) of a datum hash /DOES NOT/ indicate the 'SpendableOut'
-- --  belongs to a script or a public key; you must pattern match on the result of
-- --  'sOutAddress' or use one of 'sBelongsToPubKey' or 'sBelongsToScript' to distinguish that.
-- sOutDatumHash :: SpendableOut -> Maybe Pl.DatumHash
-- sOutDatumHash = Pl.txOutDatum . Pl.toTxOut . snd

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

spOutDatum :: SpendableOut -> Maybe Pl.Datum
spOutDatum = undefined

spOutCITxOut :: SpendableOut -> Pl.ChainIndexTxOut
spOutCITxOut = undefined

spOutTxOutRef :: SpendableOut -> Pl.TxOutRef
spOutTxOutRef = undefined

-- * Transaction labels

type LabelConstrs x = (Show x, Typeable x, Eq x, Ord x)

data TxLabel where
  TxLabel :: LabelConstrs x => x -> TxLabel

instance Eq TxLabel where
  a == x = compare a x == EQ

instance Ord TxLabel where
  (TxLabel a) <= (TxLabel x) =
    SomeTypeRep (typeOf a) <= SomeTypeRep (typeOf x)
      || case typeOf a `eqTypeRep` typeOf x of
        Just HRefl -> a <= x
        Nothing -> False

-- * Transaction options

data TxOpts deriving (Eq) -- undefined for now

instance Semigroup TxOpts

instance Monoid TxOpts

-- * Minting Constraints

-- -- | Helper for heterogeneous comparisons. This works by first comparing the
-- -- type representations, and only comparing non-heterogeneously when they are
-- -- the same.
-- compareHet :: (Typeable t, Typeable a, Typeable b, Ord (t a)) => t a -> t b -> Ordering
-- compareHet x y =
--   let tx = typeOf x
--       ty = typeOf y
--    in case compare (SomeTypeRep tx) (SomeTypeRep ty) of
--         LT -> LT
--         GT -> GT
--         EQ -> case tx `eqTypeRep` ty of
--           Just HRefl -> compare x y
--           Nothing -> LT -- this case can't happen

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
  deriving (Eq)

makeLenses ''TxSkel

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

-- instance Eq TxSkel where
--   TxSkel l1 o1 c1 == TxSkel l2 o2 c2 =
--     case l1 ~*~? l2 of
--       Just HRefl -> (l1, o1, c1) == (l2, o2, c2)
--       _ -> False

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

-- * Constraint Types

-- ** Minting Constraints

-- type MintsConstraint =

-- type SpendsConstrs a =
--   ( Pl.ToData (Pl.DatumType a),
--     Pl.ToData (Pl.RedeemerType a),
--     Pl.UnsafeFromData (Pl.DatumType a),
--     Show (Pl.DatumType a),
--     Show (Pl.RedeemerType a),
--     Pl.Eq (Pl.DatumType a),
--     Pl.Eq (Pl.RedeemerType a),
--     Typeable a
--   )

-- -- | Constraints which do not specify new transaction outputs
-- data MiscConstraint where
--   -- | Ensure that the given 'Pl.TypedValidator' spends a specific UTxO (which
--   -- must belong to the validator). That is: Unlock the UTxO described by the
--   -- given 'SpendableOut', passing the given redeemer to the validator script.
--   SpendsScript ::
--     (SpendsConstrs a) =>
--     Pl.TypedValidator a ->
--     Pl.RedeemerType a ->
--     -- | Utxo to spend.
--     -- WARNING: A "SpendableOut" contains a "ChainIndexTxOut" that contains
--     -- either a datum or its hash. Spending a script "SpendableOut" which does
--     -- not contain the datum explicitly causes the generated transaction to
--     -- fail (datum not found).
--     --
--     -- This does not occur in practice when spending UTxOs obtained by
--     -- searching through the chain with "scriptUtxosSuchThat", or those
--     -- extracted from "CardanoTx" by using "spOutsFromCardanoTx".
--     SpendableOut ->
--     MiscConstraint
--   -- | Ensure that a 'Pl.PubKeyHash' spends a specific UTxO. The hash is not an
--   -- argument since it can be read off the given 'SpendableOut'.
--   SpendsPK :: SpendableOut -> MiscConstraint
--   -- | Ensure that the transaction mints the given 'Pl.Value'. For each
--   -- 'Pl.CurrencySymbol' in the value that is to be minted, the given list of
--   -- 'Pl.MintingPolicy's has to include the policy governing that currency
--   -- symbol. The @Maybe a@-argument is the optional redeemer passed to the
--   -- policies: If you pass @Nothing@, the constraint is translated using
--   -- 'Pl.mustMintValue', which uses no redeemer; if you pass @Just r@,
--   -- 'Pl.mustMintValueWithRedeemer' is used to pass @r@ as a redeemer to the
--   -- policies.
--   Mints ::
--     MintsConstrs a =>
--     Maybe a ->
--     [Pl.MintingPolicy] ->
--     Pl.Value ->
--     MiscConstraint
--   -- | Ensure that the transaction happens no later than the given time (the end
--   -- time is included in the allowed range).
--   Before :: Pl.POSIXTime -> MiscConstraint
--   -- | Ensure that the transaction happens no earlier than the given time (the
--   -- start time is included in the allowed range).
--   After :: Pl.POSIXTime -> MiscConstraint
--   -- | Ensure that the transaction happens in the given time range.
--   ValidateIn :: Pl.POSIXTimeRange -> MiscConstraint
--   -- | Ensure that the transaction is signed by the given 'Pl.PubKeyHash'es.
--   SignedBy :: [Pl.PubKeyHash] -> MiscConstraint

-- -- NB don't forget to update the Eq instance when adding new constructors

-- (~*~?) :: forall ty a1 a2. (Typeable a1, Typeable a2) => ty a1 -> ty a2 -> Maybe (a1 :~~: a2)
-- _ ~*~? _ = typeRep @a1 `eqTypeRep` typeRep @a2

-- instance Eq MiscConstraint where
--   SpendsScript s1 r1 so1 == SpendsScript s2 r2 so2 =
--     case s1 ~*~? s2 of
--       Just HRefl -> (s1, so1) == (s2, so2) && r1 Pl.== r2
--       Nothing -> False
--   SpendsPK so1 == SpendsPK so2 = so1 == so2
--   Mints d1 p1 v1 == Mints d2 p2 v2 =
--     case d1 ~*~? d2 of
--       Just HRefl -> (p1, v1) == (p2, v2) && d1 Pl.== d2
--       Nothing -> False
--   Before t1 == Before t2 = t1 == t2
--   After t1 == After t2 = t1 == t2
--   ValidateIn r1 == ValidateIn r2 = r1 == r2
--   SignedBy hs1 == SignedBy hs2 = hs1 == hs2
--   _ == _ = False

-- -- | Constraints which specify new transaction outputs
-- data OutConstraint where
--   -- | Creates an UTxO to the given validator, with an optional
--   -- staking credential, and with the given datum and the given
--   -- value. That is, lets the script lock the given value.
--   PaysScript ::
--     (PaysScriptConstrs a) =>
--     Pl.TypedValidator a ->
--     Maybe Pl.StakingCredential ->
--     Pl.DatumType a ->
--     Pl.Value ->
--     OutConstraint
--   -- | Creates a UTxO to a specific 'Pl.PubKeyHash' with a potential 'Pl.StakePubKeyHash'.
--   -- and datum. If the stake pk is present, it will call 'Ledger.Constraints.OffChain.ownStakePubKeyHash'
--   -- to update the script lookups, hence, generating a transaction with /two/ 'PaysPKWithDatum' with
--   -- two different staking keys will cause the first staking key to be overriden by calling @ownStakePubKeyHash@
--   -- a second time. If this is an issue for you, please do submit an issue with an explanation on GitHub.
--   PaysPKWithDatum ::
--     (Pl.ToData a, Pl.Eq a, Show a, Typeable a) =>
--     Pl.PubKeyHash ->
--     Maybe Pl.StakePubKeyHash ->
--     Maybe a ->
--     Pl.Value ->
--     OutConstraint

-- -- NB don't forget to update the Eq instance when adding new constructors

-- instance Eq OutConstraint where
--   PaysScript s1 sc1 d1 v1 == PaysScript s2 sc2 d2 v2 =
--     case s1 ~*~? s2 of
--       Just HRefl -> (s1, v1) == (s2, v2) && sc1 Pl.== sc2 && d1 Pl.== d2
--       Nothing -> False
--   PaysPKWithDatum pk1 stake1 d1 v1 == PaysPKWithDatum pk2 stake2 d2 v2 =
--     case d1 ~*~? d2 of
--       Just HRefl -> (pk1, stake1, v1) == (pk2, stake2, v2) && d1 Pl.== d2
--       Nothing -> False
--   _ == _ = False

-- -- | This typeclass provides user-friendly convenience to overload the
-- -- 'txConstraints' field of 'TxSkel'. For instance, this enables us to ommit the '(:=>:)'
-- -- constructor whenever we're using only one kind of constraints.
-- -- It also opens up future alternative of constraint specification.
-- class (Eq a, Typeable a) => ConstraintsSpec a where
--   toConstraints :: a -> Constraints

-- instance ConstraintsSpec Constraints where
--   toConstraints = id

-- instance ConstraintsSpec [MiscConstraint] where
--   toConstraints = (:=>: [])

-- instance ConstraintsSpec MiscConstraint where
--   toConstraints = toConstraints . (: [])

-- instance ConstraintsSpec [OutConstraint] where
--   toConstraints = ([] :=>:)

-- instance ConstraintsSpec OutConstraint where
--   toConstraints = toConstraints . (: [])

-- paysPK :: Pl.PubKeyHash -> Pl.Value -> OutConstraint
-- paysPK pkh = PaysPKWithDatum @() pkh Nothing Nothing

-- paysScript :: (PaysScriptConstrs a) => Pl.TypedValidator a -> Pl.DatumType a -> Pl.Value -> OutConstraint
-- paysScript tv = PaysScript tv Nothing

-- mints :: [Pl.MintingPolicy] -> Pl.Value -> MiscConstraint
-- mints = Mints @() Nothing

-- type LabelConstrs x = (Show x, Typeable x, Eq x)

-- -- | Set of options to modify the behavior of generating and validating some transaction. Some of these
-- -- options only have an effect when running in the 'Plutus.Contract.Contract', some only have an effect when
-- -- running in 'MockChainT'. If nothing is explicitely stated, the option has an effect independently of the
-- -- running context.
-- data TxOpts = TxOpts
--   { -- | Performs an adjustment to unbalanced txs, making sure every UTxO that is produced
--     --  has the necessary minimum amount of Ada.
--     --
--     -- By default, this is set to @False@, given this is the default behavior in Plutus:
--     -- https://github.com/input-output-hk/plutus-apps/issues/143#issuecomment-1013012744
--     adjustUnbalTx :: Bool,
--     -- | When submitting a transaction for real (i.e., running in the 'Plutus.Contract.Contract' monad),
--     --  it is common to call 'Plutus.Contract.Request.awaitTxConfirmed' after 'Plutus.Contract.Request.submitTxConstraints'.
--     --  If you /do NOT/ wish to do so, please set this to @False@.
--     --
--     --  /This has NO effect when running outside of 'Plutus.Contract.Contract'/.
--     --  By default, this is set to @True@.
--     awaitTxConfirmed :: Bool,
--     -- | Whether to increase the slot counter automatically on this submission.
--     -- This is useful for modelling transactions that could be submitted in parallel in reality, so there
--     -- should be no explicit ordering of what comes first. One good example is in the Crowdfunding use case contract.
--     --
--     -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
--     --  By default, this is set to @True@.
--     autoSlotIncrease :: Bool,
--     -- | Reorders the transaction outputs to fit the ordering of output
--     -- constraints. Those outputs are put at the very beginning of the list.
--     -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
--     --  By default, this is set to @True@.
--     forceOutputOrdering :: Bool,
--     -- | Applies an arbitrary modification to a transaction after it has been pottentially adjusted ('adjustUnbalTx')
--     -- and balanced. This is prefixed with /unsafe/ to draw attention that modifying a transaction at
--     -- that stage might make it invalid. Still, this offers a hook for being able to alter a transaction
--     -- in unforeseen ways. It is mostly used to test contracts that have been written for custom PABs.
--     --
--     -- One interesting use of this function is to observe a transaction just before it is being
--     -- sent for validation, with @unsafeModTx = RawModTxAfterBalancing Debug.Trace.traceShowId@.
--     --
--     -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
--     -- By default, this is set to 'Id'.
--     unsafeModTx :: RawModTx,
--     -- | Whether to balance the transaction or not. Balancing is the process of ensuring that
--     --  @input + mint = output + fees@, if you decide to set @balance = false@ you will have trouble
--     -- satisfying that equation by hand because @fees@ are variable. You will likely see a @ValueNotPreserved@ error
--     -- and should adjust the fees accordingly. For now, there is no option to skip the fee computation because
--     -- without it, validation through "Ledger.Validation" would fail with @InsufficientFees@.
--     --
--     -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
--     -- By default, this is set to @True@.
--     balance :: Bool,
--     -- | Which collateral utxo to use for this transaction. A collateral UTxO must be an Ada-only utxo
--     -- and can be specified manually, or it can be chosen automatically, if any is available.
--     --
--     -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
--     -- By default, this is set to @CollateralAuto@.
--     collateral :: Collateral,
--     -- | The 'BalanceOutputPolicy' to apply when balancing the transaction.
--     --
--     -- /This has NO effect when running in 'Plutus.Contract.Contract'/.
--     -- By default, this is set to @AdjustExistingOutput@.
--     balanceOutputPolicy :: BalanceOutputPolicy
--   }
--   deriving (Eq, Show)

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

-- -- IMPORTANT INTERNAL: If you add or remove fields from 'TxOpts', make sure
-- -- to update the internal @fields@ value from 'Cooked.Tx.Constraints.Pretty'

-- -- | Wraps a function that can be applied to a transaction right before submitting it.
-- --  We have a distinguished datatype to be able to provide a little more info on
-- --  the show instance.
-- data RawModTx
--   = -- | no effect modifier
--     Id
--   | -- | Apply modification on transaction after balancing is performed
--     RawModTxAfterBalancing (Pl.Tx -> Pl.Tx)
--   | -- | Apply modification on transaction before balancing and transaction fee computation
--     --   are performed.
--     RawModTxBeforeBalancing (Pl.Tx -> Pl.Tx)

-- -- | only applies modification for RawModTxAfterBalancing
-- applyRawModOnBalancedTx :: RawModTx -> Pl.Tx -> Pl.Tx
-- applyRawModOnBalancedTx Id tx = tx
-- applyRawModOnBalancedTx (RawModTxAfterBalancing f) tx = f tx
-- applyRawModOnBalancedTx (RawModTxBeforeBalancing _) tx = tx

-- -- | only applies modification for RawModTxBeforeBalancing
-- applyRawModOnUnbalancedTx :: RawModTx -> Pl.UnbalancedTx -> Pl.UnbalancedTx
-- applyRawModOnUnbalancedTx Id tx = tx
-- applyRawModOnUnbalancedTx (RawModTxAfterBalancing _) tx = tx
-- applyRawModOnUnbalancedTx (RawModTxBeforeBalancing f) tx = (Pl.tx %~ f) tx

-- instance Eq RawModTx where
--   Id == Id = True
--   _ == _ = False

-- instance Show RawModTx where
--   show Id = "Id"
--   show (RawModTxAfterBalancing _) = "RawModTxAfterBalancing"
--   show (RawModTxBeforeBalancing _) = "RawModTxBeforeBalancing"

-- -- | Specifies how to select the collateral input
-- data Collateral
--   = -- | Will select the first Ada-only UTxO we find belonging to 'ownPaymentPubKeyHash'
--     CollateralAuto
--   | -- | Will use the 'Pl.TxOutRef's given in the list. This list can be empty, in which case
--     --  no collateral will be used whatsoever.
--     CollateralUtxos [Pl.TxOutRef]
--   deriving (Eq, Show)

-- instance Default TxOpts where
--   def =
--     TxOpts
--       { adjustUnbalTx = False,
--         awaitTxConfirmed = True,
--         autoSlotIncrease = True,
--         forceOutputOrdering = True,
--         unsafeModTx = Id,
--         balance = True,
--         collateral = CollateralAuto,
--         balanceOutputPolicy = AdjustExistingOutput
--       }
