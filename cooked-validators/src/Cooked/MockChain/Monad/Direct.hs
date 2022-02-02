{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Direct where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Cooked.MockChain.Monad
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Balance
import Cooked.Tx.Constraints
import Data.Bifunctor (Bifunctor (first, second))
import Data.Default
import Data.Foldable (asum)
import Data.Function (on)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Data.Void
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Constraints.OffChain as Pl
import qualified Ledger.Credential as Pl
import Ledger.Orphans ()
import qualified Ledger.TimeSlot as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl

-- * Direct Emulation

-- $mockchaindocstr
--
-- The MockChainT monad provides a direct emulator; that is, it gives us a simple way to call
-- validator scripts directly, without the need for all the complexity the 'Contract'
-- monad introduces.
--
-- Running a 'MockChain' produces a 'UtxoState', which is a map from 'Pl.Address' to
-- @(Pl.Value, Maybe Pl.Datum)@, and corresponds to the utxo mental model most people have.
-- Internally, however, we keep a 'Pl.UtxoIndex' in our state and feeding it to 'Pl.validateTx'.
-- For convenience, we also keep a map of 'Pl.Address' to 'Pl.Datum', giving is a simple
-- way of managing the current utxo state.

mcstToUtxoState :: MockChainSt -> UtxoState
mcstToUtxoState s =
  UtxoState . M.fromListWith (<>) . map (uncurry go1) . M.toList . Pl.getIndex . mcstIndex $ s
  where
    go1 :: Pl.TxOutRef -> Pl.TxOut -> (Pl.Address, UtxoValueSet)
    go1 _ (Pl.TxOut addr val mdh) = do
      (addr, UtxoValueSet [(val, mdh >>= go2)])

    go2 :: Pl.DatumHash -> Maybe UtxoDatum
    go2 datumHash = do
      datumStr <- M.lookup datumHash (mcstStrDatums s)
      datum <- M.lookup datumHash (mcstDatums s)
      return $ UtxoDatum datum datumStr

-- | Slightly more concrete version of 'UtxoState', used to actually run the simulation.
--  We keep a map from datum hash to datum, then a map from txOutRef to datumhash
--  Additionally, we also keep a map from datum hash to the underlying value's "show" result,
--  in order to display the contents of the state to the user.
data MockChainSt = MockChainSt
  { mcstIndex :: Pl.UtxoIndex,
    mcstDatums :: M.Map Pl.DatumHash Pl.Datum,
    mcstStrDatums :: M.Map Pl.DatumHash String,
    mcstCurrentSlot :: Pl.Slot
  }
  deriving (Show)

instance Default Pl.Slot where
  def = Pl.Slot 0

-- | The errors that can be produced by the 'MockChainT' monad
data MockChainError
  = MCEValidationError Pl.ValidationErrorInPhase
  | MCETxError Pl.MkTxError
  | MCEUnbalanceable Pl.Tx BalanceTxRes
  | FailWith String
  deriving (Show, Eq)

data MockChainEnv = MockChainEnv
  { mceSlotConfig :: Pl.SlotConfig,
    mceSigners :: NE.NonEmpty Wallet
  }
  deriving (Show)

instance Default MockChainEnv where
  def = MockChainEnv def (wallet 1 NE.:| [])

-- | The actual 'MockChainT' is a trivial combination of 'StateT' and 'ExceptT'
newtype MockChainT m a = MockChainT
  {unMockChain :: ReaderT MockChainEnv (StateT MockChainSt (ExceptT MockChainError m)) a}
  deriving newtype (Functor, Applicative, MonadState MockChainSt, MonadError MockChainError, MonadReader MockChainEnv)

-- | Non-transformer variant
type MockChain = MockChainT Identity

-- Custom monad instance made to increase the slot count automatically
instance (Monad m) => Monad (MockChainT m) where
  return = pure
  MockChainT x >>= f = MockChainT $ x >>= unMockChain . f

instance (Monad m) => MonadFail (MockChainT m) where
  fail = throwError . FailWith

instance MonadTrans MockChainT where
  lift = MockChainT . lift . lift . lift

instance (Monad m, Alternative m) => Alternative (MockChainT m) where
  empty = MockChainT $ ReaderT $ const $ StateT $ const $ ExceptT empty
  (<|>) = combineMockChainT (<|>)

combineMockChainT ::
  (Monad m) =>
  (forall a. m a -> m a -> m a) ->
  MockChainT m x ->
  MockChainT m x ->
  MockChainT m x
combineMockChainT f ma mb = MockChainT $
  ReaderT $ \r ->
    StateT $ \s ->
      let resA = runExceptT $ runStateT (runReaderT (unMockChain ma) r) s
          resB = runExceptT $ runStateT (runReaderT (unMockChain mb) r) s
       in ExceptT $ f resA resB

mapMockChainT ::
  (m (Either MockChainError (a, MockChainSt)) -> n (Either MockChainError (b, MockChainSt))) ->
  MockChainT m a ->
  MockChainT n b
mapMockChainT f = MockChainT . mapReaderT (mapStateT (mapExceptT f)) . unMockChain

-- | Executes a 'MockChainT' from some initial state and environment; does /not/
-- convert the 'MockChainSt' into a 'UtxoState'.
runMockChainTRaw ::
  (Monad m) =>
  MockChainEnv ->
  MockChainSt ->
  MockChainT m a ->
  m (Either MockChainError (a, MockChainSt))
runMockChainTRaw e0 i0 =
  runExceptT
    . flip runStateT i0
    . flip runReaderT e0
    . unMockChain

-- | Executes a 'MockChainT' from an initial state set up with the given initial value distribution.
-- Similar to 'runMockChainT', uses the default environment. Returns a 'UtxoState' instead of
-- a 'MockChainSt'. If you need the later, use 'runMockChainTRaw'
runMockChainTFrom ::
  (Monad m) =>
  InitialDistribution ->
  MockChainT m a ->
  m (Either MockChainError (a, UtxoState))
runMockChainTFrom i0 =
  fmap (fmap $ second mcstToUtxoState) . runMockChainTRaw def (mockChainSt0From i0)

-- | Executes a 'MockChainT' from the cannonical initial state and environment. The cannonical
--  environment uses the default 'SlotConfig' and @[Cooked.MockChain.Wallet.wallet 1]@ as the sole
--  wallet signing transactions.
runMockChainT :: (Monad m) => MockChainT m a -> m (Either MockChainError (a, UtxoState))
runMockChainT = runMockChainTFrom def

-- | See 'runMockChainTRaw'
runMockChainRaw :: MockChainEnv -> MockChainSt -> MockChain a -> Either MockChainError (a, MockChainSt)
runMockChainRaw e0 i0 = runIdentity . runMockChainTRaw e0 i0

-- | See 'runMockChainTFrom'
runMockChainFrom ::
  InitialDistribution -> MockChain a -> Either MockChainError (a, UtxoState)
runMockChainFrom i0 = runIdentity . runMockChainTFrom i0

-- | See 'runMockChainT'
runMockChain :: MockChain a -> Either MockChainError (a, UtxoState)
runMockChain = runIdentity . runMockChainT


-- Canonical initial values

utxoState0 :: UtxoState
utxoState0 = mcstToUtxoState mockChainSt0

mockChainSt0 :: MockChainSt
mockChainSt0 = MockChainSt utxoIndex0 M.empty M.empty def

mockChainSt0From :: InitialDistribution -> MockChainSt
mockChainSt0From i0 = MockChainSt (utxoIndex0From i0) M.empty M.empty def

instance Default MockChainSt where
  def = mockChainSt0

utxoIndex0From :: InitialDistribution -> Pl.UtxoIndex
utxoIndex0From i0 = Pl.initialise [[Pl.Valid $ initialTxFor i0]]

utxoIndex0 :: Pl.UtxoIndex
utxoIndex0 = utxoIndex0From def

-- ** Direct Interpretation of Operations

instance (Monad m) => MonadBlockChain (MockChainT m) where
  validateTxSkel skel = do
    tx <- generateTx' skel
    txId <- validateTx' tx
    when (autoSlotIncrease $ txOpts skel) $ modify' (\st -> st {mcstCurrentSlot = mcstCurrentSlot st + 1})
    return txId

  txOutByRef outref = gets (M.lookup outref . Pl.getIndex . mcstIndex)

  ownPaymentPubKeyHash = asks (walletPKHash . NE.head . mceSigners)

  utxosSuchThat = utxosSuchThat'

  currentSlot = gets mcstCurrentSlot

  currentTime = asks (Pl.slotToBeginPOSIXTime . mceSlotConfig) <*> gets mcstCurrentSlot

  awaitSlot s = modify' (\st -> st {mcstCurrentSlot = max s (mcstCurrentSlot st)}) >> currentSlot

  awaitTime t = do
    sc <- asks mceSlotConfig
    s <- awaitSlot (1 + Pl.posixTimeToEnclosingSlot sc t)
    return $ Pl.slotToBeginPOSIXTime sc s

instance (Monad m) => MonadMockChain (MockChainT m) where
  signingWith ws = local $ \env -> env {mceSigners = ws}

  askSigners = asks mceSigners

-- | Check 'validateTx' for details
validateTx' :: (Monad m) => Pl.Tx -> MockChainT m Pl.TxId
validateTx' tx = do
  s <- currentSlot
  ix <- gets mcstIndex
  slotCfg <- asks mceSlotConfig
  let res = Pl.runValidation (Pl.validateTransaction s tx) (Pl.ValidationCtx ix slotCfg)
  -- case trace (show $ snd res) $ fst res of
  case fst res of
    (Just err, _) -> throwError (MCEValidationError err)
    (Nothing, ix') -> do
      -- Validation succeeded; now we update the indexes and the managed datums.
      -- The new mcstIndex is just `ix'`; the new mcstDatums is computed by
      -- removing the datum hashes have been consumed and adding
      -- those that have been created in `tx`.
      let consumedIns = map Pl.txInRef $ S.toList (Pl.txInputs tx) ++ S.toList (Pl.txCollateral tx)
      consumedDHs <- catMaybes <$> mapM (fmap Pl.txOutDatumHash . outFromOutRef) consumedIns
      let consumedDHs' = M.fromList $ zip consumedDHs (repeat ())
      modify'
        ( \st ->
            st
              { mcstIndex = ix',
                mcstDatums = (mcstDatums st `M.difference` consumedDHs') `M.union` Pl.txData tx
              }
        )
      return $ Pl.txId tx

-- | Check 'utxosSuchThat' for details
utxosSuchThat' ::
  forall a m.
  (Monad m, Pl.FromData a) =>
  Pl.Address ->
  (Maybe a -> Pl.Value -> Bool) ->
  MockChainT m [(SpendableOut, Maybe a)]
utxosSuchThat' addr datumPred = do
  ix <- gets (Pl.getIndex . mcstIndex)
  let ix' = M.filter ((== addr) . Pl.txOutAddress) ix
  mapMaybe (fmap assocl . rstr) <$> mapM (\(oref, out) -> (oref,) <$> go oref out) (M.toList ix')
  where
    go :: Pl.TxOutRef -> Pl.TxOut -> MockChainT m (Maybe (Pl.ChainIndexTxOut, Maybe a))
    go oref (Pl.TxOut oaddr val mdatumH) =
      case Pl.addressCredential oaddr of
        -- A PK credential has no datum; just check whether we want to select this output or not.
        Pl.PubKeyCredential _ ->
          if datumPred Nothing val
            then return . Just $ (Pl.PublicKeyChainIndexTxOut oaddr val, Nothing)
            else return Nothing
        -- A script credential, on the other hand, must have a datum. Hence, we'll go look on our map of
        -- managed datum for a relevant datum, try to convert it to a value of type @a@ then see
        -- if the user wants to select said output.
        Pl.ScriptCredential (Pl.ValidatorHash vh) -> do
          managedDatums <- gets mcstDatums
          datumH <- maybe (fail $ "ScriptCredential with no datum hash: " ++ show oref) return mdatumH
          datum <-
            maybe
              (fail $ "Unmanaged datum with hash: " ++ show datumH ++ " at: " ++ show oref)
              return
              $ M.lookup datumH managedDatums
          a <-
            maybe
              (fail $ "Can't convert from builtin data at: " ++ show oref ++ "; are you sure this is the right type?")
              return
              (Pl.fromBuiltinData (Pl.getDatum datum))
          if datumPred (Just a) val
            then return . Just $ (Pl.ScriptChainIndexTxOut oaddr (Left $ Pl.ValidatorHash vh) (Right datum) val, Just a)
            else return Nothing

-- | Generates an unbalanced transaction from a skeleton; A
--  transaction is unbalanced whenever @inputs + mints != outputs + fees@.
--  In order to submit a transaction, it must be balanced, otherwise
--  we will see a @ValueNotPreserved@ error.
--
--  See "Cooked.Tx.Balance" for balancing capabilities or stick to
--  'generateTx', which generates /and/ balances a transaction.
generateUnbalTx :: TxSkel -> Either MockChainError Pl.UnbalancedTx
generateUnbalTx sk =
  let (lkups, constrs) = toLedgerConstraints @Void $ txConstraints sk
   in first MCETxError $ Pl.mkTx lkups constrs

-- | Check 'generateTx' for details
generateTx' :: (Monad m) => TxSkel -> MockChainT m Pl.Tx
generateTx' skel = do
  modify $ updateDatumStr skel
  case generateUnbalTx skel of
    Left err -> throwError err
    Right ubtx -> do
      let adjust = if adjustUnbalTx opts then Pl.adjustUnbalancedTx else id
      signers <- askSigners
      balancedTx <- balanceTxFrom (not $ balance opts) (NE.head signers) (adjust ubtx)
      return $
        foldl
          (flip txAddSignature)
          -- optionally apply a transformation to a balanced tx before sending it in.
          (applyRawModTx (unsafeModTx opts) balancedTx)
          (NE.toList signers)
  where
    opts = txOpts skel

    -- Update the map of pretty printed representations in the mock chain state
    updateDatumStr :: TxSkel -> MockChainSt -> MockChainSt
    updateDatumStr TxSkel {txConstraints} st@MockChainSt {mcstStrDatums} =
      st
        { mcstStrDatums =
            M.unions $
              mcstStrDatums : (extractDatumStrFromConstraint <$> txConstraints)
        }

-- | Sets the 'Pl.txFee' and 'Pl.txValidRange' according to our environment.
-- TODO: bring in FeeConfig: https://github.com/tweag/plutus-libs/issues/10
setFeeAndValidRange :: (Monad m) => Pl.UnbalancedTx -> MockChainT m Pl.Tx
setFeeAndValidRange (Pl.UnbalancedTx tx0 _reqSigs _uindex slotRange) = do
  let tx = tx0 {Pl.txFee = Pl.minFee tx0}
  config <- asks mceSlotConfig
  return
    tx {Pl.txValidRange = Pl.posixTimeRangeToContainedSlotRange config slotRange}

balanceTxFrom :: (Monad m) => Bool -> Wallet -> Pl.UnbalancedTx -> MockChainT m Pl.Tx
balanceTxFrom skipBalancing w ubtx = do
  tx <- setFeeAndValidRange ubtx
  if skipBalancing
    then return tx
    else do
      bres <- calcBalanceTx w tx
      case applyBalanceTx w bres tx of
        Just tx' -> return tx'
        Nothing -> throwError $ MCEUnbalanceable tx bres

data BalanceTxRes = BalanceTxRes
  { newInputs :: [Pl.TxOutRef],
    returnValue :: Pl.Value,
    remainderUtxos :: [(Pl.TxOutRef, Pl.TxOut)]
  }
  deriving (Eq, Show)

-- | Calculate the changes needed to balance a transaction with money from a given wallet.
-- Every transaction that is sent to the chain must be balanced, that is: @inputs + mint == outputs + fee@.
calcBalanceTx :: (Monad m) => Wallet -> Pl.Tx -> MockChainT m BalanceTxRes
calcBalanceTx w tx = do
  -- We start by gathering all the inputs and summing it
  lhsInputs <- mapM (outFromOutRef . Pl.txInRef) (S.toList (Pl.txInputs tx))
  let lhs = mappend (mconcat $ map Pl.txOutValue lhsInputs) (Pl.txMint tx)
  let rhs = mappend (mconcat $ map Pl.txOutValue $ Pl.txOutputs tx) (Pl.txFee tx)
  let wPKH = walletPKHash w
  let usedInTxIns = S.map Pl.txInRef (Pl.txInputs tx)
  allUtxos <- pkUtxos' wPKH
  -- It is important that we only consider utxos that have not been spent in the transaction as "available"
  let availableUtxos = filter ((`S.notMember` usedInTxIns) . fst) allUtxos
  let (usedUTxOs, leftOver, excess) = balanceWithUTxOs (rhs Pl.- lhs) availableUtxos
  return $
    BalanceTxRes
      { -- Now, we will add the necessary utxos to the transaction,
        newInputs = usedUTxOs,
        -- Pay to wPKH whatever is leftOver from newTxIns and whatever was excessive to begin with
        returnValue = leftOver <> excess,
        -- We also return the remainder utxos that could still be used in case
        -- we can't 'applyBalanceTx' this 'BalanceTxRes'.
        remainderUtxos = filter ((`L.notElem` usedUTxOs) . fst) availableUtxos
      }

-- | Once we calculated what is needed to balance a transaction @tx@, we still need to
-- apply those changes to @tx@. Because of the 'Ledger.minAdaTxOut' constraint, this
-- might not be possible: imagine the leftover is less than 'Ledger.minAdaTxOut', but
-- the transaction has no output addressed to the sending wallet. If we just
-- create a new ouput for @w@ and place the leftover there the resulting tx will fail to validate
-- with "LessThanMinAdaPerUTxO" error. Instead, we need to consume yet another UTxO belonging to @w@ to
-- then create the output with the proper leftover. If @w@ has no UTxO, then there's no
-- way to balance this transaction.
applyBalanceTx :: Wallet -> BalanceTxRes -> Pl.Tx -> Maybe Pl.Tx
applyBalanceTx w (BalanceTxRes newTxIns leftover remainders) tx = do
  -- Here we'll try a few things, in order, until one of them succeeds:
  --   1. pick out the best possible output to adjust and adjust it as long as it remains with
  --      more than 'Pl.minAdaTxOut'. No need for additional inputs.
  --   2. if the leftover is more than 'Pl.minAdaTxOut' and (1) wasn't possible, create a new output
  --      to return leftover. No need for additional inputs.
  --   3. Attempt to consume other possible utxos from 'w' in order to combine them
  --      and return the leftover.
  (txInsDelta, txOuts') <-
    asum $
      [ wOutsBest >>= fmap ([],) . adjustOutputValueAt (<> leftover) (Pl.txOutputs tx), -- 1.
        guard (isAtLeastMinAda leftover) >> return ([], Pl.txOutputs tx ++ [mkOutWithVal leftover]) -- 2.
      ]
        ++ map (fmap (second (Pl.txOutputs tx ++)) . consumeRemainder) (sortByMoreAda remainders) -- 3.
  let newTxIns' = S.fromList $ map (`Pl.TxIn` Just Pl.ConsumePublicKeyAddress) (newTxIns ++ txInsDelta)
  return $
    tx
      { Pl.txInputs = Pl.txInputs tx <> newTxIns',
        Pl.txOutputs = txOuts'
      }
  where
    wPKH = walletPKHash w
    mkOutWithVal v = Pl.TxOut (Pl.Address (Pl.PubKeyCredential wPKH) Nothing) v Nothing

    -- The best output to attempt and modify, if any, is the one with the most ada,
    -- which is at the head of wOutsIxSorted:
    wOutsBest = fst <$> L.uncons wOutsIxSorted

    -- The indexes of outputs belonging to w sorted by amount of ada.
    wOutsIxSorted :: [Int]
    wOutsIxSorted =
      map fst $
        sortByMoreAda $
          filter ((== Just wPKH) . addressIsPK . Pl.txOutAddress . snd) $
            zip [0 ..] (Pl.txOutputs tx)

    sortByMoreAda :: [(a, Pl.TxOut)] -> [(a, Pl.TxOut)]
    sortByMoreAda = L.sortBy (flip compare `on` (adaVal . Pl.txOutValue . snd))

    adaVal :: Pl.Value -> Integer
    adaVal = Pl.getLovelace . Pl.fromValue

    isAtLeastMinAda :: Pl.Value -> Bool
    isAtLeastMinAda v = adaVal v >= Pl.getLovelace Pl.minAdaTxOut

    adjustOutputValueAt :: (Pl.Value -> Pl.Value) -> [Pl.TxOut] -> Int -> Maybe [Pl.TxOut]
    adjustOutputValueAt f xs i =
      let (pref, Pl.TxOut addr val stak : rest) = L.splitAt i xs
          val' = f val
       in guard (isAtLeastMinAda val') >> return (pref ++ Pl.TxOut addr val' stak : rest)

    -- Given a list of available utxos; attept to consume them if they would enable the returning
    -- of the leftover.
    consumeRemainder :: (Pl.TxOutRef, Pl.TxOut) -> Maybe ([Pl.TxOutRef], [Pl.TxOut])
    consumeRemainder (remRef, remOut) =
      let v = leftover <> Pl.txOutValue remOut
       in guard (isAtLeastMinAda v) >> return ([remRef], [mkOutWithVal v])

-- * Utilities

addressIsPK :: Pl.Address -> Maybe Pl.PubKeyHash
addressIsPK addr = case Pl.addressCredential addr of
  Pl.PubKeyCredential pkh -> Just pkh
  _ -> Nothing

rstr :: (Monad m) => (a, m b) -> m (a, b)
rstr (a, mb) = (a,) <$> mb

assocl :: (a, (b, c)) -> ((a, b), c)
assocl (a, (b, c)) = ((a, b), c)
