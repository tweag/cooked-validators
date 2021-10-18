{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Cooked.MockChain where

import           Data.Void
import qualified Data.Map as M
import           Data.Maybe (mapMaybe, fromJust)
import           Control.Arrow (second)
import           Control.Monad.State

import qualified Ledger   as Pl
import qualified Ledger.Credential   as Pl
import qualified PlutusTx as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, RedeemerType, TypedValidator, validatorScript)

import Cooked.Tx.Constraints
import Cooked.MockChain.Base

-- |Returns a list of spendable outputs that belong to a given address and satisfy a given predicate;
-- Additionally, return the datum present in there if it happened to be a script output. It is important
-- to use @-XTypeApplications@ and pass a value for type variable @a@ below.
utxosSuchThat :: forall a m
               . (Monad m, Pl.FromData a)
              => Pl.Address -> (Maybe a -> Pl.Value -> Bool)
              -> MockChainT m [(SpendableOut, Maybe a)]
utxosSuchThat addr datumPred = do
    ix <- gets (Pl.getIndex . mcstIndex)
    let ix' = M.filter ((== addr) . Pl.txOutAddress) ix
    mapMaybe (fmap assocl . rstr) <$> mapM (\(oref, out) -> (oref,) <$> go oref out) (M.toList ix')
  where
    go :: Pl.TxOutRef -> Pl.TxOut -> MockChainT m (Maybe (Pl.ChainIndexTxOut, Maybe a))
    go oref (Pl.TxOut oaddr val mdatumH) =
      case Pl.addressCredential oaddr of
        -- A PK credential has no datum; just check whether we want to select this output or not.
        Pl.PubKeyCredential _  ->
          if datumPred Nothing val
          then return . Just $ (Pl.PublicKeyChainIndexTxOut oaddr val, Nothing)
          else return Nothing
        -- A script credential, on the other hand, must have a datum. Hence, we'll go look on our map of
        -- managed datum for a relevant datum, try to convert it to a value of type @a@ then see
        -- if the user wants to select said output.
        Pl.ScriptCredential (Pl.ValidatorHash vh) -> do
          managedDatums <- gets mcstDatums
          datumH <- maybe (fail $ "ScriptCredential with no datum hash: " ++ show oref) return mdatumH
          datum  <- maybe (fail $ "Unmanaged datum with hash: " ++ show datumH ++ " at: " ++ show oref)
                          return $ M.lookup datumH managedDatums
          a  <- maybe (fail $ "Can't convert from builtin data at: " ++ show oref ++"; are you sure this is the right type?")
                      return
                      (Pl.fromBuiltinData (Pl.getDatum datum))
          if datumPred (Just a) val
          then return . Just $ (Pl.ScriptChainIndexTxOut oaddr (Left $ Pl.ValidatorHash vh) (Right datum) val, Just a)
          else return Nothing

-- |Public-key UTxO's have no datum, hence, can be selected easily with
-- a simpler variant of 'utxosSuchThat'
pkUtxosSuchThat :: (Monad m) => Pl.PubKeyHash -> (Pl.Value -> Bool) -> MockChainT m [SpendableOut]
pkUtxosSuchThat pkh pred = map fst <$>
  utxosSuchThat @Void
    (Pl.Address (Pl.PubKeyCredential pkh) Nothing)
    (maybe pred absurd)

-- |Script UTxO's always have a datum, hence, can be selected easily with
-- a simpler variant of 'utxosSuchThat'. It is important to pass a value for type variable @a@
-- with an explicit type application to make sure the conversion to and from 'Pl.Datum' happens correctly.
scriptUtxosSuchThat :: forall tv m
                     . (Monad m, Pl.FromData (Pl.DatumType tv))
                    => Pl.TypedValidator tv
                    -> (Pl.DatumType tv -> Pl.Value -> Bool)
                    -> MockChainT m [(SpendableOut, Pl.DatumType tv)]
scriptUtxosSuchThat v pred = map (second fromJust) <$>
  utxosSuchThat
    (Pl.Address (Pl.ScriptCredential $ Pl.validatorHash $ Pl.validatorScript v) Nothing)
    (maybe (const False) pred)

rstr :: (Monad m) => (a , m b) -> m (a, b)
rstr (a, mb) = (a,) <$> mb

assocl :: (a, (b, c)) -> ((a, b) , c)
assocl (a, (b, c)) = ((a, b), c)

-- OLD COLD CODE BELOW; SHOULD DISAPPEAR LATER

{-
import Data.Void
import Data.Default
import Data.List (sortBy,groupBy)
import Data.Function (on)
import Data.IORef
import Data.Maybe (mapMaybe)
import Control.Arrow (second, (***))
import Control.Lens ((^.))
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO.Unsafe

import Ledger.Contexts
import Ledger.Index
import Ledger.Scripts
import Ledger.Slot
import Ledger.Value
import Ledger.Typed.Scripts.Validators
import Ledger.Constraints
import Ledger.Constraints.OffChain hiding (tx)
import Ledger.Tx
import Ledger.Crypto
import Ledger.Orphans                   ()
import Ledger.TimeSlot (posixTimeRangeToContainedSlotRange)

import Plutus.Trace.Emulator hiding (chainState, EmulatorConfig, throwError)
import Plutus.Contract.Trace
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential      (Credential (..))
import qualified Plutus.V1.Ledger.Api             as Api
import qualified Plutus.V1.Ledger.Ada             as Ada
import qualified PlutusTx.AssocMap                as Map
import qualified PlutusTx.Numeric as Num

import Wallet.Emulator.MultiAgent

-----

import Cooked.Tx.Constraints



type UtxoState = M.Map Pl.Address (Pl.Value, Maybe Pl.Datum)

-- TODO: write a function from MockChainSt to something closer to the
-- eutxo model we all have in our heads:
--
-- type UtxoState = Map Addr (Value, Maybe Datum)
--
-- utxoState :: MockChainSt -> UtxoState
--
-- This way we can write properties about it


-- |UNSAFE (yet probably the most convenient function)
--
-- Calls an 'EmulatorTrace' @t@ and uses the resulting 'UtxoIndex' as the initial
-- state for a 'MockChain'; We do use _unsafePerformIO_ in order to keep @t@'s return
-- value and feed it to the mock-chain we're running. This is useful to get system parameters
--
-- This function will print the resulting UtxoIndex; use the unprimed version if you
-- don't want all that noise.
runMockChainIOFrom' :: EmulatorTrace b -> (b -> MockChain a) -> IO (Either MockChainError a)
runMockChainIOFrom' tr f = do
  (ix, b) <- mcState0FromUnsafe' tr
  case runMockChain ix (f b) of
    Left err      -> return (Left err)
    Right (r, ix') -> printUtxoIndex ix' >> return (Right r)

runMockChainIOFrom :: EmulatorTrace b -> (b -> MockChain a) -> IO (Either MockChainError a)
runMockChainIOFrom tr f = do
  (ix, b) <- mcState0FromUnsafe' tr
  case runMockChain ix (f b) of
    Left err     -> return (Left err)
    Right (r, _) -> return (Right r)

-- |Returns the outputs for a given address that contain a certain value in their state;
-- we also already create the proper 'ChainIndexTxOut' for them.
scriptOutrefsFor :: (Monad m, Api.ToData a) => Address -> a -> MockChainT m [(TxOutRef, ChainIndexTxOut)]
scriptOutrefsFor addr a = do
  mRes <- mapM (secondM (flip (ciTxOutFromOut @Void) Nothing)) <$> outrefsFor addr
  return $ case mRes of
    Nothing  -> error "ciTxOutFromOut returned nothing but it shouldn't have"
    Just res -> mapMaybe (secondM $ ciHasDatumHashSet a) res

-- |Returns the current internal slot count.
slot :: (Monad m) => MockChainT m Slot
slot = gets (Slot. snd . time)

stopSlotCountWith :: (Monad m) => (Integer -> Integer) -> MockChainT m ()
stopSlotCountWith f = modify (\st -> st { time = (False, f (snd $ time st)) })

resumeSlotCount :: (Monad m) => MockChainT m ()
resumeSlotCount = modify (\st -> st { time = (True, snd $ time st) })

-- |Validates a transaction and, upon success, updates the utxo map; Since constructing
-- transactions can be painful, you probably want to use 'validateTxFromConstraints'
-- instead.
validateTx :: (Monad m) => Tx -> MockChainT m ()
validateTx tx = do
  s  <- slot
  ix <- gets utxo
  let res = runValidation (validateTransaction s tx) ix
  -- uncomment to see the ScriptValidationEvents; could be useful for debugging but it
  -- gets a bit noisy.
  --
  -- case trace (show $ snd res) $ fst res of
  case fst res of
    (Just err, _)  -> throwError (MCEValidationError err)
    (Nothing, ix') -> modify (\st -> st { utxo = ix' })

-- |Generates a transaction from constraints and signs it as if it were from the given Wallet.
validateTxFromConstraints :: forall a m
                           . ( Monad m, Api.FromData (DatumType a)
                             , Api.FromData (RedeemerType a), Api.ToData (DatumType a), Api.ToData (RedeemerType a))
                          => (Wallet, PrivateKey)
                          -> ScriptLookups a
                          -> TxConstraints (RedeemerType a) (DatumType a)
                          -> MockChainT m ()
validateTxFromConstraints (w, sk) lkups constr = do
  let etx = mkTx lkups constr
  case etx of
    Left err -> throwError (MCETxError err)
    Right tx -> balanceTxFrom w tx >>= validateTx . addSignature sk

validateTxFromConstraints' :: forall a m
                            . (Monad m, Api.FromData (DatumType a), Api.FromData (RedeemerType a), Api.ToData (DatumType a), Api.ToData (RedeemerType a))
                           => (Wallet, PrivateKey)
                           -> [(ScriptLookups a, TxConstraints (RedeemerType a) (DatumType a))]
                           -> MockChainT m ()
validateTxFromConstraints' wsk cstr =
  validateTxFromConstraints wsk (mconcat $ map fst cstr) (mconcat $ map snd cstr)

validateTxFromSkeleton :: (Monad m) => TxSkel -> MockChainT m ()
validateTxFromSkeleton (TxSkel constr ws) =
  validateTxFromConstraints @Void ws mempty (toLedgerConstraints constr)

-- instance Ord Value where
--   compare v1 v2 = compare (show v1) (show v2)

-- |Return the raw utxo's belonging to a given pubkey /in our current state/
utxosFromPK' :: (Monad m) => PubKeyHash -> MockChainT m [(TxOutRef, TxOut)]
utxosFromPK' pkH = do
  is <- gets (M.filter (belongsTo pkH . txOutAddress) . getIndex . utxo)
  return $ map (\(outref, out) -> (outref, out))
         $ M.toList is
 where
   belongsTo pkH0 = (== PubKeyCredential pkH0) . addressCredential

-- |Return the 'ChainIndexTxOut' associated with a given 'TxOutRef' such that
-- a given predicate holds
utxosFromPKst :: (Monad m)
              => (TxOut -> Bool)
              -> PubKeyHash
              -> MockChainT m [(TxOutRef, ChainIndexTxOut)]
utxosFromPKst p pk = mapMaybe (secondM go) <$> utxosFromPK' pk
  where go txOut = if p txOut then ciTxOutFromOut @Void txOut Nothing
                              else Nothing


outsFromRef :: (Monad m) => TxOutRef -> MockChainT m TxOut
outsFromRef outref = do
  mo <- gets (M.lookup outref . getIndex . utxo)
  case mo of
    Just o -> return o
    Nothing -> error ("No such output: " ++ show outref)

-- |Balances a transaction with money from a given wallet. For every transaction,
-- it must be the case that @inputs + mint == outputs + fee@.
--
-- TODO: this is currently only balancing Ada values; ideally, it should perform
-- balancing over all currencies
--
-- TODO: If the transaction has more input, we can just add an output to `w`
balanceTxFrom :: (Monad m) => Wallet -> UnbalancedTx -> MockChainT m Tx
balanceTxFrom w (UnbalancedTx tx0 _reqSigs _uindex slotRange) = do
  -- We start by gathering all the inputs and summing it
  let tx = tx0 { txFee = minFee tx0 }
  lhsInputs <- mapM (outsFromRef . txInRef) (S.toList (txInputs tx))
  let lhs = mappend (mconcat $ map txOutValue lhsInputs)      (txMint tx)
  let rhs = mappend (mconcat $ map txOutValue $ txOutputs tx) (txFee tx)
  (leftover, usedUTxO) <- foldM (\(leftOver, usedUTxO) (a,t,_) -> addInputToEquilibrate lhs rhs a t leftOver usedUTxO)
    (mempty, [])
    (toList rhs)
  let txIns' = map (`TxIn` Just ConsumePublicKeyAddress) usedUTxO
  let txOut' =
        TxOut
          (Address (PubKeyCredential $ pubKeyHash $ walletPubKey w) Nothing)
          leftover Nothing
  return tx{ txInputs  = txInputs tx <> S.fromList txIns'
           , txOutputs = txOutputs tx ++ [txOut']
           , txValidRange = posixTimeRangeToContainedSlotRange def slotRange
           }
  where
    addInputToEquilibrate lhs rhs asset token leftOver usedUTxO =
      case compare (valueOf lhs asset token) (valueOf rhs asset token) of
      EQ -> return (leftOver, usedUTxO)
      -- If the input of the transaction is already too big,
      -- then no matter which inputs we add, the transaction will fail.
      -- We still send it, in order to get the error of the chain.
      -- However, we know it will be a "MCEValidation(...,ValueNotPreserved(...))"
      GT -> return (leftOver, usedUTxO)
      LT ->
        let delta = valueOf rhs asset token - valueOf lhs asset token in
        case compare delta (valueOf leftOver asset token) of
        LT -> return (leftOver Num.- singleton asset token delta, usedUTxO)
        EQ -> return (leftOver Num.- singleton asset token delta, usedUTxO)
        GT -> do
          refsFromW <- utxosFromPK' (pubKeyHash $ walletPubKey w)
          (neededRefsFromW, additionalLeftover) <- necessaryUtxosFor asset token (delta - valueOf leftOver asset token) refsFromW usedUTxO
          return (leftOver <> additionalLeftover, usedUTxO ++ neededRefsFromW)

    -- Given an integer n and a list of UTxOs return the TxOutRes we need to consume
    -- in order to cover n and returns any potential leftover that need to be
    -- transfered to n's owner again.
    --
    -- We perform no sorting nor optimization, so we'll just select the first k necessary outrefs:
    --
    -- > necessaryUtxosFor 100 [(o1, 50, dh1), (o2, 30, dh2), (o3, 30, dh3), (o4, 120, dh4)]
    -- >    == ([o1, o2, o3], 10)
    --
    necessaryUtxosFor :: (Monad m)
                      => CurrencySymbol -> TokenName
                      -> Integer -> [(TxOutRef, TxOut)]
                      -> [TxOutRef]
                      -> MockChainT m ([TxOutRef], Value)
    necessaryUtxosFor _     _     0 _  _ = return ([], mempty)
    necessaryUtxosFor asset token n ((oref, o):os) usedUTxO
      | oref `elem` usedUTxO = necessaryUtxosFor asset token n os usedUTxO
      | valOf (txOutValue o) > n = return ([oref], txOutValue o Num.- singleton asset token n)
      | valOf (txOutValue o) == 0 = necessaryUtxosFor asset token n os usedUTxO
      | otherwise =
          let valLeftover = txOutValue o Num.- singleton asset token (valOf (txOutValue o)) in
          ((oref :) *** (valLeftover Num.+ )) <$> necessaryUtxosFor asset token (n - valOf (txOutValue o)) os usedUTxO

      where valOf val = valueOf val asset token
    -- If there is not enough funds, the transaction will fail.
    -- Even if we are already aware of it at this point,
    -- we do not want to interfere with the error generated by the contract,
    -- hence we let the wrong transaction happen.
    necessaryUtxosFor _     _     _ [] _ = return ([], mempty)

    toList :: Value -> [(CurrencySymbol, TokenName, Integer)]
    toList v = concatMap (distribute . second Map.toList) (Map.toList (getValue v))

    distribute :: (a,[(b,c)]) -> [(a,b,c)]
    distribute (a,l) = map (\(b,c) -> (a,b,c)) l

-- |Builds a 'ChainIndexTxOut' from a 'TxOut' and some potential datum that must match
-- the datum hash in the 'TxOut' one wants to consume; If you don't pass a @Just x@
-- as a second argument, make sure the 'ScriptLookups' contain the datum we need otherwise
-- tx validation will fail.
ciTxOutFromOut :: (Api.ToData a) => TxOut -> Maybe a -> Maybe ChainIndexTxOut
ciTxOutFromOut out md =
  case addressCredential $ txOutAddress out of
    PubKeyCredential _ -> return $ PublicKeyChainIndexTxOut (txOutAddress out) (txOutValue out)
    ScriptCredential (ValidatorHash vh) -> do
      dh <- txOutDatumHash out
      let v = Left (ValidatorHash vh)
      let d = maybe (Left dh) (Right . Datum . Api.toBuiltinData) md
      return $ ScriptChainIndexTxOut (txOutAddress out) v d (txOutValue out)

ciHasDatumHashSet :: (Api.ToData a) => a -> ChainIndexTxOut -> Maybe ChainIndexTxOut
ciHasDatumHashSet a (ScriptChainIndexTxOut addr v (Left dh) val)
  = let datum = Datum $ Api.toBuiltinData a
     in guard (dh == datumHash datum) >> return (ScriptChainIndexTxOut addr v (Right datum) val)
ciHasDatumHashSet _ _ = Nothing

-- * MockChain Wallets
--
-- We keep the private key associated with each wallet so we can sign transactions
-- from any wallet easily

type MCWallet = (Wallet, PrivateKey)

mcWallet :: Int -> MCWallet
mcWallet j
  | j > 0 && j <= 10 = let i = j - 1 in (knownWallets !! i , knownPrivateKeys !! i)
  | otherwise        = error "There are only 10 wallets, starting index is 1"

mcWalletPK :: MCWallet -> PubKey
mcWalletPK = walletPubKey . fst

mcWalletPKHash :: MCWallet -> PubKeyHash
mcWalletPKHash = pubKeyHash . mcWalletPK

-- * UtxoIndex
--
-- A 'UtxoIndex' is a map from 'TxOutRef' to 'TxOut' and
-- represents the current state of the system.

mcStateFromEmulatorState :: EmulatorState -> MockChainSt
mcStateFromEmulatorState st = MockChainSt
  { utxo = st ^. (chainState . index)
  , time = (True, fromIntegral $ st ^. (chainState . currentSlot))
  }

-- Default initial state
mcState0 :: MockChainSt
mcState0 = mcState0From' $ return ()

-- State that results from executing a trace
mcState0From' :: EmulatorTrace () -> MockChainSt
mcState0From' = mcState0From defaultDist

-- State that results from executing a trace with a given initial distribution
-- of funds
mcState0From :: InitialDistribution -> EmulatorTrace () -> MockChainSt
mcState0From distr tr = mcStateFromEmulatorState st
  where
    (_, _, st) = runEmulatorTrace (def { _initialChainState = Left distr }) tr

-- Uses some unsafePerformIO magic to keep the value returned by the emulator trace
mcState0FromUnsafe' :: EmulatorTrace a -> IO (MockChainSt, a)
mcState0FromUnsafe' = mcState0FromUnsafe defaultDist

mcState0FromUnsafe :: InitialDistribution -> EmulatorTrace a -> IO (MockChainSt, a)
mcState0FromUnsafe distr tr = do
  r <- newIORef Nothing
  let !(_, _, st) = runEmulatorTrace (def { _initialChainState = Left distr })
                  $ do !res <- tr
                       let !() = unsafePerformIO (writeIORef r $ Just res)
                       return ()
  (Just x) <- readIORef r
  return (mcStateFromEmulatorState st, x)

-- ** Pretty Printing UtxoIndex

printUtxoIndex :: UtxoIndex -> IO ()
printUtxoIndex (UtxoIndex ix) = do
  let perCredential = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $ map (uncurry go) $ M.toList ix
  forM_ perCredential $ \ ls -> do
    case ls of
      [] -> return ()
      ((cred, _):_) -> do
        putStrLn (showCred cred)
        forM_ ls $ \(_, (outId, valdatums)) -> do
          putStrLn ("  - " ++ outId)
          mapM_ (putStrLn . ("      " ++)) valdatums
  where
    showHash n h = take n h ++ "#" ++ drop (length h - n) h

    go :: TxOutRef -> TxOut -> (Credential,(String,[String]))
    go (TxOutRef oid oix) (TxOut (Address addrCredential _staking) val datum)
      = ( addrCredential
        , ( showHash 4 (show oid) ++ "[" ++ show oix ++ "]"
          , showDatum datum : showVal val
          ))

    showCred :: Credential -> String
    showCred (PubKeyCredential pkh) = "pubkey: " ++ showHash 4 (show pkh)
    showCred (ScriptCredential sch) = "script: " ++ showHash 4 (show sch)

    showDatum :: Maybe DatumHash -> String
    showDatum Nothing  = "no-datum"
    showDatum (Just h) = "datum: " ++ showHash 4 (show h)

    showVal :: Value -> [String]
    showVal = map (\(sym, tok, n) -> showMaybeAda sym tok n) . flattenValue

    showMaybeAda s tok n = if s == Ada.adaSymbol
                           then "Ada: " ++ show n
                           else showHash 3 (show s)  ++ ": " ++ show tok ++ " -> " ++ show n

-- Utils:

mustSpendPubKeyOutputs :: (Api.FromData (DatumType a), Api.FromData (RedeemerType a), Api.ToData (DatumType a), Api.ToData (RedeemerType a))
                       => [(TxOutRef, ChainIndexTxOut)] -> (ScriptLookups a, TxConstraints (RedeemerType a) (DatumType a))
mustSpendPubKeyOutputs ws = (lkups, tx)
  where
    lkups = unspentOutputs (M.fromList ws)
    tx    = mconcat $ map (mustSpendPubKeyOutput . fst) ws

hasAssetClass :: AssetClass -> TxOut -> Bool
hasAssetClass (AssetClass (c, s)) o = valueOf (txOutValue o) c s > 0


-- * MockChain Example
--
-- Start from the initial 'UtxoIndex' and transfer 4200 lovelace from wallet 1 to wallet 2

example :: IO (Either MockChainError ())
example = runMockChainIO mcState0 $ do
  validateTxFromConstraints @Void (mcWallet 1) mempty (mustPayToPubKey (mcWalletPKHash $ mcWallet 2) (Ada.lovelaceValueOf 4200))


-}
