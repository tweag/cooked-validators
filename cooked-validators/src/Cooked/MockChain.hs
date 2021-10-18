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
import Control.Arrow (second)
import Control.Lens ((^.))
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import System.IO.Unsafe

import qualified Ledger.Contexts as Pl
import qualified Ledger.Index as Pl
import qualified Ledger.Scripts as Pl
import qualified Ledger.Slot as Pl
import qualified Ledger.Value as Pl
import qualified Ledger.Typed.Scripts.Validators as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Tx as Pl
import qualified Ledger.Crypto as Pl

import qualified Plutus.Trace.Emulator as Pl
import qualified Plutus.Contract.Trace as Pl
import qualified Plutus.V1.Ledger.Address as Pl
import qualified Plutus.V1.Ledger.Credential as Pl
import qualified Plutus.V1.Ledger.Api as Pl



type UtxoState = M.Map Pl.Address (Pl.Value, Maybe Pl.Datum)

-- TODO: write a function from MockChainSt to something closer to the
-- eutxo model we all have in our heads:
--
-- type UtxoState = Map Addr (Value, Maybe Datum)
--
-- utxoState :: MockChainSt -> UtxoState
--
-- This way we can write properties about it

-- custom monad instance made to increase the slot count automatically
instance (Monad m) => Monad (MockChainT m) where
  return  = pure
  MockChainT x >>= f =
    MockChainT $ do
      xres <- x
      modify (\st -> st { time = incSlot (time st) })
      unMockChain (f xres)
   where
     incSlot (True , n) = (True, n+1)
     incSlot (False, n) = (False, n)

instance (Monad m) => MonadFail (MockChainT m) where
  fail = throwError . FailWith

type MockChain = MockChainT Identity

runMockChainT :: (Monad m) => MockChainSt -> MockChainT m a -> m (Either MockChainError (a, Pl.UtxoIndex))
runMockChainT i0 = runExceptT . fmap (second utxo) . flip runStateT i0 . unMockChain

runMockChain :: MockChainSt -> MockChain a -> Either MockChainError (a, Pl.UtxoIndex)
runMockChain i0 = runIdentity . runMockChainT i0

runMockChainIO :: MockChainSt -> MockChain a -> IO (Either MockChainError a)
runMockChainIO i0 f = case runMockChain i0 f of
                        Left err      -> return (Left err)
                        Right (r, ix) -> printUtxoIndex ix >> return (Right r)

-- |UNSAFE (yet probably the most convenient function)
--
-- Calls an 'EmulatorTrace' @t@ and uses the resulting 'UtxoIndex' as the initial
-- state for a 'MockChain'; We do use _unsafePerformIO_ in order to keep @t@'s return
-- value and feed it to the mock-chain we're running. This is useful to get system parameters
--
-- This function will print the resulting UtxoIndex; use the unprimed version if you
-- don't want all that noise.
runMockChainIOFrom' :: Pl.EmulatorTrace b -> (b -> MockChain a) -> IO (Either MockChainError a)
runMockChainIOFrom' tr f = do
  (ix, b) <- mcState0FromUnsafe' tr
  case runMockChain ix (f b) of
    Left err      -> return (Left err)
    Right (r, ix') -> printUtxoIndex ix' >> return (Right r)

runMockChainIOFrom :: Pl.EmulatorTrace b -> (b -> MockChain a) -> IO (Either MockChainError a)
runMockChainIOFrom tr f = do
  (ix, b) <- mcState0FromUnsafe' tr
  case runMockChain ix (f b) of
    Left err     -> return (Left err)
    Right (r, _) -> return (Right r)


-- |Returns all the outputs that belong to a given address
outrefsFor :: (Monad m) => Pl.Address -> MockChainT m [(Pl.TxOutRef, Pl.TxOut)]
outrefsFor addr = do
  ix <- gets (Pl.getIndex . utxo)
  let ix' = M.filter ((== addr) . Pl.txOutAddress) ix
  return $ M.toList ix'

-- |Returns the outputs for a given address that contain a certain value in their state;
-- we also already create the proper 'ChainIndexTxOut' for them.
scriptOutrefsFor :: (Monad m, Pl.ToData a) => Pl.Address -> a -> MockChainT m [(Pl.TxOutRef, Pl.ChainIndexTxOut)]
scriptOutrefsFor addr a = do
  mRes <- mapM (secondM (flip (ciTxOutFromOut @Void) Nothing)) <$> outrefsFor addr
  return $ case mRes of
    Nothing  -> error "ciTxOutFromOut returned nothing but it shouldn't have"
    Just res -> mapMaybe (secondM $ ciHasDatumHashSet a) res

-- |Returns the current internal slot count.
slot :: (Monad m) => MockChainT m Pl.Slot
slot = gets (Pl.Slot. snd . time)

stopSlotCountWith :: (Monad m) => (Integer -> Integer) -> MockChainT m ()
stopSlotCountWith f = modify (\st -> st { time = (False, f (snd $ time st)) })

resumeSlotCount :: (Monad m) => MockChainT m ()
resumeSlotCount = modify (\st -> st { time = (True, snd $ time st) })

-- |Validates a transaction and, upon success, updates the utxo map; Since constructing
-- transactions can be painful, you probably want to use 'validateTxFromConstraints'
-- instead.
validateTx :: (Monad m) => Pl.Tx -> MockChainT m ()
validateTx tx = do
  s  <- slot
  ix <- gets utxo
  let res = Pl.runValidation (Pl.validateTransaction s tx) ix
  -- uncomment to see the ScriptValidationEvents; could be useful for debugging but it
  -- gets a bit noisy.
  --
  -- case trace (show $ snd res) $ fst res of
  case fst res of
    (Just err, _)  -> throwError (MCEValidationError err)
    (Nothing, ix') -> modify (\st -> st { utxo = ix' })

-- instance Ord Value where
--   compare v1 v2 = compare (show v1) (show v2)

-- |Return the raw utxo's belonging to a given pubkey /in our current state/
utxosFromPK' :: (Monad m) => Pl.PubKeyHash -> MockChainT m [(Pl.TxOutRef, Pl.TxOut)]
utxosFromPK' pkH = do
  is <- gets (M.filter (belongsTo pkH . Pl.txOutAddress) . Pl.getIndex . utxo)
  return $ map (\(outref, out) -> (outref, out))
         $ M.toList is
 where
   belongsTo pkH0 = (== Pl.PubKeyCredential pkH0) . Pl.addressCredential

-- |Return the 'ChainIndexTxOut' associated with a given 'TxOutRef' such that
-- a given predicate holds
utxosFromPKst :: (Monad m)
              => (Pl.TxOut -> Bool)
              -> Pl.PubKeyHash
              -> MockChainT m [(Pl.TxOutRef, Pl.ChainIndexTxOut)]
utxosFromPKst p pk = mapMaybe (secondM go) <$> utxosFromPK' pk
  where go txOut = if p txOut then ciTxOutFromOut @Void txOut Nothing
                              else Nothing


outsFromRef :: (Monad m) => Pl.TxOutRef -> MockChainT m Pl.TxOut
outsFromRef outref = do
  mo <- gets (M.lookup outref . Pl.getIndex . utxo)
  case mo of
    Just o -> return o
    Nothing -> error ("No such output: " ++ show outref)

-- |Builds a 'ChainIndexTxOut' from a 'TxOut' and some potential datum that must match
-- the datum hash in the 'TxOut' one wants to consume; If you don't pass a @Just x@
-- as a second argument, make sure the 'ScriptLookups' contain the datum we need otherwise
-- tx validation will fail.
ciTxOutFromOut :: (Pl.ToData a) => Pl.TxOut -> Maybe a -> Maybe Pl.ChainIndexTxOut
ciTxOutFromOut out md =
  case Pl.addressCredential $ Pl.txOutAddress out of
    Pl.PubKeyCredential _ -> return $ Pl.PublicKeyChainIndexTxOut (Pl.txOutAddress out) (Pl.txOutValue out)
    Pl.ScriptCredential (Pl.ValidatorHash vh) -> do
      dh <- Pl.txOutDatumHash out
      let v = Left (Pl.ValidatorHash vh)
      let d = maybe (Left dh) (Right . Pl.Datum . Pl.toBuiltinData) md
      return $ Pl.ScriptChainIndexTxOut (Pl.txOutAddress out) v d (Pl.txOutValue out)

ciHasDatumHashSet :: (Pl.ToData a) => a -> Pl.ChainIndexTxOut -> Maybe Pl.ChainIndexTxOut
ciHasDatumHashSet a (Pl.ScriptChainIndexTxOut addr v (Left dh) val)
  = let datum = Pl.Datum $ Pl.toBuiltinData a
     in guard (dh == Pl.datumHash datum) >> return (Pl.ScriptChainIndexTxOut addr v (Right datum) val)
ciHasDatumHashSet _ _ = Nothing

-- * MockChain Wallets
--
-- We keep the private key associated with each wallet so we can sign transactions
-- from any wallet easily

type MCWallet = (Pl.Wallet, Pl.PrivateKey)

mcWallet :: Int -> MCWallet
mcWallet j
  | j > 0 && j <= 10 = let i = j - 1 in (Pl.knownWallets !! i , Pl.knownPrivateKeys !! i)
  | otherwise        = error "There are only 10 wallets, starting index is 1"

mcWalletPK :: MCWallet -> Pl.PubKey
mcWalletPK = Pl.walletPubKey . fst

mcWalletPKHash :: MCWallet -> Pl.PubKeyHash
mcWalletPKHash = Pl.pubKeyHash . mcWalletPK

-- * UtxoIndex
--
-- A 'UtxoIndex' is a map from 'TxOutRef' to 'TxOut' and
-- represents the current state of the system.

mcStateFromEmulatorState :: EmulatorState -> MockChainSt
mcStateFromEmulatorState st = MockChainSt
  { utxo = st ^. (chainState . Pl.index)
  , time = (True, fromIntegral $ st ^. (chainState . Pl.currentSlot))
  }

-- Default initial state
mcState0 :: MockChainSt
mcState0 = mcState0From' $ return ()

-- State that results from executing a trace
mcState0From' :: Pl.EmulatorTrace () -> MockChainSt
mcState0From' = mcState0From Pl.defaultDist

-- State that results from executing a trace with a given initial distribution
-- of funds
mcState0From :: Pl.InitialDistribution -> Pl.EmulatorTrace () -> MockChainSt
mcState0From distr tr = mcStateFromEmulatorState st
  where
    (_, _, st) = Pl.runEmulatorTrace (def { Pl._initialChainState = Left distr }) tr

-- Uses some unsafePerformIO magic to keep the value returned by the emulator trace
mcState0FromUnsafe' :: Pl.EmulatorTrace a -> IO (MockChainSt, a)
mcState0FromUnsafe' = mcState0FromUnsafe Pl.defaultDist

mcState0FromUnsafe :: Pl.InitialDistribution -> Pl.EmulatorTrace a -> IO (MockChainSt, a)
mcState0FromUnsafe distr tr = do
  r <- newIORef Nothing
  let !(_, _, st) = Pl.runEmulatorTrace (def { Pl._initialChainState = Left distr })
                  $ do !res <- tr
                       let !() = unsafePerformIO (writeIORef r $ Just res)
                       return ()
  (Just x) <- readIORef r
  return (mcStateFromEmulatorState st, x)

-- ** Pretty Printing UtxoIndex

printUtxoIndex :: Pl.UtxoIndex -> IO ()
printUtxoIndex (Pl.UtxoIndex ix) = do
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

    go :: Pl.TxOutRef -> Pl.TxOut -> (Pl.Credential,(String,[String]))
    go (Pl.TxOutRef oid oix) (Pl.TxOut (Pl.Address addrCredential _staking) val datum)
      = ( addrCredential
        , ( showHash 4 (show oid) ++ "[" ++ show oix ++ "]"
          , showDatum datum : showVal val
          ))

    showCred :: Pl.Credential -> String
    showCred (Pl.PubKeyCredential pkh) = "pubkey: " ++ showHash 4 (show pkh)
    showCred (Pl.ScriptCredential sch) = "script: " ++ showHash 4 (show sch)

    showDatum :: Maybe Pl.DatumHash -> String
    showDatum Nothing  = "no-datum"
    showDatum (Just h) = "datum: " ++ showHash 4 (show h)

    showVal :: Pl.Value -> [String]
    showVal = map (\(sym, tok, n) -> showMaybeAda sym tok n) . Pl.flattenValue

    showMaybeAda s tok n = if s == Pl.adaSymbol
                           then "Ada: " ++ show n
                           else showHash 3 (show s)  ++ ": " ++ show tok ++ " -> " ++ show n

-- Utils:

mustSpendPubKeyOutputs :: (Pl.FromData (Pl.DatumType a), Pl.FromData (Pl.RedeemerType a), Pl.ToData (Pl.DatumType a), Pl.ToData (Pl.RedeemerType a))
                       => [(Pl.TxOutRef, Pl.ChainIndexTxOut)] -> (Pl.ScriptLookups a, Pl.TxConstraints (Pl.RedeemerType a) (Pl.DatumType a))
mustSpendPubKeyOutputs ws = (lkups, tx)
  where
    lkups = Pl.unspentOutputs (M.fromList ws)
    tx    = mconcat $ map (Pl.mustSpendPubKeyOutput . fst) ws

hasAssetClass :: Pl.AssetClass -> Pl.TxOut -> Bool
hasAssetClass (Pl.AssetClass (c, s)) o = Pl.valueOf (Pl.txOutValue o) c s > 0


-- * MockChain Example
--
-- Start from the initial 'UtxoIndex' and transfer 4200 lovelace from wallet 1 to wallet 2

example :: IO (Either MockChainError ())
example = runMockChainIO mcState0 $ do
  validateTxFromConstraints @Void (mcWallet 1) mempty (mustPayToPubKey (mcWalletPKHash $ mcWallet 2) (Ada.lovelaceValueOf 4200))


-}
