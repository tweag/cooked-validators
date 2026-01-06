{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides a staged implementation of our `MonadBlockChain`. The
-- motivation behind this is to be able to modify traces using `Cooked.Ltl` and
-- `Cooked.Tweak` while they are interpreted.
module Cooked.MockChain.Staged
  ( interpretAndRunWith,
    interpretAndRun,
    StagedMockChain,
    MockChainBuiltin,
    runTweakFrom,
    MonadModalBlockChain,
    InterpMockChain,
    somewhere,
    somewhere',
    runTweak,
    everywhere,
    everywhere',
    withTweak,
    there,
    there',
    nowhere',
    nowhere,
    whenAble',
    whenAble,
    Staged (..),
    singletonBuiltin,
    interpStaged,
    interpStagedMockChain,
    MonadLtl (..),
    MockChainTweak,
  )
where

import Cardano.Node.Emulator qualified as Emulator
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Cooked.InitialDistribution
import Cooked.Ltl
import Cooked.Ltl.Combinators
import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
import Cooked.Pretty.Hashable
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Default
import Data.Functor
import Data.Kind
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- * Freer monad to represent an AST on a set of operations

-- | The freer monad on @op@. We think of this as the AST of a computation with
-- operations of types @op a@. These operation will in turn be instantiated with
-- mockchain builtins alongside the appropriate effects.
data Staged (op :: Type -> Type) :: Type -> Type where
  Return :: a -> Staged op a
  Instr :: op a -> (a -> Staged op b) -> Staged op b

instance Functor (Staged op) where
  fmap f (Return x) = Return $ f x
  fmap f (Instr op cont) = Instr op (fmap f . cont)

instance Applicative (Staged op) where
  pure = Return
  (<*>) = ap

instance Monad (Staged op) where
  (Return x) >>= f = f x
  (Instr i m) >>= f = Instr i (m >=> f)

-- | Building an singleton instruction in a staged monad
singletonBuiltin :: builtin a -> Staged builtin a
singletonBuiltin = (`Instr` Return)

-- | Interprets a staged computation given a interpreter of the builtins
interpStaged :: forall op m. (Monad m) => (forall a. op a -> m a) -> forall a. Staged op a -> m a
interpStaged _ (Return a) = return a
interpStaged interpBuiltin (Instr op cont) = interpBuiltin op >>= interpStaged interpBuiltin . cont

-- | A 'StagedMockChain' is an AST of mockchain builtins. The idea is to keep
-- the builtins abstract and postpone interpretation, to open up the possibility
-- of applying tweaks before submitting transaction.
type StagedMockChain = Staged MockChainBuiltin

instance Alternative StagedMockChain where
  empty = singletonBuiltin Empty
  a <|> b = singletonBuiltin $ Alt a b

instance MonadFail StagedMockChain where
  fail = singletonBuiltin . Fail

instance MonadError MockChainError StagedMockChain where
  throwError = singletonBuiltin . ThrowError
  catchError act = singletonBuiltin . CatchError act

instance MonadLtl MockChainTweak StagedMockChain where
  modifyLtl formula = singletonBuiltin . ModifyLtl formula

instance MonadBlockChainBalancing StagedMockChain where
  getParams = singletonBuiltin GetParams
  txSkelOutByRef = singletonBuiltin . TxSkelOutByRef
  utxosAt = singletonBuiltin . UtxosAt
  logEvent = singletonBuiltin . LogEvent

instance MonadBlockChainWithoutValidation StagedMockChain where
  allUtxos = singletonBuiltin AllUtxos
  setParams = singletonBuiltin . SetParams
  waitNSlots = singletonBuiltin . WaitNSlots
  define name = singletonBuiltin . Define name
  setConstitutionScript = singletonBuiltin . SetConstitutionScript
  getConstitutionScript = singletonBuiltin GetConstitutionScript
  getCurrentReward = singletonBuiltin . GetCurrentReward

instance MonadBlockChain StagedMockChain where
  validateTxSkel = singletonBuiltin . ValidateTxSkel
  forceOutputs = singletonBuiltin . ForceOutputs

-- * Interpreting and running 'StagedMockChain'

-- | Interprets the staged mockchain then runs the resulting computation with a
-- custom function. This can be used, for example, to supply a custom
-- 'InitialDistribution' by providing 'runMockChainTFromInitDist'.
interpretAndRunWith :: (forall m. (Monad m) => MockChainT m a -> m res) -> StagedMockChain a -> [res]
interpretAndRunWith f = f . interpret

-- | Same as 'interpretAndRunWith' but using 'runMockChainT' as the default way
-- to run the computation.
interpretAndRun :: StagedMockChain a -> [MockChainReturn a]
interpretAndRun = interpretAndRunWith runMockChainT

-- | The semantic domain in which 'StagedMockChain' gets interpreted
type InterpMockChain = MockChainT []

-- | Tweaks operating within the 'InterpMockChain' domain
type MockChainTweak = UntypedTweak InterpMockChain

-- | The 'interpret' function gives semantics to our traces. One
-- 'StagedMockChain' computation yields a potential list of 'MockChainT'
-- computations.
interpret :: StagedMockChain a -> InterpMockChain a
interpret = flip evalStateT [] . interpStagedMockChain

-- * 'StagedMockChain': An AST for 'MonadMockChain' computations

-- | Abstract representation of all the builtin functions of a 'MonadBlockChain'
data MockChainBuiltin a where
  -- methods of 'MonadBlockChain'
  GetParams :: MockChainBuiltin Emulator.Params
  SetParams :: Emulator.Params -> MockChainBuiltin ()
  ValidateTxSkel :: TxSkel -> MockChainBuiltin Ledger.CardanoTx
  TxSkelOutByRef :: Api.TxOutRef -> MockChainBuiltin TxSkelOut
  WaitNSlots :: (Integral i) => i -> MockChainBuiltin Ledger.Slot
  AllUtxos :: MockChainBuiltin [(Api.TxOutRef, TxSkelOut)]
  UtxosAt :: (Script.ToAddress a) => a -> MockChainBuiltin [(Api.TxOutRef, TxSkelOut)]
  LogEvent :: MockChainLogEntry -> MockChainBuiltin ()
  Define :: (ToHash a) => String -> a -> MockChainBuiltin a
  SetConstitutionScript :: (ToVScript s) => s -> MockChainBuiltin ()
  GetConstitutionScript :: MockChainBuiltin (Maybe VScript)
  GetCurrentReward :: (Script.ToCredential c) => c -> MockChainBuiltin (Maybe Api.Lovelace)
  ForceOutputs :: [TxSkelOut] -> MockChainBuiltin [Api.TxOutRef]
  -- TODO the following are effects outside of the mockchain builtins per se. It
  -- would likely be more precise to use a dedicated library to handle those.
  --
  -- The empty set of traces
  Empty :: MockChainBuiltin a
  -- The union of two sets of traces
  Alt :: StagedMockChain a -> StagedMockChain a -> MockChainBuiltin a
  -- for the 'MonadFail' instance
  Fail :: String -> MockChainBuiltin a
  -- for the 'MonadError MockChainError' instance
  ThrowError :: MockChainError -> MockChainBuiltin a
  CatchError :: StagedMockChain a -> (MockChainError -> StagedMockChain a) -> MockChainBuiltin a
  -- for the Ltl modifications
  ModifyLtl :: Ltl MockChainTweak -> StagedMockChain a -> MockChainBuiltin a

-- * Interpreting the AST

-- | To be a suitable semantic domain for computations modified by LTL formulas,
-- a monad @m@ has to
--
-- * have the right @builtin@ functions, which can be modified by the right
--   @modification@s,
--
-- * be a 'MonadPlus', because one LTL formula might yield different modified
--   versions of the computation, and
--
-- This type class only requires from the user to specify how to interpret the
-- (modified) builtins. In order to do so, it passes around the formulas that
-- are to be applied to the next time step in a @StateT@

-- * 'InterpLtl' instance

-- | Interpret a 'Staged' computation into a suitable domain
interpStagedMockChain :: StagedMockChain a -> StateT [Ltl MockChainTweak] InterpMockChain a
interpStagedMockChain = interpStaged $ \case
  GetParams -> getParams
  (SetParams params) -> setParams params
  (ValidateTxSkel skel) -> do
    modifications <- gets nowLaterList
    msum . (modifications <&>) $
      \(now, later) -> do
        (_, skel') <-
          lift . (`runTweakInChain` skel) $
            foldr
              ( \(UntypedTweak tweak, mode) acc ->
                  if mode
                    then tweak >> acc
                    else ensureFailingTweak tweak >> acc
              )
              doNothingTweak
              now
        put later
        validateTxSkel skel'
  (TxSkelOutByRef o) -> txSkelOutByRef o
  (WaitNSlots s) -> waitNSlots s
  AllUtxos -> allUtxos
  (UtxosAt address) -> utxosAt address
  Empty -> mzero
  (Alt l r) -> interpStagedMockChain l `mplus` interpStagedMockChain r
  (Fail msg) -> fail msg
  (ThrowError err) -> throwError err
  (CatchError act handler) -> catchError (interpStagedMockChain act) (interpStagedMockChain . handler)
  (LogEvent entry) -> logEvent entry
  (Define name hash) -> define name hash
  (SetConstitutionScript script) -> setConstitutionScript script
  GetConstitutionScript -> getConstitutionScript
  (GetCurrentReward cred) -> getCurrentReward cred
  (ForceOutputs outs) -> forceOutputs outs
  (ModifyLtl formula comp) -> do
    modify' (formula :)
    res <- interpStagedMockChain comp
    formulas <- get
    unless (null formulas) $ do
      guard $ finished $ head formulas
      put $ tail formulas
    return res

-- ** Helpers to run tweaks for use in tests for tweaks

-- | Runs a 'Tweak' from a given 'TxSkel' within a mockchain
runTweak :: Tweak InterpMockChain a -> TxSkel -> [MockChainReturn (a, TxSkel)]
runTweak = runTweakFrom def

-- | Runs a 'Tweak' from a given 'TxSkel' and 'InitialDistribution' within a
-- mockchain
runTweakFrom :: InitialDistribution -> Tweak InterpMockChain a -> TxSkel -> [MockChainReturn (a, TxSkel)]
runTweakFrom initDist tweak = runMockChainTFromInitDist initDist . runTweakInChain tweak

-- ** Modalities

-- | A modal mockchain is a mockchain that allows us to use LTL modifications
-- with 'Tweak's
type MonadModalBlockChain m = (MonadBlockChain m, MonadLtl MockChainTweak m)

fromTweak :: Tweak m a -> Ltl (UntypedTweak m)
fromTweak = LtlAtom . UntypedTweak

-- | Apply a 'Tweak' to some transaction in the given Trace. The tweak must
-- apply at least once.
somewhere :: (MonadModalBlockChain m) => Tweak InterpMockChain b -> m a -> m a
somewhere = somewhere' . fromTweak

-- | Apply an Ltl modification somewhere in the given Trace. The modification
-- must apply at least once.
somewhere' :: (MonadLtl mod m) => Ltl mod -> m a -> m a
somewhere' = modifyLtl . eventually'

-- | Apply a 'Tweak' to every transaction in a given trace. This is also
-- successful if there are no transactions at all.
everywhere :: (MonadModalBlockChain m) => Tweak InterpMockChain b -> m a -> m a
everywhere = everywhere' . fromTweak

-- | Apply an Ltl modification everywhere it can be (including nowhere if it
-- does not apply). If the modification branches, this will branch at every
-- location the modification can be applied.
everywhere' :: (MonadLtl mod m) => Ltl mod -> m a -> m a
everywhere' = modifyLtl . always'

-- | Ensures a given 'Tweak' can never successfully be applied in a computation
nowhere :: (MonadModalBlockChain m) => Tweak InterpMockChain b -> m a -> m a
nowhere = nowhere' . fromTweak

-- | Ensures a given Ltl modification can never be applied on a computation
nowhere' :: (MonadLtl mod m) => Ltl mod -> m a -> m a
nowhere' = modifyLtl . never'

-- | Apply a given 'Tweak' at every location in a computation where it does not
-- fail, which might never occur.
whenAble :: (MonadModalBlockChain m) => Tweak InterpMockChain b -> m a -> m a
whenAble = whenAble' . fromTweak

-- | Apply an Ltl modification at every location in a computation where it is
-- possible. Does not fail if no such position exists.
whenAble' :: (MonadLtl mod m) => Ltl mod -> m a -> m a
whenAble' = modifyLtl . whenPossible'

-- | Apply a 'Tweak' to the (0-indexed) nth transaction in a given
-- trace. Successful when this transaction exists and can be modified.
there :: (MonadModalBlockChain m) => Integer -> Tweak InterpMockChain b -> m a -> m a
there n = there' n . fromTweak

-- | Apply an Ltl modification to the (0-indexed) nth transaction in a
-- given trace. Successful when this transaction exists and can be modified.
--
-- See also `Cooked.Tweak.Labels.labelled'` to select transactions based on
-- labels instead of their order.
there' :: (MonadLtl mod m) => Integer -> Ltl mod -> m a -> m a
there' n = modifyLtl . delay' n

-- | Apply a 'Tweak' to the next transaction in the given trace. The order of
-- arguments is reversed compared to 'somewhere' and 'everywhere', because that
-- enables an idiom like
--
-- > do ...
-- >    endpoint arguments `withTweak` someModification
-- >    ...
--
-- where @endpoint@ builds and validates a single transaction depending on the
-- given @arguments@. Then `withTweak` says "I want to modify the transaction
-- returned by this endpoint in the following way".
withTweak :: (MonadModalBlockChain m) => m x -> Tweak InterpMockChain a -> m x
withTweak = flip (there 0)
