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
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api

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

-- | The 'interpret' function gives semantics to our traces. One
-- 'StagedMockChain' computation yields a potential list of 'MockChainT'
-- computations.
interpret :: StagedMockChain a -> InterpMockChain a
interpret = flip evalStateT [] . interpLtl

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
  -- The empty set of traces
  Empty :: MockChainBuiltin a
  -- The union of two sets of traces
  Alt :: StagedMockChain a -> StagedMockChain a -> MockChainBuiltin a
  -- for the 'MonadFail' instance
  Fail :: String -> MockChainBuiltin a
  -- for the 'MonadError MockChainError' instance
  ThrowError :: MockChainError -> MockChainBuiltin a
  CatchError :: StagedMockChain a -> (MockChainError -> StagedMockChain a) -> MockChainBuiltin a

-- | A 'StagedMockChain' is a mockchain that can be modified using
-- 'Cooked.Tweak.Common.Tweak's whenever a transaction is being sent for
-- validation. Selecting which transactions should be modified before going to
-- validations is done using 'Cooked.Ltl.Ltl' formulas.
type StagedMockChain = Staged (LtlOp (UntypedTweak InterpMockChain) MockChainBuiltin)

instance Alternative StagedMockChain where
  empty = Instr (Builtin Empty) Return
  a <|> b = Instr (Builtin (Alt a b)) Return

instance MonadFail StagedMockChain where
  fail msg = Instr (Builtin (Fail msg)) Return

-- * 'InterpLtl' instance

instance (MonadPlus m) => MonadPlus (MockChainT m) where
  mzero = lift mzero
  mplus = combineMockChainT mplus

instance InterpLtl (UntypedTweak InterpMockChain) MockChainBuiltin InterpMockChain where
  interpBuiltin GetParams = getParams
  interpBuiltin (SetParams params) = setParams params
  interpBuiltin (ValidateTxSkel skel) = do
    modifications <- gets nowLaterList
    msum . (modifications <&>) $
      \(now, later) -> do
        (_, skel') <-
          lift . (`runTweakInChain` skel) $
            foldr
              (\(UntypedTweak tweak, mode) acc -> if mode then tweak >> acc else ensureFailingTweak tweak >> acc)
              doNothingTweak
              now
        put later
        validateTxSkel skel'
  interpBuiltin (TxSkelOutByRef o) = txSkelOutByRef o
  interpBuiltin (WaitNSlots s) = waitNSlots s
  interpBuiltin AllUtxos = allUtxos
  interpBuiltin (UtxosAt address) = utxosAt address
  interpBuiltin Empty = mzero
  interpBuiltin (Alt l r) = interpLtl l `mplus` interpLtl r
  interpBuiltin (Fail msg) = fail msg
  interpBuiltin (ThrowError err) = throwError err
  interpBuiltin (CatchError act handler) = catchError (interpLtl act) (interpLtl . handler)
  interpBuiltin (LogEvent entry) = logEvent entry
  interpBuiltin (Define name hash) = define name hash
  interpBuiltin (SetConstitutionScript script) = setConstitutionScript script
  interpBuiltin GetConstitutionScript = getConstitutionScript
  interpBuiltin (GetCurrentReward cred) = getCurrentReward cred
  interpBuiltin (ForceOutputs outs) = forceOutputs outs

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
type MonadModalBlockChain m = (MonadBlockChain m, MonadModal m, Modification m ~ UntypedTweak InterpMockChain)

fromTweak :: Tweak m a -> Ltl (UntypedTweak m)
fromTweak = LtlAtom . UntypedTweak

-- | Apply a 'Tweak' to some transaction in the given Trace. The tweak must
-- apply at least once.
somewhere :: (MonadModalBlockChain m) => Tweak InterpMockChain b -> m a -> m a
somewhere = somewhere' . fromTweak

-- | Apply an Ltl modification somewhere in the given Trace. The modification
-- must apply at least once.
somewhere' :: (MonadModal m) => Ltl (Modification m) -> m a -> m a
somewhere' = modifyLtl . eventually'

-- | Apply a 'Tweak' to every transaction in a given trace. This is also
-- successful if there are no transactions at all.
everywhere :: (MonadModalBlockChain m) => Tweak InterpMockChain b -> m a -> m a
everywhere = everywhere' . fromTweak

-- | Apply an Ltl modification everywhere it can be (including nowhere if it
-- does not apply). If the modification branches, this will branch at every
-- location the modification can be applied.
everywhere' :: (MonadModal m) => Ltl (Modification m) -> m a -> m a
everywhere' = modifyLtl . always'

-- | Ensures a given 'Tweak' can never successfully be applied in a computation
nowhere :: (MonadModalBlockChain m) => Tweak InterpMockChain b -> m a -> m a
nowhere = nowhere' . fromTweak

-- | Ensures a given Ltl modification can never be applied on a computation
nowhere' :: (MonadModal m) => Ltl (Modification m) -> m a -> m a
nowhere' = modifyLtl . never'

-- | Apply a given 'Tweak' at every location in a computation where it does not
-- fail, which might never occur.
whenAble :: (MonadModalBlockChain m) => Tweak InterpMockChain b -> m a -> m a
whenAble = whenAble' . fromTweak

-- | Apply an Ltl modification at every location in a computation where it is
-- possible. Does not fail if no such position exists.
whenAble' :: (MonadModal m) => Ltl (Modification m) -> m a -> m a
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
there' :: (MonadModal m) => Integer -> Ltl (Modification m) -> m a -> m a
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

-- * 'MonadBlockChain' and 'MonadMockChain' instances

singletonBuiltin :: builtin a -> Staged (LtlOp modification builtin) a
singletonBuiltin b = Instr (Builtin b) Return

instance MonadError MockChainError StagedMockChain where
  throwError = singletonBuiltin . ThrowError
  catchError act handler = singletonBuiltin $ CatchError act handler

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
