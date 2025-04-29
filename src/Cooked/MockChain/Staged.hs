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
    runTweak,
    everywhere,
    withTweak,
    there,
  )
where

import Cardano.Node.Emulator qualified as Emulator
import Control.Applicative
import Control.Monad (MonadPlus (..), msum)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Cooked.InitialDistribution
import Cooked.Ltl
import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
import Cooked.Pretty.Hashable
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Default
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- * Interpreting and running 'StagedMockChain'

-- | Interprets the staged mockchain then runs the resulting computation with a
-- custom function. This can be used, for example, to supply a custom
-- 'InitialDistribution' by providing 'runMockChainTFrom'.
interpretAndRunWith ::
  (forall m. (Monad m) => MockChainT m a -> m res) ->
  StagedMockChain a ->
  [res]
interpretAndRunWith f smc = f $ interpret smc

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
interpret = flip evalStateT [] . interpLtlAndPruneUnfinished

-- * 'StagedMockChain': An AST for 'MonadMockChain' computations

-- | Abstract representation of all the builtin functions of a 'MonadBlockChain'
data MockChainBuiltin a where
  -- methods of 'MonadBlockChain'
  GetParams :: MockChainBuiltin Emulator.Params
  SetParams :: Emulator.Params -> MockChainBuiltin ()
  ValidateTxSkel :: TxSkel -> MockChainBuiltin Ledger.CardanoTx
  TxOutByRef :: Api.TxOutRef -> MockChainBuiltin (Maybe TxSkelOut)
  GetCurrentSlot :: MockChainBuiltin Ledger.Slot
  AwaitSlot :: Ledger.Slot -> MockChainBuiltin Ledger.Slot
  AllUtxos :: MockChainBuiltin [(Api.TxOutRef, TxSkelOut)]
  UtxosAt :: Api.Address -> MockChainBuiltin [(Api.TxOutRef, TxSkelOut)]
  LogEvent :: MockChainLogEntry -> MockChainBuiltin ()
  Define :: (ToHash a) => String -> a -> MockChainBuiltin a
  SetConstitutionScript :: (Script.ToVersioned Script.Script s) => s -> MockChainBuiltin ()
  -- | The empty set of traces
  Empty :: MockChainBuiltin a
  -- | The union of two sets of traces
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
  interpBuiltin (ValidateTxSkel skel) =
    get
      >>= msum
        . map (uncurry interpretNow)
        . nowLaterList
    where
      interpretNow ::
        UntypedTweak InterpMockChain ->
        [Ltl (UntypedTweak InterpMockChain)] ->
        StateT [Ltl (UntypedTweak InterpMockChain)] InterpMockChain Ledger.CardanoTx
      interpretNow (UntypedTweak now) later = do
        (_, skel') <- lift $ runTweakInChain now skel
        put later
        validateTxSkel skel'
  interpBuiltin (TxOutByRef o) = txOutByRef o
  interpBuiltin GetCurrentSlot = currentSlot
  interpBuiltin (AwaitSlot s) = awaitSlot s
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

-- ** Helpers to run tweaks for use in tests for tweaks

-- | Runs a 'Tweak' from a given 'TxSkel' within a mockchain
runTweak :: Tweak InterpMockChain a -> TxSkel -> [MockChainReturn (a, TxSkel)]
runTweak = runTweakFrom def

-- | Runs a 'Tweak' from a given 'TxSkel' and 'InitialDistribution' within a
-- mockchain
runTweakFrom :: InitialDistribution -> Tweak InterpMockChain a -> TxSkel -> [MockChainReturn (a, TxSkel)]
runTweakFrom initDist tweak = runMockChainTFrom initDist . runTweakInChain tweak

-- ** Modalities

-- | A modal mock chain is a mock chain that allows us to use LTL modifications
-- with 'Tweak's
type MonadModalBlockChain m = (MonadBlockChain m, MonadModal m, Modification m ~ UntypedTweak InterpMockChain)

-- | Apply a 'Tweak' to some transaction in the given Trace. The tweak must
-- apply at least once.
somewhere :: (MonadModalBlockChain m) => Tweak InterpMockChain b -> m a -> m a
somewhere = modifyLtl . LtlUntil LtlTruth . LtlAtom . UntypedTweak

-- | Apply a 'Tweak' to every transaction in a given trace. This is also
-- successful if there are no transactions at all.
everywhere :: (MonadModalBlockChain m) => Tweak InterpMockChain b -> m a -> m a
everywhere = modifyLtl . LtlRelease LtlFalsity . LtlAtom . UntypedTweak

-- | Apply a 'Tweak' to the (0-indexed) nth transaction in a given
-- trace. Successful when this transaction exists and can be modified.
there :: (MonadModalBlockChain m) => Integer -> Tweak InterpMockChain b -> m a -> m a
there n = modifyLtl . mkLtlFormula n
  where
    mkLtlFormula x =
      if x == 0
        then LtlAtom . UntypedTweak
        else LtlNext . mkLtlFormula (x - 1)

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
  txOutByRef = singletonBuiltin . TxOutByRef
  utxosAt = singletonBuiltin . UtxosAt
  logEvent = singletonBuiltin . LogEvent

instance MonadBlockChainWithoutValidation StagedMockChain where
  allUtxos = singletonBuiltin AllUtxos
  setParams = singletonBuiltin . SetParams
  currentSlot = singletonBuiltin GetCurrentSlot
  awaitSlot = singletonBuiltin . AwaitSlot
  define name = singletonBuiltin . Define name
  setConstitutionScript = singletonBuiltin . SetConstitutionScript

instance MonadBlockChain StagedMockChain where
  validateTxSkel = singletonBuiltin . ValidateTxSkel
