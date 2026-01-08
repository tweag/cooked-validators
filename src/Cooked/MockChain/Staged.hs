{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides a staged implementation of our `MonadBlockChain`. The
-- motivation behind this is to be able to modify traces using `Cooked.Ltl` and
-- `Cooked.Tweak` while they are interpreted.
module Cooked.MockChain.Staged
  ( interpretAndRunWith,
    interpretAndRun,
    StagedMockChain,
    MockChainBuiltin,
    MockChainTweak,
    MonadModalBlockChain,
    InterpMockChain,
    somewhere,
    somewhere',
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
import Cooked.Ltl
import Cooked.Ltl.Combinators
import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
import Cooked.Pretty.Hashable
import Cooked.Skeleton
import Cooked.Tweak.Common
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api

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
  -- for the 'MonadError MockChainError' instance
  ThrowError :: MockChainError -> MockChainBuiltin a
  CatchError :: StagedMockChain a -> (MockChainError -> StagedMockChain a) -> MockChainBuiltin a

-- | A 'StagedMockChain' is an AST of mockchain builtins wrapped into @LtlOp@ to
-- be subject to @Ltl@ modifications.
type StagedMockChain = StagedLtl MockChainTweak MockChainBuiltin

instance Alternative StagedMockChain where
  empty = singletonBuiltin Empty
  a <|> b = singletonBuiltin $ Alt a b

instance MonadPlus StagedMockChain where
  mzero = empty
  mplus = (<|>)

instance MonadFail StagedMockChain where
  fail = singletonBuiltin . ThrowError . FailWith

instance MonadError MockChainError StagedMockChain where
  throwError = singletonBuiltin . ThrowError
  catchError act = singletonBuiltin . CatchError act

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

-- | The domain in which 'StagedMockChain' gets interpreted
type InterpMockChain = MockChainT []

-- | Tweaks operating within the 'InterpMockChain' domain
type MockChainTweak = UntypedTweak InterpMockChain

instance ModInterpBuiltin MockChainTweak MockChainBuiltin InterpMockChain where
  modifyAndInterpBuiltin = \case
    GetParams -> Left getParams
    SetParams params -> Left $ setParams params
    ValidateTxSkel skel -> Right $ \now -> do
      (_, skel') <-
        (`runTweakInChain` skel) $
          foldr
            ( \req acc -> case req of
                Apply (UntypedTweak tweak) -> tweak >> acc
                EnsureFailure (UntypedTweak tweak) -> ensureFailingTweak tweak >> acc
            )
            doNothingTweak
            now
      validateTxSkel skel'
    TxSkelOutByRef o -> Left $ txSkelOutByRef o
    WaitNSlots s -> Left $ waitNSlots s
    AllUtxos -> Left allUtxos
    UtxosAt address -> Left $ utxosAt address
    LogEvent entry -> Left $ logEvent entry
    Define name hash -> Left $ define name hash
    SetConstitutionScript script -> Left $ setConstitutionScript script
    GetConstitutionScript -> Left getConstitutionScript
    GetCurrentReward cred -> Left $ getCurrentReward cred
    ForceOutputs outs -> Left $ forceOutputs outs
    Empty -> Left mzero
    Alt l r -> Left $ interpStagedLtl l `mplus` interpStagedLtl r
    ThrowError err -> Left $ throwError err
    CatchError act handler -> Left $ catchError (interpStagedLtl act) (interpStagedLtl . handler)

-- | Interprets the staged mockchain then runs the resulting computation with a
-- custom function. This can be used, for example, to supply a custom
-- 'InitialDistribution' by providing 'runMockChainTFromInitDist'.
interpretAndRunWith :: (forall m. (Monad m) => MockChainT m a -> m res) -> StagedMockChain a -> [res]
interpretAndRunWith f = f . interpStagedLtl

-- | Same as 'interpretAndRunWith' but using 'runMockChainT' as the default way
-- to run the computation.
interpretAndRun :: StagedMockChain a -> [MockChainReturn a]
interpretAndRun = interpretAndRunWith runMockChainT

-- * Modalities

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
-- See also `Cooked.Tweak.Labels.labelled` to select transactions based on
-- labels instead of their index.
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
withTweak :: (MonadModalBlockChain m) => m a -> Tweak InterpMockChain b -> m a
withTweak = flip (there 0)
