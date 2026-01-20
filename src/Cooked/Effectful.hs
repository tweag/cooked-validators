{-# LANGUAGE TemplateHaskell #-}

module Cooked.Effectful where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad (guard, msum, unless)
import Cooked.Ltl (Ltl, Requirement (..), finished, nowLaterList)
import Cooked.MockChain.BlockChain (MockChainError (..), MockChainLogEntry)
import Cooked.MockChain.Direct (MockChainBook (..))
import Cooked.MockChain.MockChainState (MockChainState (..), mcstConstitutionL, mcstLedgerStateL)
import Cooked.Pretty.Hashable (ToHash, toHash)
import Cooked.Skeleton (ToVScript, TxSkel, TxSkelOut, VScript)
import Data.Coerce
import Data.Default
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Ledger.Slot qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error (Error (..), mapError, runError, throw)
import Polysemy.Fail (Fail (Fail))
import Polysemy.Internal (Subsume)
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer (Writer, runWriter, tell)

-- * Tweak

-- | An effet that allows to store or retrieve a skeleton from the context
data Tweak :: Effect where
  GetTxSkel :: Tweak m TxSkel
  SetTxSkel :: TxSkel -> Tweak m ()

makeSem ''Tweak

-- | Running a Tweak should be equivalent to running a state monad
runTweak ::
  forall effs a.
  TxSkel ->
  Sem (Tweak : effs) a ->
  Sem effs (TxSkel, a)
runTweak txSkel =
  runState txSkel
    . reinterpret
      ( \case
          GetTxSkel -> get
          SetTxSkel skel -> put skel
      )

-- | An UntypedTweak does three things on top of tweaks:
-- - It erases the return type of the computation
-- - It stacks up a NonDet effect in the effects stacks
-- - It makes the underlying effect stack visible in the type
-- All of these will be useful to use them as modification.
data UntypedTweak effs where
  UntypedTweak :: Sem (Tweak : NonDet : effs) a -> UntypedTweak effs

-- * ToCardanoError

runToCardanoError ::
  forall effs a.
  (Member (Error MockChainError) effs) =>
  Sem (Error Ledger.ToCardanoError : effs) a ->
  Sem effs a
runToCardanoError = mapError (MCEToCardanoError "")

-- * Fail

-- | A possible semantics for fail that is interpreted in terms of Error. It
-- could also technically be run in NonDet but the error message would be lost
-- if transformed to mzero. This might not be the soundest interpretation, but
-- this does the job. After all, the only use for this effect will be to allow
-- partial assignments in our monadic setting.
runFailInMockChainError ::
  forall effs a.
  (Member (Error MockChainError) effs) =>
  Sem (Fail : effs) a ->
  Sem effs a
runFailInMockChainError = interpret $
  \(Fail s) -> throw $ FailWith s

-- * MockChainMisc

-- | An effect that corresponds to extra QOL capabilities of the MockChain
data MockChainMisc :: Effect where
  Define :: (ToHash a) => String -> a -> MockChainMisc m a

makeSem ''MockChainMisc

runMockChainMisc ::
  forall effs a.
  (Member (Writer MockChainBook) effs) =>
  Sem (MockChainMisc : effs) a ->
  Sem effs a
runMockChainMisc = interpret $
  \(Define name hashable) -> do
    tell (MockChainBook [] (Map.singleton (toHash hashable) name))
    return hashable

-- * MockChainRead

-- | An effect that corresponds to querying the current state of the mockchain.
data MockChainRead :: Effect where
  GetParams :: MockChainRead m Emulator.Params
  TxSkelOutByRef :: Api.TxOutRef -> MockChainRead m TxSkelOut
  CurrentSlot :: MockChainRead m Ledger.Slot
  AllUtxos :: MockChainRead m [(Api.TxOutRef, TxSkelOut)]
  UtxosAt :: (Script.ToAddress a) => a -> MockChainRead m [(Api.TxOutRef, TxSkelOut)]
  GetConstitutionScript :: MockChainRead m (Maybe VScript)
  GetCurrentReward :: (Script.ToCredential c) => c -> MockChainRead m (Maybe Api.Lovelace)

makeSem ''MockChainRead

-- | The interpretation for read-only effect in the blockchain state
runMockChainRead ::
  forall effs a.
  ( Members
      '[ State MockChainState,
         Error Ledger.ToCardanoError,
         Error MockChainError
       ]
      effs
  ) =>
  Sem (MockChainRead : effs) a ->
  Sem effs a
runMockChainRead = interpret $ \case
  GetParams -> gets mcstParams
  TxSkelOutByRef oRef -> do
    res <- gets $ Map.lookup oRef . mcstOutputs
    case res of
      Just (txSkelOut, True) -> return txSkelOut
      _ -> throw $ MCEUnknownOutRef oRef
  AllUtxos -> fetchUtxos (const True)
  UtxosAt (Script.toAddress -> addr) -> fetchUtxos ((== addr) . Script.toAddress)
  CurrentSlot -> gets (Emulator.getSlot . mcstLedgerState)
  GetConstitutionScript -> gets (view mcstConstitutionL)
  GetCurrentReward (Script.toCredential -> cred) -> do
    stakeCredential <- undefined
    gets
      ( fmap (Api.Lovelace . Cardano.unCoin)
          . Emulator.getReward stakeCredential
          . view mcstLedgerStateL
      )
  where
    fetchUtxos decide =
      gets $
        mapMaybe
          ( \(oRef, (txSkelOut, isAvailable)) ->
              if isAvailable && decide txSkelOut then Just (oRef, txSkelOut) else Nothing
          )
          . Map.toList
          . mcstOutputs

-- * MockChainLog

-- | An effect to allow logging of mockchain events
data MockChainLog :: Effect where
  LogEvent :: MockChainLogEntry -> MockChainLog m ()

makeSem ''MockChainLog

runMockChainLog ::
  forall effs a.
  (Member (Writer MockChainBook) effs) =>
  Sem (MockChainLog : effs) a ->
  Sem effs a
runMockChainLog = interpret $
  \(LogEvent event) -> tell $ MockChainBook [event] Map.empty

-- * MockChainWrite

-- | An effect that corresponds to all the primitives that are not
-- read-only. They range from actual modification of the index state to storage
-- of logging information.
data MockChainWrite :: Effect where
  WaitNSlots :: Integer -> MockChainWrite m Ledger.Slot
  SetParams :: Emulator.Params -> MockChainWrite m ()
  ValidateTxSkel :: TxSkel -> MockChainWrite m Ledger.CardanoTx
  SetConstitutionScript :: (ToVScript s) => s -> MockChainWrite m ()
  ForceOutputs :: [TxSkelOut] -> MockChainWrite m [Api.TxOutRef]

makeSem ''MockChainWrite

-- | 'MockChainWrite' is subject to be modified by UntypedTweak, when the event
-- is a 'ValidateTxSkel'. To handle that we proposed a reinterpretation of the
-- effect in itself, when the 'ModifyLocally' effect exists in the stack.
interceptMockChainWriteWithTweak ::
  forall tweakEffs effs a.
  ( Members
      '[ ModifyLocally (UntypedTweak tweakEffs),
         NonDet
       ]
      effs,
    Subsume tweakEffs effs
  ) =>
  Sem (MockChainWrite : effs) a ->
  Sem (MockChainWrite : effs) a
interceptMockChainWriteWithTweak = reinterpret @MockChainWrite $ \case
  ValidateTxSkel skel -> do
    requirements <- getRequirements
    let sumTweak :: Sem (Tweak : NonDet : tweakEffs) () =
          foldr
            ( \req acc -> case req of
                Apply (UntypedTweak tweak) -> tweak >> acc
                EnsureFailure (UntypedTweak tweak) -> do
                  txSkel' <- getTxSkel
                  results <- raise_ $ runNonDet @[] $ runTweak txSkel' tweak
                  guard $ null results
                  acc
            )
            (return ())
            requirements
    newTxSkel <- raise $ subsume_ $ fst <$> runTweak skel sumTweak
    validateTxSkel newTxSkel
  a -> send $ coerce a

-- | Interpreting the 'MockChainWrite' effect is purely domain-specific.
runMockChainWrite ::
  forall effs a.
  ( Members
      '[ State MockChainState,
         Error Ledger.ToCardanoError,
         Error MockChainError,
         MockChainLog,
         MockChainRead,
         Fail
       ]
      effs
  ) =>
  Sem (MockChainWrite : effs) a ->
  Sem effs a
runMockChainWrite = interpret $ \case
  ValidateTxSkel skel -> do
    undefined
  ForceOutputs outs -> undefined
  builtin -> undefined

-- * MockChainDirect

-- | A possible stack of effects to handle a direct interpretation of the
-- mockchain, that is without any tweaks nor branching.
type MockChainDirect a =
  Sem
    '[ MockChainWrite,
       MockChainRead,
       MockChainMisc,
       Fail
     ]
    a

runMockChainDirect :: MockChainDirect a -> (MockChainBook, (MockChainState, Either MockChainError a))
runMockChainDirect =
  run
    . runWriter
    . runMockChainLog
    . runState def
    . runError
    . runToCardanoError
    . runFailInMockChainError
    . runMockChainMisc
    . runMockChainRead
    . runMockChainWrite
    . insertAt @4 @[Error Ledger.ToCardanoError, Error MockChainError, State MockChainState, MockChainLog, Writer MockChainBook]

-- * MockChainFull

type TweakStack = '[MockChainRead, Fail, NonDet]

-- | A possible stack of effects to handle staged interpretation of the
-- mockchain, that is with tweaks and branching.
type MockChainFull a =
  Sem
    [ ModifyOnTime (UntypedTweak TweakStack),
      MockChainWrite,
      MockChainMisc,
      MockChainRead,
      Fail,
      NonDet
    ]
    a

runMockChainFull :: MockChainFull a -> [(MockChainBook, (MockChainState, Either MockChainError a))]
runMockChainFull =
  run
    . runNonDet
    . runWriter
    . runMockChainLog
    . runState def
    . runError
    . runToCardanoError
    . runFailInMockChainError
    . runMockChainRead
    . runMockChainMisc
    . evalState []
    . runModifyLocally
    . runMockChainWrite
    . insertAt @6 @[Error Ledger.ToCardanoError, Error MockChainError, State MockChainState, MockChainLog, Writer MockChainBook]
    . interceptMockChainWriteWithTweak
    . runModifyOnTime
    . insertAt @2 @[ModifyLocally (UntypedTweak TweakStack), State [Ltl (UntypedTweak TweakStack)]]
