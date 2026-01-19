{-# LANGUAGE TemplateHaskell #-}

module Cooked.Effectful where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad (guard, msum, unless)
import Cooked.Families (type (++))
import Cooked.Ltl (Ltl, Requirement (..), finished, nowLaterList)
import Cooked.MockChain.BlockChain (MockChainError (..), MockChainLogEntry)
import Cooked.MockChain.Direct (MockChainBook (..))
import Cooked.MockChain.MockChainState (MockChainState (..), mcstConstitutionL, mcstLedgerStateL)
import Cooked.Pretty.Hashable (ToHash, toHash)
import Cooked.Skeleton (ToVScript, TxSkel, TxSkelOut, VScript)
import Data.Default
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Ledger.Slot qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error (Error, runError, throw)
import Polysemy.Fail (Fail (Fail))
import Polysemy.Internal (Raise)
import Polysemy.Internal.Combinators (stateful)
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer (Writer, runWriter, tell)

-- * ModifyGlobally

-- | An effect to modify a computation with a Ltl Formula. The idea is that the
-- formula pinpoints location where a modification should either be applied or
-- yield an empty computation (when negated).
data ModifyGlobally a :: Effect where
  ModifyLtl :: Ltl a -> m b -> ModifyGlobally a m b

makeSem ''ModifyGlobally

-- | Running the `ModifyGlobally` effect requires to have access of the current
-- list of Ltl formulas, and to be able to return an empty computation. A new
-- formula is appended at the head of the current list of formula. Then, the
-- actual computation is run, after which the newly added formula must be
-- finished, otherwise the empty computation is returned.
runModifyGlobally ::
  forall modification effs a.
  ( Members
      '[ State [Ltl modification],
         NonDet
       ]
      effs
  ) =>
  Sem (ModifyGlobally modification ': effs) a ->
  Sem effs a
runModifyGlobally =
  interpretH $ \case
    ModifyLtl formula comp -> do
      modify (formula :)
      -- TODO : this is type-correct, but does it have the right semantics?
      -- It seems weird to "run it twice" and recursively call the runner
      -- that is currently being defined, which I assumed was already done
      -- by "interpretH".
      comp' <- runT comp
      res <- raise $ runModifyGlobally comp'
      formulas <- get
      unless (null formulas) $ do
        guard (finished (head formulas))
        put (tail formulas)
      return res

-- * ModifyLocally

-- | An effect to request and consume the modifications to be applied at the
-- current time step.
data ModifyLocally a :: Effect where
  GetRequirements :: ModifyLocally a m [Requirement a]

makeSem ''ModifyLocally

-- | Running the `ModifyLocally` effect requires to have access of the current
-- list of Ltl formulas, and to be able to branch. The function `nowLaterList`
-- is invoked to fetch the various paths implied by the current formulas, and a
-- branching is performed to explore all of them. The new formulas are stored,
-- and each path is given the requirements to satisfy at the current time step.
runModifyLocally ::
  forall modification effs a.
  ( Members
      '[ State [Ltl modification],
         NonDet
       ]
      effs
  ) =>
  Sem (ModifyLocally modification : effs) a ->
  Sem effs a
runModifyLocally =
  interpret $ \GetRequirements -> do
    modifications <- gets nowLaterList
    msum . (modifications <&>) $ \(now, later) -> put later >> return now

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

-- * Fail

-- | A possible semantics for fail that is interpreted in terms of Error. It
-- could also technically be run in NonDet but the error message would be lost
-- if transformed to mzero. This might not be the soundest interpretation, but
-- this does the job. After all, the only use for this effect will be to allow
-- partial assignments in our monadic setting.
runFail ::
  forall effs a.
  (Member (Error MockChainError) effs) =>
  Sem (Fail : effs) a ->
  Sem effs a
runFail = interpret $ \case
  Fail s -> throw $ FailWith s

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

-- | This interpretation is fully domain-based
runMockChainRead ::
  forall effs a.
  ( Members
      '[ State MockChainState,
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
  AllUtxos ->
    gets $
      mapMaybe
        ( \(oRef, (txSkelOut, isAvailable)) ->
            if isAvailable
              then
                Just (oRef, txSkelOut)
              else Nothing
        )
        . Map.toList
        . mcstOutputs
  -- TODO : I could technically reinterpret UtxosAt in terms of AllUtxos when it
  -- is available (in the emulator) but I don't want to go through the hassle of
  -- forwarding by hand all the other constructors.
  UtxosAt (Script.toAddress -> addr) ->
    gets $
      mapMaybe
        ( \(oRef, (txSkelOut, isAvailable)) ->
            if isAvailable && Script.toAddress txSkelOut == addr
              then
                Just (oRef, txSkelOut)
              else Nothing
        )
        . Map.toList
        . mcstOutputs
  CurrentSlot -> gets (Emulator.getSlot . mcstLedgerState)
  GetConstitutionScript -> gets (view mcstConstitutionL)
  GetCurrentReward (Script.toCredential -> cred) -> do
    stakeCredential <- undefined
    gets
      ( fmap (Api.Lovelace . Cardano.unCoin)
          . Emulator.getReward stakeCredential
          . view mcstLedgerStateL
      )

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
  LogEvent :: MockChainLogEntry -> MockChainWrite m ()
  Define :: (ToHash a) => String -> a -> MockChainWrite m a

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
    -- TODO : Ideally, I would want to avoid having a second NonDet in tweakEffs, and instead:
    -- - Use the top NonDet when ensuring a tweak fails
    -- - Forward to the NonDet in effs to apply tweaks
    -- It seems I can't do it because of the limitations of Members and raise_
    Member NonDet tweakEffs,
    -- TODO : do we have a more flexible equivalent of raise (Typically Members) that
    -- can be translated to some concrete transformations, like Raise allows?
    Raise tweakEffs effs
  ) =>
  Sem (MockChainWrite : effs) a ->
  Sem (MockChainWrite : effs) a
-- TODO : I used reinterpret instead of intercept because it does not force
-- the effect to be on top of the stack, which I do want. Is this the right
-- way to proceed?
interceptMockChainWriteWithTweak = reinterpret @MockChainWrite $ \case
  ValidateTxSkel skel -> do
    requirements <- getRequirements
    let sumTweak =
          foldr
            ( \req acc -> case req of
                Apply (UntypedTweak tweak) -> tweak >> acc
                EnsureFailure (UntypedTweak tweak) -> do
                  txSkel' <- getTxSkel
                  results <- raise_ $ runNonDet @[] $ runTweak txSkel' tweak
                  -- TODO : there are 2 NonDet on the stack, which once
                  -- will be used? I'm assuming the first occurrence, starting
                  -- from the top of the stack.
                  guard $ null results
                  acc
            )
            (return ())
            requirements
        sumTweakRaised :: Sem effs TxSkel
        sumTweakRaised = raise_ $ subsume $ fst <$> runTweak skel sumTweak
    newTxSkel <- raise_ sumTweakRaised
    validateTxSkel newTxSkel
  -- TODO : can we factor this ??
  ForceOutputs outs -> forceOutputs outs
  WaitNSlots n -> waitNSlots n
  SetConstitutionScript script -> setConstitutionScript script
  SetParams params -> setParams params
  LogEvent event -> logEvent event
  Define name hashable -> define name hashable

-- | Interpreting the 'MockChainWrite' effect is purely domain-specific.
runMockChainWrite ::
  forall effs a.
  ( Members
      '[ State MockChainState,
         Error MockChainError,
         Writer MockChainBook,
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
  LogEvent event -> tell $ MockChainBook [event] Map.empty
  Define name hashable -> tell (MockChainBook [] (Map.singleton (toHash hashable) name)) >> return hashable
  builtin -> undefined

-- * MockChainDirect

-- | A possible stack of effects to handle a direct interpretation of the
-- mockchain, that is without any tweaks nor branching.
type MockChainDirect a =
  Sem
    '[ MockChainWrite,
       MockChainRead,
       Fail,
       Error MockChainError,
       State MockChainState,
       Writer MockChainBook
     ]
    a

runMockChainDirect :: MockChainDirect a -> (MockChainBook, (MockChainState, Either MockChainError a))
runMockChainDirect =
  run
    . runWriter
    . runState def
    . runError
    . runFail
    . runMockChainRead
    . runMockChainWrite

-- * MockChainFull

-- TODO : what I want the users to see are
-- - ModifyGlobally
-- - MockChainWrite
-- - MockChainRead
-- - Fail
-- - NonDet

-- The rest should be hidden and only used for interpretation.
-- I also want users to be able use their own effects on top
-- (or at the bottom, what's the best option there?)
-- of this stacks, such as a new state to manipulate.

-- Should I keep a "MonadBlockChain" type class?. With instance
-- "MockChainDirect" and "MockChainFull"?

type BottomStack =
  '[ MockChainRead,
     Fail,
     Error MockChainError,
     State MockChainState,
     Writer MockChainBook,
     NonDet
   ]

-- | A possible stack of effects to handle staged interpretation of the
-- mockchain, that is with tweaks and branching.
type MockChainFull a =
  Sem
    ( [ ModifyGlobally (UntypedTweak BottomStack),
        MockChainWrite,
        ModifyLocally (UntypedTweak BottomStack),
        State [Ltl (UntypedTweak BottomStack)]
      ]
        ++ BottomStack
    )
    a

runMockChainFull :: MockChainFull a -> [(MockChainBook, (MockChainState, Either MockChainError a))]
runMockChainFull =
  run
    . runNonDet
    . runWriter
    . runState def
    . runError
    . runFail
    . runMockChainRead
    . evalState []
    . runModifyLocally
    . runMockChainWrite
    . interceptMockChainWriteWithTweak
    . runModifyGlobally
