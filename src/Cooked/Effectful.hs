{-# LANGUAGE TemplateHaskell #-}

module Cooked.Effectful where

import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Control.Monad (guard, msum, unless)
import Cooked.Ltl (Ltl, Requirement, finished, nowLaterList)
import Cooked.MockChain.BlockChain (MockChainError, MockChainLogEntry)
import Cooked.MockChain.MockChainState (MockChainState)
import Cooked.Pretty.Hashable (ToHash)
import Cooked.Skeleton (ToVScript, TxSkel, TxSkelOut, VScript)
import Data.Functor ((<&>))
import Ledger.Slot qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Final
import Polysemy.NonDet
import Polysemy.State

data ModifyGlobally a :: Effect where
  ModifyLtl :: Ltl a -> m b -> ModifyGlobally a m b

makeSem ''ModifyGlobally

runModifyGlobally ::
  forall f r a.
  (Members '[State [Ltl f], NonDet] r) =>
  Sem (ModifyGlobally f ': r) a ->
  Sem r a
runModifyGlobally =
  interpretH $ \case
    ModifyLtl formula comp -> do
      modify (formula :)
      res <- runT comp
      formulas <- get
      unless (null formulas) $ do
        guard (finished (head formulas))
        put (tail formulas)
      pureT res

-- runModifyGlobally ::
--   forall f effs a.
--   ( State [Ltl f] :> effs,
--     NonDet :> effs
--   ) =>
--   Eff (ModifyGlobally f : effs) a ->
--   Eff effs a
-- runModifyGlobally =
--   interpret $
--     \env (ModifyLtl formula comp) -> localSeqUnlift env $ \unlift -> do
--       modify (formula :)
--       res <- unlift comp
--       formulas :: [Ltl f] <- get
--       unless (null formulas) $ do
--         guard $ finished $ head formulas
--         put $ tail formulas
--       return res

-- data ModifyLocally a :: Effect where
--   GetRequirements :: ModifyLocally a m [Requirement a]

-- makeEffect ''ModifyLocally

-- runModifyLocally ::
--   forall f effs a.
--   ( State [Ltl f] :> effs,
--     NonDet :> effs
--   ) =>
--   Eff (ModifyLocally f : effs) a ->
--   Eff effs a
-- runModifyLocally =
--   interpret $ \_ GetRequirements -> do
--     modifications <- gets nowLaterList
--     msum . (modifications <&>) $
--       \(now, later) -> do
--         put later
--         return now

-- data MockChainRead :: Effect where
--   GetParams :: MockChainRead m Emulator.Params
--   TxSkelOutByRef :: Api.TxOutRef -> MockChainRead m TxSkelOut
--   GetSlot :: MockChainRead m Ledger.Slot
--   AllUtxos :: MockChainRead m [(Api.TxOutRef, TxSkelOut)]
--   UtxosAt :: (Script.ToAddress a) => a -> MockChainRead m [(Api.TxOutRef, TxSkelOut)]
--   LogEvent :: MockChainLogEntry -> MockChainRead m ()
--   Define :: (ToHash a) => String -> a -> MockChainRead m a
--   GetConstitutionScript :: MockChainRead m (Maybe VScript)
--   GetCurrentReward :: (Script.ToCredential c) => c -> MockChainRead m (Maybe Api.Lovelace)

-- makeEffect ''MockChainRead

-- data MockChainWrite :: Effect where
--   WaitNSlots :: Integer -> MockChainWrite m Ledger.Slot
--   SetParams :: Emulator.Params -> MockChainWrite m ()
--   ValidateTxSkel :: TxSkel -> MockChainWrite m Ledger.CardanoTx
--   SetConstitutionScript :: (ToVScript s) => s -> MockChainWrite m ()
--   ForceOutputs :: [TxSkelOut] -> MockChainWrite m [Api.TxOutRef]

-- makeEffect ''MockChainWrite

-- data Tweak :: Effect where
--   GetTxSkel :: Tweak m TxSkel
--   SetTxSkel :: TxSkel -> Tweak m ()

-- makeEffect ''Tweak

-- runTweak ::
--   forall effs a.
--   TxSkel ->
--   Eff (Tweak : effs) a ->
--   Eff effs TxSkel
-- runTweak skel = reinterpret (execStateLocal skel) $ \_ -> \case
--   GetTxSkel -> get
--   SetTxSkel skel' -> put skel'

-- data UntypedTweak effs where
--   UntypedTweak :: Eff (Tweak : effs) a -> UntypedTweak effs

-- runUntypedTweak ::
--   forall effs.
--   TxSkel ->
--   UntypedTweak effs ->
--   Eff effs TxSkel
-- runUntypedTweak skel (UntypedTweak tweak) = runTweak skel tweak

-- runMockChain ::
--   forall effs a.
--   ( ModifyLocally (UntypedTweak effs) :> effs,
--     State MockChainState :> effs,
--     Error MockChainError :> effs,
--     Writer [MockChainLogEntry] :> effs,
--     MockChainRead :> effs,
--     Fail :> effs
--   ) =>
--   Eff (MockChainWrite : effs) a ->
--   Eff effs a
-- runMockChain = interpret $ \_ -> \case
--   ValidateTxSkel skel -> do
--     requirements :: [Requirement (UntypedTweak effs)] <- getRequirements
--     undefined
--   ForceOutputs outs -> undefined
--   builtin -> undefined
