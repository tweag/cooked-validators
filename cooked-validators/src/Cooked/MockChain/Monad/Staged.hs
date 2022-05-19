{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.MockChain.Monad.Staged where

import Control.Monad.Except
import Control.Monad.Writer.Strict
import Cooked.MockChain.Ltl
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import qualified Data.List.NonEmpty as NE
import qualified Ledger as Pl
import qualified Ledger.TimeSlot as Pl
import qualified PlutusTx as Pl (FromData)
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

-- * Interpreting and Running 'StagedMockChain'

-- | Interprets the staged mockchain then runs the resulting computation
-- with a custom function. This can be used, for example, to supply
-- a custom 'InitialDistribution' by providing 'runMockChainTFrom'.
interpretAndRunWith ::
  (forall m. Monad m => MockChainT m a -> m res) ->
  StagedMockChain a ->
  [(res, TraceDescr)]
interpretAndRunWith f smc = runWriterT $ f (interpret smc)

-- | Interprets and runs the mockchain computation from the default 'InitialDistribution'
interpretAndRun ::
  StagedMockChain a ->
  [(Either MockChainError (a, UtxoState), TraceDescr)]
interpretAndRun = interpretAndRunWith runMockChainT

-- * Interpreting 'StagedMockChain'

-- | The semantic domain in which 'StagedMockChain' gets interpreted; see the
--  'interpret' function for more.
type InterpMockChain = MockChainT (WriterT TraceDescr [])

-- | The 'interpret' function gives semantics to our traces. One
--  'StagedMockChain' computation yields a potential list of 'MockChainT'
--  computations, which emit a description of their operation. Recall a
--  'MockChainT' is a state and except monad composed:
--
--  >     MockChainT (WriterT TraceDescr []) a
--  > =~= st -> (WriterT TraceDescr []) (Either err (a, st))
--  > =~= st -> [(Either err (a, st) , TraceDescr)]
interpret :: StagedMockChain a -> InterpMockChain a
interpret = undefined

data MockChainBuiltin a where
  ValidateTxSkel :: TxSkel -> MockChainBuiltin Pl.TxId
  TxOutByRef :: Pl.TxOutRef -> MockChainBuiltin (Maybe Pl.TxOut)
  GetCurrentSlot :: MockChainBuiltin Pl.Slot
  AwaitSlot :: Pl.Slot -> MockChainBuiltin Pl.Slot
  GetCurrentTime :: MockChainBuiltin Pl.POSIXTime
  AwaitTime :: Pl.POSIXTime -> MockChainBuiltin Pl.POSIXTime
  UtxosSuchThat ::
    (Pl.FromData a) =>
    Pl.Address ->
    (Maybe a -> Pl.Value -> Bool) ->
    MockChainBuiltin [(SpendableOut, Maybe a)]
  OwnPubKey :: MockChainBuiltin Pl.PubKeyHash
  -- the following are only available in MonadMockChain, not MonadBlockChain:
  SigningWith :: NE.NonEmpty Wallet -> StagedMockChain a -> MockChainBuiltin a
  AskSigners :: MockChainBuiltin (NE.NonEmpty Wallet)
  GetSlotConfig :: MockChainBuiltin Pl.SlotConfig

type MockChainOp = Op (TxSkel -> Maybe TxSkel) MockChainBuiltin

type StagedMockChain = Staged MockChainOp

-- interpretOp (ValidateTxSkel skel) = validateTxSkel skel
-- interpretOp (TxOutByRef ref) = txOutByRef ref
-- interpretOp GetCurrentSlot = currentSlot
-- interpretOp GetCurrentTime = currentTime
-- interpretOp (AwaitSlot s) = awaitSlot s
-- interpretOp (AwaitTime t) = awaitTime t
-- interpretOp (UtxosSuchThat addr predi) = utxosSuchThat addr predi
-- interpretOp (Fail str) = fail str
-- interpretOp OwnPubKey = ownPaymentPubKeyHash
-- interpretOp AskSigners = askSigners
-- interpretOp GetSlotConfig = slotConfig
-- interpretOp (SigningWith ws act) = signingWith ws (interpretNonDet act)
-- interpretOp (Modify m' block) = do
--   ms <- lift get
--   lift $ put (ms ++ [m'])
--   interpretNonDet block
-- interpretOp MZero = lift $ lift $ lift mzero
-- interpretOp (MPlus a b) = combineInterpMockChainMod (interpretNonDet a) (interpretNonDet b)
--   where
--     combineInterpMockChainMod ::
--       MonadPlus m =>
--       InterpMockChainMod m a ->
--       InterpMockChainMod m a ->
--       InterpMockChainMod m a
--     combineInterpMockChainMod = combineMockChainT $ \ma mb ->
--       WriterT $
--         StateT $ \s ->
--           runStateT (runWriterT ma) s `mplus` runStateT (runWriterT mb) s

-- | This is an internal type that keeps track of the list of modalities that
-- are present in each branch, it's isomorphic to:
--
--  > St -> ListMods -> m ((Either Err (a, St) , TraceDescr), ListMods)
singleton :: op a -> Staged op a
singleton x = Instr x Return

-- * Instances of HasBuiltins

instance (MonadPlus m, MonadMockChain m) => HasBuiltins (TxSkel -> Maybe TxSkel) MockChainBuiltin m where
  modifyBuiltin m (ValidateTxSkel skel) = maybe mzero validateTxSkel (m skel)
  modifyBuiltin _ (TxOutByRef oref) = txOutByRef oref
  modifyBuiltin _ GetCurrentSlot = currentSlot
  modifyBuiltin _ (AwaitSlot s) = awaitSlot s
  modifyBuiltin _ GetCurrentTime = currentTime
  modifyBuiltin _ (AwaitTime t) = awaitTime t
  modifyBuiltin _ (UtxosSuchThat addr pr) = utxosSuchThat addr pr
  modifyBuiltin _ OwnPubKey = ownPaymentPubKeyHash
  modifyBuiltin _ (SigningWith wal trace) = signingWith wal trace
  modifyBuiltin _ AskSigners = askSigners
  modifyBuiltin _ GetSlotConfig = slotConfig

-- * Human Readable Traces

-- | Generates a 'TraceDescr'iption for the given operation; we're mostly interested in seeing
--  the transactions that were validated, so many operations have no description.
prettyMockChainOp :: NE.NonEmpty Wallet -> MockChainOp a -> TraceDescr
prettyMockChainOp signers (Builtin (ValidateTxSkel skel)) =
  trSingleton $
    PP.hang 2 $
      PP.vsep ["ValidateTxSkel", prettyTxSkel (NE.toList signers) skel]
prettyMockChainOp _ (Fail reason) =
  trSingleton $ PP.hang 2 $ PP.vsep ["Fail", PP.pretty reason]
prettyMockChainOp _ _ = mempty

-- | A 'TraceDescr' is a list of 'Doc' encoded as a difference list for
--  two reasons (check 'ShowS' if you're confused about how this works, its the same idea).
--    1) Naturally, these make for efficient concatenation
--    2) More importantly, this makes it easy to define the empty 'TraceDescr'
--       as @TraceDescr id@ instead of relying on 'PP.emptyDoc', which generates
--       empty lines when used with 'PP.vsep'. This avoids generating these empty lines
newtype TraceDescr = TraceDescr {trApp :: [Doc ()] -> [Doc ()]}

trSingleton :: Doc ann -> TraceDescr
trSingleton d = TraceDescr (void d :)

instance Show TraceDescr where
  show (TraceDescr gen) =
    let tr = gen []
        numbered = zipWith (\n d -> PP.pretty n <> ")" <+> PP.align d) [1 :: Integer ..] tr
     in PP.renderString . PP.layoutPretty PP.defaultLayoutOptions $ PP.vsep numbered

instance Semigroup TraceDescr where
  x <> y = TraceDescr $ trApp x . trApp y

instance Monoid TraceDescr where
  mempty = TraceDescr id
