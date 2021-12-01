{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.MockChain.Monad.Staged where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.List
import Control.Monad.Operational
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Monad.Writer
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Time
import Cooked.Tx.Constraints
import qualified Data.Map as M
import qualified Ledger as Pl
import qualified PlutusTx as Pl (FromData)
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

-- TODO: How should we go about modifying the generated traces?
-- At first, I believed it could make sense to have some modifiers on the
-- StagedMockChain level that work as:
--
-- > mapTxSkel :: (TxSkel -> TxSkel) -> StagedMockChain a -> StagedMockChain a
--
-- Because we're modifying the tree, we are able to map over the
-- bind arguments easily:
--
-- > mapTxSkel f (Bind x m) = Bind (mapTxSkel f x) (mapTxSkel f . m)
--
-- The problem starts when we want to apply f to just one transaction;
--
-- > onOne :: (TxSkel -> TxSkel) -> StagedMockChain a -> [StagedMockChain a]
-- > onOne f (Bind x m) = do
-- >   x' <- onOne f x
-- >   return $ Bind x' ???
--
-- Maybe the solution is to rely on the transformer layer? (again!)
--
-- > onOne :: (TxSkel -> TxSkel) -> StagedMockChain a -> StagedMockChainT [] a
--
-- Then, it would actually make sense to change the interpretT type to
--
-- > interpretT :: forall m a. (Monad m) => StagedMockChainT m a -> m (MockChain a)
--
-- Now, running `interpretT (onOne f tr)` will return a list of `MockChain a`, which
-- we can run individually.

-- | This is an initial encoding of the MockChain operations, it provides
--  a simple way of altering the AST of a trace before actually executing it.
--  On top of the operations from 'MonadMockChain' we also have 'Fail' to make
--  sure the resulting monad will be an instance of 'MonadFail'.
data MockChainOp a where
  ValidateTxSkel :: TxSkel -> MockChainOp ()
  Index :: MockChainOp (M.Map Pl.TxOutRef Pl.TxOut)
  GetSlotCounter :: MockChainOp SlotCounter
  ModifySlotCounter :: (SlotCounter -> SlotCounter) -> MockChainOp ()
  UtxosSuchThat ::
    (Pl.FromData a) =>
    Pl.Address ->
    (Maybe a -> Pl.Value -> Bool) ->
    MockChainOp [(SpendableOut, Maybe a)]
  Fail :: String -> MockChainOp a

type StagedMockChainT = ProgramT MockChainOp

type StagedMockChain = StagedMockChainT Identity

instance (Monad m) => MonadFail (StagedMockChainT m) where
  fail = singleton . Fail

instance (Monad m) => MonadMockChain (StagedMockChainT m) where
  validateTxSkel = singleton . ValidateTxSkel
  index = singleton Index
  slotCounter = singleton GetSlotCounter
  modifySlotCounter = singleton . ModifySlotCounter
  utxosSuchThat addr = singleton . UtxosSuchThat addr

-- | Interprets a single operation in the direct 'MockChainT' monad.
interpretOp :: (Monad m) => MockChainOp a -> MockChainT m a
interpretOp (ValidateTxSkel skel) = validateTxSkel skel
interpretOp Index = index
interpretOp GetSlotCounter = slotCounter
interpretOp (ModifySlotCounter f) = modifySlotCounter f
interpretOp (UtxosSuchThat addr predi) = utxosSuchThat addr predi
interpretOp (Fail str) = fail str

int :: (Monad m) => StagedMockChain a -> MockChainT m a
int = interpretWithMonad interpretOp . liftProgram

-- Alright; here's one interpretation of a stagedmockchain that returns a list
-- of traces, each of which where one transaction was altered by f;
--
-- PROBLEM: how do we pass parameters that were discovered in between
-- the trace to the transformation? Maybe this really has to become a method
-- in MonadMockChain as proposed by GR
onOne :: (TxSkel -> TxSkel) -> StagedMockChain a -> MockChainT (WriterT TraceDescr []) a
onOne t = go . view
  where
    go :: ProgramView MockChainOp a -> MockChainT (WriterT TraceDescr []) a
    go (Return a) = return a
    go (instr@(ValidateTxSkel skel) :>>= f) =
      ( lift (tell $ trSingleton "NEXT WAS:")
          >> lift (tell $ prettyMockChainOp instr)
          >> lift (tell $ trSingleton "BECAME:")
          >> lift (tell $ prettyMockChainOp (ValidateTxSkel $ t skel))
          >> interpretOp (ValidateTxSkel $ t skel) >>= int . f
      )
        <|> ( lift (tell $ prettyMockChainOp instr)
                >> interpretOp (ValidateTxSkel skel) >>= onOne t . f
            )
    go (instr :>>= f) =
      interpretOp instr >>= onOne t . f

-- | Similar to interpret; but produces a description of the transactions that were
--  issued to the mockchain as evaluation happens. There is no way of producing a
--  description /without/ evaluating the script because of how we encoded /bind/.
--  We need the result of the previous computation to generate the new AST, which
--  then gets interpreted.
interpretWithDescrT ::
  forall m a.
  (Monad m) =>
  StagedMockChainT m a ->
  MockChainT (WriterT TraceDescr m) a
interpretWithDescrT = join . lift . lift . fmap eval . viewT
  where
    eval :: ProgramViewT MockChainOp m a -> MockChainT (WriterT TraceDescr m) a
    eval (Return a) = return a
    eval (instr :>>= f) =
      lift (tell $ prettyMockChainOp instr)
        >> interpretOp instr
        >>= interpretWithDescrT . f

interpretWithDescr :: forall a. StagedMockChain a -> MockChainT (Writer TraceDescr) a
interpretWithDescr = interpretWithDescrT @Identity

-- * Human Readable Traces

-- | Generates a 'TraceDescr'iption for the given operation; we're mostly interested in seeing
--  the transactions that were validated, so many operations have no description.
prettyMockChainOp :: MockChainOp a -> TraceDescr
prettyMockChainOp (ValidateTxSkel skel) =
  trSingleton $ PP.hang 2 $ PP.vsep ["ValidateTxSkel", prettyTxSkel skel]
prettyMockChainOp (Fail reason) =
  trSingleton $ PP.hang 2 $ PP.vsep ["Fail", PP.pretty reason]
prettyMockChainOp _ = mempty

-- | A 'TraceDescr' is a list of 'Doc' encoded as a difference list for
--  two reasons (check 'ShowS' if you're confused about how this works, its the same idea).
--    1) Naturally, these make for efficient concatenation
--    2) More importantly, this makes it easy to define the empty 'TraceDescr'
--       as @TraceDescr id@ instead of reliying on 'PP.emptyDoc', which generates
--       empty lines when used with 'PP.vsep'. This avoids generating these empty lines
newtype TraceDescr = TraceDescr {trApp :: [Doc ()] -> [Doc ()]}

trSingleton :: Doc ann -> TraceDescr
trSingleton d = TraceDescr (fmap (const ()) d :)

instance Show TraceDescr where
  show (TraceDescr gen) =
    let tr = gen []
        numbered = zipWith (\n d -> PP.pretty n <> ")" <+> PP.align d) [1 :: Integer ..] tr
     in PP.renderString . PP.layoutPretty PP.defaultLayoutOptions $ PP.vsep numbered

instance Semigroup TraceDescr where
  x <> y = TraceDescr $ trApp x . trApp y

instance Monoid TraceDescr where
  mempty = TraceDescr id
