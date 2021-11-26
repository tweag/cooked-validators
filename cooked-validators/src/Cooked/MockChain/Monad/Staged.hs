{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.MockChain.Monad.Staged where

import Control.Monad.Identity
import Control.Monad.Operational
import Control.Monad.Trans
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Time
import Cooked.Tx.Constraints
import qualified Data.Map as M
import qualified Ledger as Pl
import qualified PlutusTx as Pl (FromData)

-- | This is an initial encoding of the MockChain operations, it provides
--  a simple way of altering the AST of a trace before actually executing it.
--  On top of the operations from 'MonadMockChain' we also have 'Fail' to make
--  sure the resulting monad will be an instance of 'MonadFail'.
data MockChainOp a where
  GenerateTx :: TxSkel -> MockChainOp Pl.Tx
  ValidateTx :: Pl.Tx -> MockChainOp ()
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
  generateTx = singleton . GenerateTx
  validateTx = singleton . ValidateTx
  index = singleton Index
  slotCounter = singleton GetSlotCounter
  modifySlotCounter = singleton . ModifySlotCounter
  utxosSuchThat addr = singleton . UtxosSuchThat addr

-- | Interprets a single operation in the direct 'MockChainT' monad.
interpretOp :: (Monad m) => MockChainOp a -> MockChainT m a
interpretOp (GenerateTx skel) = generateTx skel
interpretOp (ValidateTx tx) = validateTx tx
interpretOp Index = index
interpretOp GetSlotCounter = slotCounter
interpretOp (ModifySlotCounter f) = modifySlotCounter f
interpretOp (UtxosSuchThat addr predi) = utxosSuchThat addr predi
interpretOp (Fail str) = fail str

-- | Interprets a 'StagedMockChainT' into a 'MockChainT' computation.
interpret :: forall m a. (Monad m) => StagedMockChainT m a -> MockChainT m a
interpret = join . lift . fmap eval . viewT
  where
    eval :: ProgramViewT MockChainOp m a -> MockChainT m a
    eval (Return a) = return a
    eval (instr :>>= f) = interpretOp instr >>= interpret . f
