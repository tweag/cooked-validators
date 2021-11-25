{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cooked.MockChain.Staged where

import Control.Arrow (second)
import Control.Monad.Identity
import Control.Monad.Operational
import Cooked.MockChain.Monad.Class
import Cooked.MockChain.Time
import Cooked.Tx.Constraints
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Void
import qualified Ledger as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorScript)
import qualified PlutusTx as Pl (FromData)

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
