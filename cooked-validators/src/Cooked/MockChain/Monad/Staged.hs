{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.MockChain.Monad.Staged where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer.Strict hiding (Alt)
import Cooked.Attack
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
interpretAndRunWith f smc = flip evalStateT LtlTruth $ runWriterT $ f (interpLtl smc)

-- | Interprets and runs the mockchain computation from the default 'InitialDistribution'
interpretAndRun ::
  StagedMockChain a ->
  [(Either MockChainError (a, UtxoState), TraceDescr)]
interpretAndRun = interpretAndRunWith runMockChainT

-- * Interpreting 'StagedMockChain'

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

type MockChainOp = LtlOp Attack MockChainBuiltin

type StagedMockChain = Staged MockChainOp

singleton :: op a -> Staged op a
singleton x = Instr x Return

-- * Instances of HasBuiltins

-- type Attack = TxSkel -> Maybe TxSkel

instance {-# OVERLAPS #-} Semigroup Attack where
  f <> g = maybe Nothing f . g

instance {-# OVERLAPS #-} Monoid Attack where
  mempty = Just

type InterpMockChainLtl = MockChainT (WriterT TraceDescr (StateT (Ltl Attack) []))

interpLtl :: StagedMockChain a -> InterpMockChainLtl a
-- The following five equations should always be the same, no matter what the
-- builtins are. Do we have a nice way to abstract them away?
interpLtl (Return a) = lift get >>= \x -> if finished x then return a else empty
interpLtl (Instr Empty _) = empty
interpLtl (Instr (Alt l r) f) = interpLtl (l >>= f) <|> interpLtl (r >>= f)
interpLtl (Instr (StartLtl new) f) = lift get >>= \old -> lift (put (LtlAnd new old)) >>= interpLtl . f
interpLtl (Instr StopLtl f) = lift get >>= \x -> if finished x then interpLtl $ f () else empty
interpLtl (Instr (Fail msg) _) = fail msg
-- now the MockChain-specific cases: first the two difficult/interesting ones:
interpLtl (Instr (Builtin (ValidateTxSkel skel)) f) = do
  x <- lift get
  nonFailing $
    map
      ( \(m, y) -> do
          txid <- maybe empty validateTxSkel (m skel)
          lift $ put y
          interpLtl $ f txid
      )
      (nowLater x)
  where
    nonFailing [] = empty
    nonFailing (w : ws) = w <|> nonFailing ws
interpLtl (Instr (Builtin (SigningWith ws tr)) f) = signingWith ws (interpLtl tr) >>= interpLtl . f
-- now the easy ones:
interpLtl (Instr (Builtin (TxOutByRef oref)) f) = txOutByRef oref >>= interpLtl . f
interpLtl (Instr (Builtin GetCurrentSlot) f) = currentSlot >>= interpLtl . f
interpLtl (Instr (Builtin (AwaitSlot s)) f) = awaitSlot s >>= interpLtl . f
interpLtl (Instr (Builtin GetCurrentTime) f) = currentTime >>= interpLtl . f
interpLtl (Instr (Builtin (AwaitTime t)) f) = awaitTime t >>= interpLtl . f
interpLtl (Instr (Builtin (UtxosSuchThat a p)) f) = utxosSuchThat a p >>= interpLtl . f
interpLtl (Instr (Builtin OwnPubKey) f) = ownPaymentPubKeyHash >>= interpLtl . f
interpLtl (Instr (Builtin AskSigners) f) = askSigners >>= interpLtl . f
interpLtl (Instr (Builtin GetSlotConfig) f) = slotConfig >>= interpLtl . f

singletonBuiltin :: builtin a -> Staged (LtlOp modification builtin) a
singletonBuiltin b = Instr (Builtin b) Return

instance MonadBlockChain StagedMockChain where
  validateTxSkel = singletonBuiltin . ValidateTxSkel
  utxosSuchThat a p = singletonBuiltin (UtxosSuchThat a p)
  txOutByRef = singletonBuiltin . TxOutByRef
  ownPaymentPubKeyHash = singletonBuiltin OwnPubKey
  currentSlot = singletonBuiltin GetCurrentSlot
  currentTime = singletonBuiltin GetCurrentTime
  awaitSlot = singletonBuiltin . AwaitSlot
  awaitTime = singletonBuiltin . AwaitTime

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
