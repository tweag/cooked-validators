{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Staged where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer.Strict hiding (Alt)
import Cooked.Attack
import Cooked.Ltl
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import qualified Data.List.NonEmpty as NE
import qualified Ledger as Pl
import qualified PlutusTx as Pl (FromData)
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

-- * Interpreting and running 'StagedMockChain'

-- | Interprets the staged mockchain then runs the resulting computation
-- with a custom function. This can be used, for example, to supply
-- a custom 'InitialDistribution' by providing 'runMockChainTFrom'.
interpretAndRunWith ::
  MonadPlus am =>
  (forall m. Monad m => MockChainT m a -> m res) ->
  StagedMockChain am a ->
  [(res, TraceDescr)]
interpretAndRunWith f smc = runWriterT $ f $ interpret smc

interpretAndRun ::
  MonadPlus am =>
  StagedMockChain am a ->
  [(Either MockChainError (a, UtxoState), TraceDescr)]
interpretAndRun = interpretAndRunWith runMockChainT

-- | The semantic domain in which 'StagedMockChain' gets interpreted; see
--  the 'interpret' function for more.
type InterpMockChain = MockChainT (WriterT TraceDescr [])

-- | The 'interpret' function gives semantics to our traces. One
--  'StagedMockChain' computation yields a potential list of 'MockChainT'
--  computations, which emit a description of their operation. Recall a
--  'MockChainT' is a state and except monad composed:
--
--  >     MockChainT (WriterT TraceDescr []) a
--  > =~= st -> (WriterT TraceDescr []) (Either err (a, st))
--  > =~= st -> [(Either err (a, st) , TraceDescr)]
interpret :: MonadPlus am => StagedMockChain am a -> InterpMockChain a
interpret = flip evalStateT [] . interpLtlAndPruneUnfinished

-- * 'StagedMockChain': An AST for 'MonadMockChain' computations

-- am is the monad attacks live in, a the return type of the reified operation

data MockChainBuiltin am a where
  ValidateTxSkel :: TxSkel -> MockChainBuiltin am Pl.CardanoTx
  TxOutByRef :: Pl.TxOutRef -> MockChainBuiltin am (Maybe Pl.TxOut)
  GetCurrentSlot :: MockChainBuiltin am Pl.Slot
  AwaitSlot :: Pl.Slot -> MockChainBuiltin am Pl.Slot
  GetCurrentTime :: MockChainBuiltin am Pl.POSIXTime
  AwaitTime :: Pl.POSIXTime -> MockChainBuiltin am Pl.POSIXTime
  UtxosSuchThat ::
    (Pl.FromData a) =>
    Pl.Address ->
    (Maybe a -> Pl.Value -> Bool) ->
    MockChainBuiltin am [(SpendableOut, Maybe a)]
  OwnPubKey :: MockChainBuiltin am Pl.PubKeyHash
  -- the following are only available in MonadMockChain, not MonadBlockChain:
  SigningWith :: NE.NonEmpty Wallet -> StagedMockChain am a -> MockChainBuiltin am a
  AskSigners :: MockChainBuiltin am (NE.NonEmpty Wallet)
  GetParams :: MockChainBuiltin am Pl.Params
  LocalParams :: (Pl.Params -> Pl.Params) -> StagedMockChain am a -> MockChainBuiltin am a
  -- the following are not strictly blockchain specific, but they allow us to
  -- combine several traces into one and to signal failure.

  -- | The empty set of traces
  Empty :: MockChainBuiltin am a
  -- | The union of two sets of traces
  Alt ::
    StagedMockChain am a ->
    StagedMockChain am a ->
    MockChainBuiltin am a
  -- | The failing operation
  Fail :: String -> MockChainBuiltin am a

type MockChainOp am = LtlOp (Attack am) (MockChainBuiltin am)

type StagedMockChain am = Staged (MockChainOp am)

instance Alternative (StagedMockChain am) where
  empty = Instr (Builtin Empty) Return
  a <|> b = Instr (Builtin (Alt a b)) Return

instance MonadFail (StagedMockChain am) where
  fail msg = Instr (Builtin (Fail msg)) Return

-- * 'InterpLtl' instance

instance {-# OVERLAPS #-} Monad am => Semigroup (Attack am) where
  -- This means that @a <> b@ is the attack that applies @b@ first and then @a@
  f <> g = g >=> f

instance {-# OVERLAPS #-} MonadPlus am => Monoid (Attack am) where
  mempty = const mzero

instance MonadPlus m => MonadPlus (MockChainT m) where
  mzero = lift mzero
  mplus = combineMockChainT mplus

instance MonadPlus am => InterpLtl (Attack am) (MockChainBuiltin am) InterpMockChain where
  interpBuiltin (ValidateTxSkel skel) =
    get
      >>= msum
        . map (uncurry interpretAndTell)
        . nowLaterList
    where
      interpretAndTell :: Attack am -> [Ltl (Attack am)] -> StateT [Ltl (Attack am)] InterpMockChain Pl.CardanoTx
      interpretAndTell now later = undefined
  -- do
  -- mockSt <- lift get
  -- msum $
  --   map
  --     ( \skel' -> do
  --         signers <- askSigners
  --         lift $ lift $ tell $ prettyMockChainOp signers $ Builtin $ ValidateTxSkel skel'
  --         tx <- validateTxSkel skel'
  --         put later
  --         return tx
  --     )
  --     (now mockSt skel)
  interpBuiltin (SigningWith ws act) = signingWith ws (interpLtl act)
  interpBuiltin (TxOutByRef o) = txOutByRef o
  interpBuiltin GetCurrentSlot = currentSlot
  interpBuiltin (AwaitSlot s) = awaitSlot s
  interpBuiltin GetCurrentTime = currentTime
  interpBuiltin (AwaitTime t) = awaitTime t
  interpBuiltin (UtxosSuchThat a p) = utxosSuchThat a p
  interpBuiltin OwnPubKey = ownPaymentPubKeyHash
  interpBuiltin AskSigners = askSigners
  interpBuiltin GetParams = params
  interpBuiltin (LocalParams f act) = localParams f (interpLtl act)
  interpBuiltin Empty = mzero
  interpBuiltin (Alt l r) = interpLtl l `mplus` interpLtl r
  interpBuiltin (Fail msg) = do
    signers <- askSigners
    lift $ lift $ tell $ prettyMockChainOp signers $ Builtin $ Fail msg
    fail msg

-- ** Modalities

-- | A modal mock chain is a mock chain that allows us to use LTL modifications with 'Attack's
type MonadModalMockChain am m = (MonadMockChain m, MonadModal m, Modification m ~ Attack am)

-- * 'MonadBlockChain' and 'MonadMockChain' instances

singletonBuiltin :: builtin a -> Staged (LtlOp modification builtin) a
singletonBuiltin b = Instr (Builtin b) Return

instance MonadBlockChain (StagedMockChain am) where
  validateTxSkel = singletonBuiltin . ValidateTxSkel
  utxosSuchThat a p = singletonBuiltin (UtxosSuchThat a p)
  txOutByRef = singletonBuiltin . TxOutByRef
  ownPaymentPubKeyHash = singletonBuiltin OwnPubKey
  currentSlot = singletonBuiltin GetCurrentSlot
  currentTime = singletonBuiltin GetCurrentTime
  awaitSlot = singletonBuiltin . AwaitSlot
  awaitTime = singletonBuiltin . AwaitTime

instance MonadMockChain (StagedMockChain am) where
  signingWith ws act = singletonBuiltin (SigningWith ws act)
  askSigners = singletonBuiltin AskSigners
  params = singletonBuiltin GetParams
  localParams f act = singletonBuiltin (LocalParams f act)

-- * Human Readable Traces

-- | Generates a 'TraceDescr'iption for the given operation; we're mostly interested in seeing
--  the transactions that were validated, so many operations have no description.
prettyMockChainOp :: NE.NonEmpty Wallet -> MockChainOp m a -> TraceDescr
prettyMockChainOp signers (Builtin (ValidateTxSkel skel)) =
  trSingleton $
    PP.hang 2 $
      PP.vsep ["ValidateTxSkel", prettyTxSkel (NE.toList signers) skel]
prettyMockChainOp _ (Builtin (Fail reason)) =
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
