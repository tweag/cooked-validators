{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.MockChain.Monad.Staged where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Data.Default
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Ledger as Pl
import qualified PlutusTx as Pl (FromData)
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

type InterpMockChain = MockChainT (WriterT TraceDescr [])

-- | The 'interpret' function gives semantics to our traces. One 'StagedMockChain'
--  computation yields a potential list of 'MockChainT' computations, which emmit
--  a description of their operation. Recall a 'MockChainT' is a state and except
--  monad composed:
--
--  >     MockChainT (WriterT TraceDescr []) a
--  > =~= st -> (WriterT TraceDescr []) (Either err (a, st))
--  > =~= st -> [(Either err (a, st) , TraceDescr)]
interpret :: StagedMockChain a -> InterpMockChain a
interpret = mapMockChainT (mapWriterT $ flip evalStateT []) . interpretNonDet

-- | This is an initial encoding of the MockChain operations, it provides
--  a simple way of altering the AST of a trace before actually executing it.
--  On top of the operations from 'MonadBlockChain' we also have 'Fail' to make
--  sure the resulting monad will be an instance of 'MonadFail'.
data MockChainOp a where
  ValidateTxSkel :: TxSkel -> MockChainOp Pl.TxId
  TxOutByRef :: Pl.TxOutRef -> MockChainOp (Maybe Pl.TxOut)
  GetCurrentSlot :: MockChainOp Pl.Slot
  AwaitSlot :: Pl.Slot -> MockChainOp Pl.Slot
  GetCurrentTime :: MockChainOp Pl.POSIXTime
  AwaitTime :: Pl.POSIXTime -> MockChainOp Pl.POSIXTime
  UtxosSuchThat ::
    (Pl.FromData a) =>
    Pl.Address ->
    (Maybe a -> Pl.Value -> Bool) ->
    MockChainOp [(SpendableOut, Maybe a)]
  Fail :: String -> MockChainOp a
  OwnPubKey :: MockChainOp Pl.PubKeyHash
  --
  SigningWith :: NE.NonEmpty Wallet -> StagedMockChain a -> MockChainOp a
  AskSigners :: MockChainOp (NE.NonEmpty Wallet)
  --
  Modify :: Modality TxSkel -> StagedMockChain a -> MockChainOp a

data Staged (op :: * -> *) :: * -> * where
  Return :: a -> Staged op a
  Instr :: op a -> (a -> Staged op b) -> Staged op b

type StagedMockChain = Staged MockChainOp

singleton :: MockChainOp a -> StagedMockChain a
singleton op = Instr op Return

instance Functor StagedMockChain where
  fmap f (Return x) = Return $ f x
  fmap f (Instr op cont) = Instr op (fmap f . cont)

instance Applicative StagedMockChain where
  pure = Return
  (<*>) = ap

instance Monad StagedMockChain where
  (Return x) >>= f = f x
  (Instr i m) >>= f = Instr i (m >=> f)

-- | Interprets a single operation in the direct 'MockChainT' monad. We need to
-- pass around the modalities since some operations refer back to 'StagedMockChain',
-- and that needs to be interpreted taking into account the existing modalities.
interpretOp :: MockChainOp a -> InterpMockChainMod a
interpretOp (ValidateTxSkel skel) = validateTxSkel skel
interpretOp (TxOutByRef ref) = txOutByRef ref
interpretOp GetCurrentSlot = currentSlot
interpretOp GetCurrentTime = currentTime
interpretOp (AwaitSlot s) = awaitSlot s
interpretOp (AwaitTime t) = awaitTime t
interpretOp (UtxosSuchThat addr predi) = utxosSuchThat addr predi
interpretOp (Fail str) = fail str
interpretOp OwnPubKey = ownPaymentPubKeyHash
interpretOp AskSigners = askSigners
interpretOp (SigningWith ws act) = signingWith ws (interpretNonDet act)
interpretOp (Modify m' block) = lift (modify (++ [m'])) >> interpretNonDet block

-- | This is an internal type that keeps track of the list of modalities that
-- are present in each branch, it's isomorphic to:
--
--  > St -> ListMods -> [((Either Err (a, St) , TraceDescr), ListMods)]
type InterpMockChainMod = MockChainT (WriterT TraceDescr (StateT [Modality TxSkel] []))

-- | Interprets a 'StagedMockChain' that must be modified with a certain list of modalities.
interpretNonDet :: forall a. StagedMockChain a -> InterpMockChainMod a
interpretNonDet (Return a) = do
  ms <- lift get
  -- When returning, if we are returning from a point where a /Somewhere/ modality is yet to be consumed,
  -- return empty. If we return a, it would correspond to a trace where the modality was never applied.
  if any isSomewhere ms then empty else return a
-- When interpreting a 'ValidateTxSkel', we evaluate the modalities and return a union
-- of all possible outcomes.
interpretNonDet (Instr (ValidateTxSkel skel) f) = do
  ms <- lift get
  asum $ map (uncurry interpAux) $ interpModalities ms skel
  where
    interpAux :: TxSkel -> [Modality TxSkel] -> InterpMockChainMod a
    interpAux skel' ms' = lift (put ms') >> interpAndTellOp (ValidateTxSkel skel') >>= interpretNonDet . f
-- Interpreting any other instruction is just a matter of interpreting a bind
interpretNonDet (Instr op f) = interpAndTellOp op >>= interpretNonDet . f

-- | Auxiliar function to interpret a bind.
interpAndTellOp :: MockChainOp a -> InterpMockChainMod a
interpAndTellOp op = do
  signers <- askSigners
  lift (tell $ prettyMockChainOp signers op)
  interpretOp op

-- | Interprets and runs the mockchain computation from a given initial state.
interpretAndRunRaw ::
  StagedMockChain a ->
  MockChainEnv ->
  MockChainSt ->
  [(Either MockChainError (a, UtxoState), TraceDescr)]
interpretAndRunRaw smc e0 st0 = runWriterT $ runMockChainTRaw e0 st0 (interpret smc)

interpretAndRun ::
  StagedMockChain a ->
  [(Either MockChainError (a, UtxoState), TraceDescr)]
interpretAndRun smc = interpretAndRunRaw smc def def

-- * MonadBlockChain Instance

instance MonadFail StagedMockChain where
  fail = singleton . Fail

instance MonadBlockChain StagedMockChain where
  validateTxSkel = singleton . ValidateTxSkel
  utxosSuchThat addr = singleton . UtxosSuchThat addr
  txOutByRef = singleton . TxOutByRef
  ownPaymentPubKeyHash = singleton OwnPubKey
  currentSlot = singleton GetCurrentSlot
  currentTime = singleton GetCurrentTime
  awaitSlot = singleton . AwaitSlot
  awaitTime = singleton . AwaitTime

instance MonadMockChain StagedMockChain where
  signingWith ws = singleton . SigningWith ws
  askSigners = singleton AskSigners

instance MonadModal StagedMockChain where
  type Action StagedMockChain = TxSkel
  somewhere m tree = singleton (Modify (Somewhere m) tree)
  everywhere m tree = singleton (Modify (Everywhere m) tree)

-- * Modalities

-- | Modalities apply a function to a trace;
data Modality a = Somewhere (a -> Maybe a) | Everywhere (a -> Maybe a)

isSomewhere :: Modality a -> Bool
isSomewhere (Somewhere _) = True
isSomewhere _ = False

-- | Performs one step of interpreting a composition of modalities to an input. For example,
--
--  > interpModalities [Somewhere h, Everywhere g, Somewhere f] x
--
--  Returns:
--
--  > [ ( h (g (f x)) , [Everywhere g] )
--  > , ( h (g    x)) , [Everywhere g, Somewhere f] )
--  > , (   (g (f x)) , [Somewhere h, Everywhere g] )
--  > , (   (g    x)) , [Somewhere h, Everywhere g, Somewhere f])
--
--  Where each element of the list corresponds to a universe where x suffered the effect
--  of a modality, and which modalities to consider when continuing to evaluate such universe.
interpModalities :: [Modality a] -> a -> [(a, [Modality a])]
interpModalities [] s = [(s, [])]
interpModalities (Everywhere f : ms) s = concatMap here $ interpModalities ms s
  where
    here (hs, mods)
      | Just fhs <- f hs = [(fhs, Everywhere f : mods)]
      | otherwise = [(s, Everywhere f : mods)]
interpModalities (Somewhere f : ms) s = concatMap hereOrThere $ interpModalities ms s
  where
    hereOrThere (hs, mods)
      | Just fhs <- f hs = [(fhs, mods), (hs, Somewhere f : mods)]
      | otherwise = [(hs, Somewhere f : mods)]

-- * Human Readable Traces

-- | Generates a 'TraceDescr'iption for the given operation; we're mostly interested in seeing
--  the transactions that were validated, so many operations have no description.
prettyMockChainOp :: NE.NonEmpty Wallet -> MockChainOp a -> TraceDescr
prettyMockChainOp signers (ValidateTxSkel skel) =
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
