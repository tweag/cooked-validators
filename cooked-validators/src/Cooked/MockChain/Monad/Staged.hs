{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.MockChain.Monad.Staged where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Writer
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Data.Default
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Ledger as Pl
import qualified PlutusTx as Pl (FromData)
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

-- | This is an initial encoding of the MockChain operations, it provides
--  a simple way of altering the AST of a trace before actually executing it.
--  On top of the operations from 'MonadBlockChain' we also have 'Fail' to make
--  sure the resulting monad will be an instance of 'MonadFail'.
data MockChainOp a where
  ValidateTxSkel :: ValidateTxOpts -> TxSkel -> MockChainOp Pl.TxId
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

data StagedMockChain a where
  Return :: a -> StagedMockChain a
  Instr :: MockChainOp a -> (a -> StagedMockChain b) -> StagedMockChain b
  -- Modify is implemented as a separate constructor into `StagedMockChain` to simplify
  -- the definition of `interpretOp`; otherwise, we'd need to pass the set of modalities
  -- one is interpreting into `interpretOp`.
  Modify :: Modality TxSkel -> StagedMockChain a -> (a -> StagedMockChain b) -> StagedMockChain b

singleton :: MockChainOp a -> StagedMockChain a
singleton op = Instr op Return

instance Functor StagedMockChain where
  fmap f (Return x) = Return $ f x
  fmap f (Instr op cont) = Instr op (fmap f . cont)
  fmap f (Modify h m cont) = Modify h m (fmap f . cont)

instance Applicative StagedMockChain where
  pure = Return
  (<*>) = ap

instance Monad StagedMockChain where
  (Return x) >>= f = f x
  (Instr i m) >>= f = Instr i (m >=> f)
  (Modify h tree cont) >>= f = Modify h tree (cont >=> f)

-- I) return x >>= f ~ f x
--
-- Holds definitionally
--
-- II) m >>= return ~ m
--
-- Induction on m; case m of
--   Return x -> Rturn x >>= return ~ return x ~ Return x
--   Instr i m -> Instr i m >>= return ~ Instr i (m >=> return) ~ Instr i m
--   Modify h t c -> Modify h t c >>= return ~ Modify h t (c >=> return) ~ Modify h t c
--
--
-- III) (h >>= g) >>= j ~ h >>= (g >=> j)
--
-- Induction on h; case h of
--   Return x -> (Return x >>= g) >>= j
--             ~ g x >>= j
--             ~ (g >=> j) x
--             ~ Return x >>= (g >=> j)
--
--   Instr i m -> (Instr i m >>= h) >>= j
--             ~  Instr i (m >=> h) >>= j
--             ~  Instr i ((m >=> h) >=> j)
--             ~  Instr i (m >=> (h >=> j))
--             ~  Instr i m >>= (h >=> j)
--
--   Modify h t c -> (Modify h t c >>= h) >>= j
--                 ~ Modify h t (c >=> h) >>= j
--                 ~ Modify h t ((c >=> h) >>= j)
--                 ~ Modify h t (c >=> (h >=> j))
--                 ~ Modify h t c >>= (h >=> j)
--
-- On our particular case, it must be that (Modify (Everwhere f) Return c ~ c),
-- and (Modify (Somewhere f) Return c ~ empty) following the intuition of modal logics
-- that diamond (ie., Somewhere) implies some sort of progress.

-- | Interprets a single operation in the direct 'MockChainT' monad.
interpretOp :: MockChainOp a -> MockChainT (WriterT TraceDescr []) a
interpretOp (ValidateTxSkel opts skel) = validateTxSkelOpts opts skel
interpretOp (TxOutByRef ref) = txOutByRef ref
interpretOp GetCurrentSlot = currentSlot
interpretOp GetCurrentTime = currentTime
interpretOp (AwaitSlot s) = awaitSlot s
interpretOp (AwaitTime t) = awaitTime t
interpretOp (UtxosSuchThat addr predi) = utxosSuchThat addr predi
interpretOp (Fail str) = fail str
interpretOp OwnPubKey = ownPaymentPubKeyHash
interpretOp AskSigners = askSigners
interpretOp (SigningWith ws act) = signingWith ws (interpret act)

instance MonadFail StagedMockChain where
  fail = singleton . Fail

instance MonadBlockChain StagedMockChain where
  validateTxSkelOpts opts = singleton . ValidateTxSkel opts
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
  somewhere m tree = Modify (Somewhere m) tree Return
  everywhere m tree = Modify (Everywhere m) tree Return

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
interpret = goDet
  where
    interpBind :: MockChainOp a -> (a -> InterpMockChain b) -> InterpMockChain b
    interpBind op f = do
      signers <- askSigners
      lift (tell $ prettyMockChainOp signers op)
      a <- interpretOp op
      f a

    goDet :: StagedMockChain a -> InterpMockChain a
    goDet (Return a) = return a
    goDet (Instr op f) = interpBind op (goDet . f)
    goDet (Modify c block cont) = goMod [c] block >>= goDet . cont

    -- Interprets a staged MockChain with modalities over the transactions
    -- that are meant to be submitted.
    goMod :: [Modality TxSkel] -> StagedMockChain a -> InterpMockChain a
    -- When returning, if we are returning from a point where a /Somewhere/ modality is yet to be consumed,
    -- return empty. If we return a, it would correspond to a trace where the modality was never applied.
    goMod ms (Return a)
      | any isSomewhere ms = empty
      | otherwise = return a
    -- When interpreting a new modality, we just compose them by pushing it into the stack
    goMod ms (Modify m block cont) = goMod (m : ms) block >>= goMod ms . cont
    -- Finally, when interpreting a instruction, we evaluate the modalities
    goMod ms (Instr (ValidateTxSkel opts skel) f) =
      asum $ map (uncurry (validateThenGoMod opts f)) $ interpModalities ms skel
    goMod ms (Instr op f) = interpBind op (goMod ms . f)

    validateThenGoMod :: ValidateTxOpts -> (Pl.TxId -> StagedMockChain a) -> TxSkel -> [Modality TxSkel] -> InterpMockChain a
    validateThenGoMod opts f skel ms = interpBind (ValidateTxSkel opts skel) (goMod ms . f)

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
      | otherwise = []
interpModalities (Somewhere f : ms) s = concatMap hereOrThere $ interpModalities ms s
  where
    hereOrThere (hs, mods)
      | Just fhs <- f hs = [(fhs, mods), (hs, Somewhere f : mods)]
      | otherwise = [(hs, Somewhere f : mods)]

-- * Human Readable Traces

-- | Generates a 'TraceDescr'iption for the given operation; we're mostly interested in seeing
--  the transactions that were validated, so many operations have no description.
prettyMockChainOp :: NE.NonEmpty Wallet -> MockChainOp a -> TraceDescr
prettyMockChainOp signers (ValidateTxSkel opts skel) =
  trSingleton $
    PP.hang 2 $
      PP.vsep $
        catMaybes [Just "ValidateTxSkel", mopts, Just $ prettyTxSkel (NE.toList signers) skel]
  where
    mopts = if opts == def then Nothing else Just ("Opts:" <+> PP.viaShow opts)
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
