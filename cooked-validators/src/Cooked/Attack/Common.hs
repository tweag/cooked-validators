{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.Attack.Common where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoPredicate
import Cooked.Tx.Constraints
import Data.Default
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl

-- * The type of attacks

-- | An attack is a function that, depending on the 'MockChainSt'ate, looks at a
-- transaction and computes a list of modified transactions, together with some
-- additional values.
--
-- Our intuition (and also the language of the comments in this module and its
-- submodules) is that an attack
--
-- - /fails/ if it returns @[]@
--
-- - /modifies a transaction/, where the /unmodified transaction/ is the name we
--   give to the input 'TxSkel', and each of the 'TxSkel's in the output list is
--   a /modified transaction/
--
-- - /returns/ the value(s) in the 'snd' component of the pairs in the output
--   list. This is reflected by the 'Monad' instance for 'Attack's.
newtype Attack a = Attack {getAttack :: MockChainSt -> TxSkel -> [(TxSkel, a)]}

-- | Internal wrapper type for compatibility with the LTL modalities. You'll
-- probably never work with this type if you want to build and use attacks.
data UntypedAttack where
  UntypedAttack :: Attack a -> UntypedAttack

instance Functor Attack where
  fmap f (Attack g) = Attack $ \mcst skel -> second f <$> g mcst skel

instance Applicative Attack where
  pure x = Attack $ \_ skel -> [(skel, x)]
  (<*>) = ap

instance Monad Attack where
  Attack g >>= h = Attack $ \mcst skel ->
    concatMap (\(skel', x) -> getAttack (h x) mcst skel') $ g mcst skel

instance Alternative Attack where
  empty = Attack $ \_ _ -> []
  Attack f <|> Attack g = Attack $ \mcst skel -> f mcst skel ++ g mcst skel

instance MonadPlus Attack

-- * Simple attacks

-- | The never-applicable attack.
failingAttack :: Attack a
failingAttack = empty

-- | The attack that always applies and leaves the transaction unchanged.
doNothingAttack :: Attack ()
doNothingAttack = return ()

-- | The "attack" that returns the current 'MockChainSt'
mcstAttack :: Attack MockChainSt
mcstAttack = Attack $ \mcst skel -> [(skel, mcst)]

-- | The "attack" that obtains some value from the 'TxSkel'.
viewAttack :: Is k A_Getter => Optic' k is TxSkel a -> Attack a
viewAttack optic = Attack $ \_mcst skel -> [(skel, view optic skel)]

-- | The attack that sets a certain value in the 'TxSkel'.
setAttack :: Is k A_Setter => Optic' k is TxSkel a -> a -> Attack ()
setAttack optic newValue = Attack $ \_mcst skel -> [(set optic newValue skel, ())]

-- | The atack that modifies a certain value in the 'TxSkel'.
overAttack :: Is k A_Setter => Optic' k is TxSkel a -> (a -> a) -> Attack ()
overAttack optic change = Attack $ \_mcst skel -> [(over optic change skel, ())]

-- * Composing attacks

-- | Turn a potentially failing attack into an always successful attack, that
-- just leaves the 'TxSkel' unmodified if the original attack would have failed.
--
-- In cases where the original attack would have failed, this returns
-- @Nothing@. If the original attack would have been applicable, this is
-- signalled by wrapping the original attack's return value in a @Just@.
tryAttack :: Attack a -> Attack (Maybe a)
tryAttack (Attack f) = Attack $
  \mcst skel -> case f mcst skel of
    [] -> [(skel, Nothing)]
    l -> second Just <$> l

-- * Constructing Attacks from Optics

-- | Probably the most common way to make an attack from an optic: Try to apply
-- a given function of type @MockChainSt -> a -> Maybe a@ to all foci of an
-- optic, modifying all foci that return @Just ...@.
--
-- If at least one focus was modified, this attack returns a list of the foci
-- that were modified, in the order in which they occured on the original
-- transaction (and with no modification applied to them).
--
-- If no focus was modified, or if nothing was in focus, the attack fails.
mkAttack ::
  (Is k A_Traversal) =>
  -- | Optic focussing potentially interesting points to modify.
  Optic' k is TxSkel a ->
  -- | The modification to apply; return @Nothing@ if you want to leave the
  -- given focus as it is.
  (MockChainSt -> a -> Maybe a) ->
  Attack [a]
mkAttack optic change = do
  unmodified <-
    mkAccumLAttack
      optic
      ( \mcst acc oldFocus -> case change mcst oldFocus of
          Just newFocus -> (newFocus, oldFocus : acc)
          Nothing -> (oldFocus, acc)
      )
      []
  guard $ not $ null unmodified
  return $ reverse unmodified

-- | Sometimes 'mkAttack' modifies too many foci. For example, there might be
-- many identical outputs on a transaction but you only want to modify a few of
-- them.
--
-- This is the problem solved by this attack. It traverses all foci of the given
-- optic from the left to the right, while counting the foci to which the
-- modification successfully applies, but modifies the i-th modifiable focus
-- only if i satisfies a given predicate.
--
-- The return value and failure behaviour of this attack is similar to the one
-- of 'mkAttack': It returns a list of the modified foci, as they were before
-- the modification, and fails if nothing was modified.
mkSelectAttack ::
  (Is k A_Traversal) =>
  -- | Optic focussing potentially interesting points to modify.
  Optic' k is TxSkel a ->
  -- | The modification to apply; return @Nothing@ if you want to leave the
  -- given focus as it is.
  (MockChainSt -> a -> Maybe a) ->
  -- | Maybe more than one of the foci is modifiable (i.e. the modification
  -- returns @Just@). Use this predicate to selectively apply the modification
  -- only to some of the modifiable foci: This attack counts the modifiable foci
  -- in the order of the traversal, starting with 0, and only applies the
  -- modification to the @i@-th modifiable focus if @i@ satisfies the predicate.
  (Integer -> Bool) ->
  Attack [a]
mkSelectAttack optic change select = do
  (_, unmodified) <-
    mkAccumLAttack
      optic
      ( \mcst (index, acc) oldFocus ->
          case change mcst oldFocus of
            Just newFocus ->
              if select index
                then (newFocus, (index + 1, oldFocus : acc))
                else (oldFocus, (index + 1, acc))
            Nothing -> (oldFocus, (index, acc))
      )
      (0, [])
  guard $ not $ null unmodified
  return $ reverse unmodified

-- | A very simple, but flexible way to build an attack from an optic: Traverse
-- all foci of the optic from the left to the right, collecting an accumulator
-- while also modifying the foci.
--
-- This attack never fails, and returns the accumulator, even if no
-- modifications were made to the 'TxSkel'. So, it's up to the caller of this
-- function to look at the accumulator and decide how to proceed.
mkAccumLAttack ::
  Is k A_Traversal =>
  -- | Optic focussing potentially interesting points to modify
  Optic' k is TxSkel a ->
  -- | function that describes the modification of the accumulator and the
  -- current focus.
  (MockChainSt -> acc -> a -> (a, acc)) ->
  -- | Initial accumulator.
  acc ->
  Attack acc
mkAccumLAttack optic f initAcc = Attack $ \mcst skel ->
  [mapAccumLOf optic (f mcst) initAcc skel]

-- * Helpers to interact with 'MockChainSt'

-- | Like 'utxosSuchThat', but with the 'MockChainSt'ate as an explicit argument
utxosSuchThatMcst ::
  (Pl.FromData a) =>
  MockChainSt ->
  L.Address ->
  UtxoPredicate a ->
  [(SpendableOut, Maybe a)]
utxosSuchThatMcst mcst addr select =
  case runMockChainRaw def mcst (utxosSuchThat addr select) of
    Left _ -> []
    Right (utxos, _) -> utxos

-- | Like 'scriptUtxosSuchThat', but with the 'MockChainSt'ate as an explicit
-- argument
scriptUtxosSuchThatMcst ::
  (Pl.FromData (L.DatumType a)) =>
  MockChainSt ->
  L.TypedValidator a ->
  (L.DatumType a -> L.Value -> Bool) ->
  [(SpendableOut, L.DatumType a)]
scriptUtxosSuchThatMcst mcst val select =
  map (second fromJust) $
    utxosSuchThatMcst
      mcst
      (L.validatorAddress val)
      (maybe (const False) select)

-- * General helpers

-- | Add a label to a 'TxSkel'. If there is already a pre-existing label, the
-- given label will be added, forming a pair @(newlabel, oldlabel)@.
addLabel :: LabelConstrs x => x -> TxSkel -> TxSkel
addLabel newlabel =
  over
    txLabelL
    ( \case
        TxLabel Nothing -> TxLabel $ Just newlabel
        TxLabel (Just oldlabel) -> TxLabel $ Just (newlabel, oldlabel)
    )

-- * Some more simple attacks

-- | Change some 'Value's in a 'TxSkel'. Returns a list of the increments by
-- which all of the focused values were changed.
--
-- This attack never fails.
changeValueAttack ::
  Is k A_Traversal =>
  Optic' k is TxSkel L.Value ->
  -- | A function describing how a focused value should change. Return the
  -- modified value.
  (L.Value -> L.Value) ->
  Attack [L.Value]
changeValueAttack optic change =
  mkAccumLAttack
    optic
    ( \_mcst acc oldValue ->
        let newValue = change oldValue
         in (newValue, acc ++ [newValue <> Pl.negate oldValue])
    )
    []

-- | Add a label to a 'TxSkel'. If there is already a pre-existing label, the
-- given label will be added, forming a pair @(newlabel, oldlabel)@.
addLabelAttack :: LabelConstrs x => x -> Attack ()
addLabelAttack newlabel = Attack $ \_mcst skel ->
  [ ( over
        txLabelL
        ( \case
            TxLabel Nothing -> TxLabel $ Just newlabel
            TxLabel (Just oldlabel) -> TxLabel $ Just (newlabel, oldlabel)
        )
        skel,
      ()
    )
  ]
