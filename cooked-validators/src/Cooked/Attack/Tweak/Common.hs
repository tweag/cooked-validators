{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | This module defines 'Tweaks' which are the fundamental building blocks of
-- our "domain specific language" for attacks.
module Cooked.Attack.Tweak.Common where

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

-- * The type of tweaks

-- | A tweak is a function that, depending on the 'MockChainSt'ate, looks at a
-- transaction and computes a list of modified transactions, together with some
-- additional values.
--
-- Our intuition (and also the language of the comments pertaining to tweaks) is
-- that a tweak
--
-- - /fails/ if it returns @[]@
--
-- - /modifies a transaction/, where the /unmodified transaction/ is the name we
--   give to the input 'TxSkel', and each of the 'TxSkel's in the output list is
--   a /modified transaction/
--
-- - /returns/ the value(s) in the 'snd' component of the pairs in the output
--   list. This is reflected by the 'Monad' instance for 'Tweak's.
newtype Tweak a = Tweak {getTweak :: MockChainSt -> TxSkel -> [(TxSkel, a)]}

-- | Internal wrapper type for compatibility with the LTL modalities. You'll
-- probably never work with this type if you want to build and use tweaks or
-- peeks.
data UntypedTweak where
  UntypedTweak :: Tweak a -> UntypedTweak

instance Functor Tweak where
  fmap f g = Tweak $ \mcst skel -> second f <$> getTweak g mcst skel

instance Applicative Tweak where
  pure x = Tweak $ \_ skel -> [(skel, x)]
  (<*>) = ap

instance Monad Tweak where
  Tweak g >>= h = Tweak $ \mcst skel ->
    concatMap (\(skel', x) -> getTweak (h x) mcst skel') $ g mcst skel

instance Alternative Tweak where
  empty = Tweak $ \_ _ -> []
  Tweak f <|> Tweak g = Tweak $ \mcst skel -> f mcst skel ++ g mcst skel

instance MonadPlus Tweak

instance MonadFail Tweak where
  fail _ = empty

-- * Simple tweaks

-- | The never-applicable tweak.
failingTweak :: Tweak a
failingTweak = empty

-- | The tweak that always applies and leaves the transaction unchanged.
doNothingTweak :: Tweak ()
doNothingTweak = return ()

-- | The "tweak" that returns the current 'MockChainSt'. This does *not* modify
-- the transaction.
mcstTweak :: Tweak MockChainSt
mcstTweak = Tweak $ \mcst skel -> [(skel, mcst)]

-- | The "tweak" that obtains some value from the 'TxSkel'. This does *not*
-- modify the transaction.
viewTweak :: Is k A_Getter => Optic' k is TxSkel a -> Tweak a
viewTweak optic = Tweak $ \_mcst skel -> [(skel, view optic skel)]

-- | The tweak that sets a certain value in the 'TxSkel'.
setTweak :: Is k A_Setter => Optic' k is TxSkel a -> a -> Tweak ()
setTweak optic newValue = Tweak $ \_mcst skel -> [(set optic newValue skel, ())]

-- | The tweak that modifies a certain value in the 'TxSkel'.
overTweak :: Is k A_Setter => Optic' k is TxSkel a -> (a -> a) -> Tweak ()
overTweak optic change = Tweak $ \_mcst skel -> [(over optic change skel, ())]

-- * Composing tweaks

-- | Turn a potentially failing tweak into an always successful tweak, that
-- just leaves the 'TxSkel' unmodified if the original tweak would have failed.
--
-- In cases where the original tweak would have failed, this returns
-- @Nothing@. If the original tweak would have been applicable, this is
-- signalled by wrapping the original tweak's return value in a @Just@.
tryTweak :: Tweak a -> Tweak (Maybe a)
tryTweak (Tweak f) = Tweak $
  \mcst skel -> case f mcst skel of
    [] -> [(skel, Nothing)]
    l -> second Just <$> l

-- | This "tweak" returns the transaction as it has been modified by now. Use
-- this as a kind of savepoint, if you want execute some sequence of tweaks of
-- which you're not sure that they will lead to the right result. You can
-- restore the savepoint after that sequence with 'setTxSkel'.
getTxSkel :: Tweak TxSkel
getTxSkel = Tweak $ \_mcst skel -> [(skel, skel)]

-- | See the comment at 'getTxSkel'.
setTxSkel :: TxSkel -> Tweak ()
setTxSkel skel = Tweak $ \_ _ -> [(skel, ())]

-- * Applying 'Tweak's to 'Constraints'

-- | Most tweaks do something interesting only to the 'Constraints' of a
-- transaction. This function extracts that action on 'Constraints' from an
-- tweak.
applyToConstraints :: Tweak a -> MockChainSt -> Constraints -> [Constraints]
applyToConstraints tweak mcst cs =
  txConstraints . fst <$> getTweak tweak mcst (txSkel cs)

-- * Constructing Tweaks from Optics

-- | Probably the most common way to make an tweak from an optic: Try to apply
-- a given function of type @MockChainSt -> a -> Maybe a@ to all foci of an
-- optic, modifying all foci that return @Just ...@.
--
-- If at least one focus was modified, this tweak returns a list of the foci
-- that were modified, in the order in which they occured on the original
-- transaction (and with no modification applied to them).
--
-- If no focus was modified, or if nothing was in focus, the tweak fails.
mkTweak ::
  (Is k A_Traversal) =>
  -- | Optic focussing potentially interesting points to modify.
  Optic' k is TxSkel a ->
  -- | The modification to apply; return @Nothing@ if you want to leave the
  -- given focus as it is.
  (MockChainSt -> a -> Maybe a) ->
  Tweak [a]
mkTweak optic change = do
  unmodified <-
    mkAccumLTweak
      optic
      ( \mcst acc oldFocus -> case change mcst oldFocus of
          Just newFocus -> (newFocus, oldFocus : acc)
          Nothing -> (oldFocus, acc)
      )
      []
  guard $ not $ null unmodified
  return $ reverse unmodified

-- | Sometimes 'mkTweak' modifies too many foci. For example, there might be
-- many identical outputs on a transaction but you only want to modify a few of
-- them.
--
-- This is the problem solved by this tweak. It traverses all foci of the given
-- optic from the left to the right, while counting the foci to which the
-- modification successfully applies, but modifies the i-th modifiable focus
-- only if i satisfies a given predicate.
--
-- The return value and failure behaviour of this tweak is similar to the one
-- of 'mkTweak': It returns a list of the modified foci, as they were before
-- the modification, and fails if nothing was modified.
mkSelectTweak ::
  (Is k A_Traversal) =>
  -- | Optic focussing potentially interesting points to modify.
  Optic' k is TxSkel a ->
  -- | The modification to apply; return @Nothing@ if you want to leave the
  -- given focus as it is.
  (MockChainSt -> a -> Maybe a) ->
  -- | Maybe more than one of the foci is modifiable (i.e. the modification
  -- returns @Just@). Use this predicate to selectively apply the modification
  -- only to some of the modifiable foci: This tweak counts the modifiable foci
  -- in the order of the traversal, starting with 0, and only applies the
  -- modification to the @i@-th modifiable focus if @i@ satisfies the predicate.
  (Integer -> Bool) ->
  Tweak [a]
mkSelectTweak optic change select = do
  (_, unmodified) <-
    mkAccumLTweak
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

-- | A very simple, but flexible way to build an tweak from an optic: Traverse
-- all foci of the optic from the left to the right, collecting an accumulator
-- while also modifying the foci.
--
-- This tweak never fails, and returns the accumulator, even if no
-- modifications were made to the 'TxSkel'. So, it's up to the caller of this
-- function to look at the accumulator and decide how to proceed.
mkAccumLTweak ::
  Is k A_Traversal =>
  -- | Optic focussing potentially interesting points to modify
  Optic' k is TxSkel a ->
  -- | function that describes the modification of the accumulator and the
  -- current focus.
  (MockChainSt -> acc -> a -> (a, acc)) ->
  -- | Initial accumulator.
  acc ->
  Tweak acc
mkAccumLTweak optic f initAcc = Tweak $ \mcst skel ->
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

-- * Some more simple tweaks

-- | Change some 'Value's in a 'TxSkel'. Returns a list of the increments by
-- which all of the focused values were changed.
--
-- This tweak never fails.
changeValueTweak ::
  Is k A_Traversal =>
  Optic' k is TxSkel L.Value ->
  -- | A function describing how a focused value should change. Return the
  -- modified value.
  (L.Value -> L.Value) ->
  Tweak [L.Value]
changeValueTweak optic change =
  mkAccumLTweak
    optic
    ( \_mcst acc oldValue ->
        let newValue = change oldValue
         in (newValue, acc ++ [newValue <> Pl.negate oldValue])
    )
    []

-- | Add a label to a 'TxSkel'. If there is already a pre-existing label, the
-- given label will be added, forming a pair @(newlabel, oldlabel)@.
addLabelTweak :: LabelConstrs x => x -> Tweak ()
addLabelTweak newlabel =
  overTweak
    txLabelL
    ( \case
        TxLabel Nothing -> TxLabel $ Just newlabel
        TxLabel (Just oldlabel) -> TxLabel $ Just (newlabel, oldlabel)
    )
