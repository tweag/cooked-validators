{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines 'Tweaks' which are the fundamental building blocks of
-- our "domain specific language" for attacks.
module Cooked.Attack.Tweak.Common where

import Control.Monad
import Cooked.MockChain.Monad
import Cooked.Tx.Constraints.Type
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import ListT (ListT)
import qualified ListT
import Optics.Core

-- * The type of tweaks

class (MonadPlus m, MonadBlockChainWithoutValidation m) => MonadTweak m where
  getTxSkel :: m TxSkel
  putTxSkel :: TxSkel -> m ()

type Tweak m = StateT TxSkel (ListT m)

instance MonadBlockChainWithoutValidation m => MonadTweak (Tweak m) where
  getTxSkel = get
  putTxSkel = put

-- | This is the function that gives a meaning to 'Tweak's: A 'Tweak' is a
-- computation that, depending on the state of the chain, looks at a transaction
-- and returns zero or more modified transactions, together with some additional
-- values.
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
-- - /modifies/ a 'TxSkel'. Since it can use every method of
--   'MonadBlockChainWithoutValidateTxSkel' to do so, this also includes
--   stateful lookups or even things like waiting for a certain amount of time
--   before submitting the transaction.
runTweakInChain :: (MonadBlockChainWithoutValidation m, MonadPlus m) => Tweak m a -> TxSkel -> m (a, TxSkel)
runTweakInChain tweak skel = ListT.alternate $ runStateT tweak skel

-- | Internal wrapper type for compatibility with the LTL modalities. You'll
-- probably never work with this type if you want to build and use tweaks.
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

-- * A few fundamental tweaks

-- | The never-applicable tweak.
failingTweak :: Tweak a
failingTweak = empty

-- | The tweak that always applies and leaves the transaction unchanged.
doNothingTweak :: Tweak ()
doNothingTweak = return ()

-- * Constructing Tweaks from Optics

-- | The "tweak" that obtains some value from the 'TxSkel'. This does *not*
-- modify the transaction.
viewTweak :: (MonadTweak m, Is k A_Getter) => Optic' k is TxSkel a -> m a
viewTweak optic = getTxSkel <&> view optic

-- | Like the 'viewTweak', but returns a list of all foci
viewAllTweak :: (MonadTweak m, Is k A_Fold) => Optic' k is TxSkel a -> m [a]
viewAllTweak optic = getTxSkel <&> toListOf optic

-- | The tweak that sets a certain value in the 'TxSkel'.
setTweak :: (MonadTweak m, Is k A_Setter) => Optic' k is TxSkel a -> a -> m ()
setTweak optic newValue = getTxSkel >>= putTxSkel . set optic newValue

-- | The tweak that modifies a certain value in the 'TxSkel'.
overTweak :: (MonadTweak m, Is k A_Setter) => Optic' k is TxSkel a -> (a -> a) -> m ()
overTweak optic change = getTxSkel >>= putTxSkel . over optic change

-- | Like 'overTweak', but only modifies foci on which the argument function
-- returns @Just@ the new focus. Returns a list of the foci that were modified,
-- as they were /before/ the tweak, and in the order in which they occurred on
-- the original transaction.
overMaybeTweak :: (MonadTweak m, Is k A_Traversal) => Optic' k is TxSkel a -> (a -> Maybe a) -> m [a]
overMaybeTweak optic mChange = overMaybeTweakSelecting optic mChange (const True)

-- | Sometimes 'overMaybeTweak' modifies too many foci. This might be the case
-- if there are several identical foci, but you only want to modify some of
-- them. This is where this 'Tweak' becomes useful: The @(Integer -> Bool)@
-- argument can be used to select which of the modifiable foci should be
-- actually modified.
overMaybeTweakSelecting ::
  forall a m k is.
  (MonadTweak m, Is k A_Traversal) =>
  Optic' k is TxSkel a ->
  (a -> Maybe a) ->
  (Integer -> Bool) ->
  m [a]
overMaybeTweakSelecting optic mChange select = do
  allFoci <- viewTweak $ partsOf optic
  let evaluatedFoci :: [(a, Maybe a)]
      evaluatedFoci =
        snd $
          mapAccumL
            ( \i unmodifiedFocus ->
                case mChange unmodifiedFocus of
                  Just modifiedFocus ->
                    if select i
                      then (i + 1, (unmodifiedFocus, Just modifiedFocus))
                      else (i + 1, (unmodifiedFocus, Nothing))
                  Nothing -> (i, (unmodifiedFocus, Nothing))
            )
            0
            allFoci
  setTweak (partsOf optic) $ map (uncurry fromMaybe) evaluatedFoci -- If the second  component of the pair is @Just@, use it.
  return $
    mapMaybe
      (\(original, mNew) -> if isJust mNew then Just original else Nothing)
      evaluatedFoci

-- * Some more simple tweaks

-- | Add a label to a 'TxSkel'. If there is already a pre-existing label, the
-- given label will be added, forming a pair @(newlabel, oldlabel)@.
addLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m ()
addLabelTweak newlabel = overTweak txSkelLabelL (Set.insert $ TxLabel newlabel)
