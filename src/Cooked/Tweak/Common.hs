{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines 'Tweaks' which are the fundamental building blocks of
-- our "domain specific language" for attacks.
module Cooked.Tweak.Common
  ( runTweakInChain,
    runTweakInChain',
    Tweak,
    UntypedTweak (UntypedTweak),

    -- * User API
    MonadTweak (..),
    failingTweak,
    doNothingTweak,
    viewTweak,
    viewAllTweak,
    setTweak,
    overTweak,
    overMaybeTweak,
    overMaybeSelectingTweak,
    selectP,
  )
where

import Control.Monad
import Control.Monad.State
import Cooked.MockChain.BlockChain
import Cooked.Skeleton
import Data.List
import Data.Maybe
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
-- Our intuition (and also the language of the comments pertaining to 'Tweak's)
-- is that a 'Tweak' @t@
--
-- - /fails/ if @runTweakInChain t skel@ is @mzero@.
--
-- - /returns/ the value in the first component of the pair returned by this
--   function (which is also the value it returns in the monad @Tweak m@).
--
-- - /modifies/ a 'TxSkel'. Since it can use every method of
--   'MonadBlockChainWithoutValidateTxSkel' to do so, this also includes
--   stateful lookups or even things like waiting for a certain amount of time
--   before submitting the transaction.
--
-- If you're using tweaks in a 'MonadModalBlockChain' together with mechanisms
-- like 'withTweak', 'somewhere', or 'everywhere', you should never have areason
-- to use this function.
runTweakInChain :: (MonadBlockChainWithoutValidation m, MonadPlus m) => Tweak m a -> TxSkel -> m (a, TxSkel)
runTweakInChain tweak skel = ListT.alternate $ runStateT tweak skel

-- | Like 'runTweakInChain', but for when you want to explicitly apply a tweak
-- to a transaction skeleton and get all results as a list.
--
-- If you're trying to apply a tweak to a transaction directly before it's
-- modified, consider using 'MonadModalBlockChain' and idioms like 'withTweak',
-- 'somewhere', or 'everywhere'.
runTweakInChain' :: MonadBlockChainWithoutValidation m => Tweak m a -> TxSkel -> m [(a, TxSkel)]
runTweakInChain' tweak skel = ListT.toList $ runStateT tweak skel

-- | This is a wrapper type used in the implementation of the Staged monad. You
-- will probably never use it while you're building 'Tweak's.
data UntypedTweak m where
  UntypedTweak :: Tweak m a -> UntypedTweak m

instance Monad m => Semigroup (UntypedTweak m) where
  -- The right tweak is applied first
  UntypedTweak f <> UntypedTweak g = UntypedTweak $ g >> f

instance Monad m => Monoid (UntypedTweak m) where
  mempty = UntypedTweak $ return ()

-- * A few fundamental tweaks

-- | The never-applicable tweak.
failingTweak :: MonadTweak m => m a
failingTweak = mzero

-- | The tweak that always applies and leaves the transaction unchanged.
doNothingTweak :: MonadTweak m => m ()
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
overMaybeTweak optic mChange = overMaybeSelectingTweak optic mChange (const True)

-- | Sometimes 'overMaybeTweak' modifies too many foci. This might be the case
-- if there are several identical foci, but you only want to modify some of
-- them. This is where this 'Tweak' becomes useful: The @(Integer -> Bool)@
-- argument can be used to select which of the modifiable foci should be
-- actually modified.
overMaybeSelectingTweak ::
  forall a m k is.
  (MonadTweak m, Is k A_Traversal) =>
  Optic' k is TxSkel a ->
  (a -> Maybe a) ->
  (Integer -> Bool) ->
  m [a]
overMaybeSelectingTweak optic mChange select = do
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

-- | 'overMaybeTweak' requires a modification that can fail (targeting 'Maybe').
-- Sometimes, it can prove more convenient to explicitly state which property
-- the foci shoud satisfy to be eligible for a modification that cannot fail instead.
-- 'selectP' provides a prism to make such a selection.
-- The intended use case is 'overTweak (optic % selectP prop) mod'
-- where 'optic' gives the candidate foci, 'prop' is the predicate to be satisfied
-- by the foci, and 'mod' is the modification to be applied to the selected foci.
selectP :: (a -> Bool) -> Prism' a a
selectP prop = prism' id (\a -> if prop a then Just a else Nothing)
