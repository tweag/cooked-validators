-- | This module defines 'Tweak's which are the building blocks of our DSL for
-- attacks. They are skeleton modifications aware of the mockchain state.
module Cooked.Tweak.Common
  ( -- * Tweak effect
    Tweak (..),
    getTxSkel,
    putTxSkel,

    -- * Running a tweak
    runTweak,
    evalTweak,
    execTweak,

    -- * Viewing tweaks
    viewTweak,
    iviewTweak,
    viewAllTweak,
    viewAnyTweak,

    -- * Setting tweaks
    setTweak,
    isetTweak,

    -- * Overing tweaks
    overTweak,
    ioverTweak,
    overModsTweak,
    overModsSelectingTweak,
    overModsTweakAll,
    overModsTweakAny,

    -- * Traversing tweaks
    traverseTweak,
    itraverseTweak,

    -- * Adding tweaks
    addTweak,
    addThereTweak,
    addFirstTweak,
    addLastTweak,

    -- * Removing tweaks
    removeIfTweak,
    removeAtTweak,

    -- * Helpers to build optics
    selectP,
    selectF,
    embedFoldable,
    embedTypeChange,
  )
where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad
import Cooked.Skeleton
import Data.Either (isRight)
import Data.List (partition)
import Data.Set qualified as Set
import Optics.Core
import Polysemy
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer

-- * Tweaks: state aware modifications over a 'TxSkel'

-- | An effect that allows to store or retrieve a 'TxSkel' from a context
data Tweak :: Effect where
  -- | Retrieves the 'TxSkel' from the context
  GetTxSkel :: Tweak m TxSkel
  -- | Overrides the 'TxSkel' in the context
  PutTxSkel :: TxSkel -> Tweak m ()

makeSem ''Tweak

-- * Running 'Tweak's

-- | Running a Tweak is equivalent to running a state monad storing a 'TxSkel'
runTweak ::
  TxSkel ->
  Sem (Tweak : effs) a ->
  Sem effs (TxSkel, a)
runTweak txSkel =
  runState txSkel
    . reinterpret
      ( \case
          GetTxSkel -> get
          PutTxSkel skel -> put skel
      )

-- | Same as 'runTweak' but discards the returned 'TxSkel'
evalTweak ::
  TxSkel ->
  Sem (Tweak : effs) a ->
  Sem effs a
evalTweak skel = (snd <$>) . runTweak skel

-- | Same as 'runTweak' but discards the returned value
execTweak ::
  TxSkel ->
  Sem (Tweak : effs) a ->
  Sem effs TxSkel
execTweak skel = (fst <$>) . runTweak skel

-- * Basic viewing 'Tweak's

-- | Retrieves the focus from the 'TxSkel' given a getter
viewTweak ::
  (Member Tweak effs, Is k A_Getter) =>
  Optic' k is TxSkel a ->
  Sem effs a
viewTweak optic = getTxSkel <&> view optic

-- | Like 'viewTweak', but also returns the index associated with the retrieved
-- focus.
iviewTweak ::
  (Member Tweak effs, Is k A_Getter) =>
  Optic' k (WithIx i) TxSkel a ->
  Sem effs (i, a)
iviewTweak optic = getTxSkel <&> iview optic

-- | Retrieves all the foci targeted by a given fold within a 'TxSkel' and
-- returns them as a list.
viewAllTweak ::
  (Member Tweak effs, Is k A_Fold) =>
  Optic' k is TxSkel a ->
  Sem effs [a]
viewAllTweak optic = getTxSkel <&> toListOf optic

-- | Like 'viewAllTweak', but returns each focus in a separate branch
viewAnyTweak ::
  (Members '[Tweak, NonDet] effs, Is k A_Fold) =>
  Optic' k is TxSkel a ->
  Sem effs a
viewAnyTweak optic = viewAllTweak optic >>= msum . fmap return

-- * Basic modifying 'Tweak's

-- | The tweak that sets a certain value in the 'TxSkel'.
setTweak ::
  (Member Tweak effs, Is k A_Setter) =>
  Optic' k is TxSkel a ->
  a ->
  Sem effs ()
setTweak optic = overTweak optic . const

-- | Like 'setTweak', but the value to set is computed from the index of each
-- focus.
isetTweak ::
  (Member Tweak effs, Is k A_Setter) =>
  Optic' k (WithIx i) TxSkel a ->
  (i -> a) ->
  Sem effs ()
isetTweak optic f = getTxSkel >>= putTxSkel . iset optic f

-- | The tweak that modifies a certain value in the 'TxSkel'.
overTweak ::
  (Member Tweak effs, Is k A_Setter) =>
  Optic k is TxSkel TxSkel a b ->
  (a -> b) ->
  Sem effs ()
overTweak optic change = getTxSkel >>= putTxSkel . over optic change

-- | Like 'overTweak', but the modification of each focus also depends on its
-- index.
ioverTweak ::
  (Member Tweak effs, Is k A_Setter) =>
  Optic k (WithIx i) TxSkel TxSkel a b ->
  (i -> a -> b) ->
  Sem effs ()
ioverTweak optic change = getTxSkel >>= putTxSkel . iover optic change

-- | Like 'overTweak', but the modification of each focus runs in the tweak's
-- effect stack. The foci are visited in the order in which they occur in the
-- 'TxSkel'.
traverseTweak ::
  (Member Tweak effs, Is k A_Traversal) =>
  Optic' k is TxSkel a ->
  (a -> Sem effs a) ->
  Sem effs ()
traverseTweak optic change = getTxSkel >>= traverseOf optic change >>= putTxSkel

-- | Like 'traverseTweak', for indexed optics
itraverseTweak ::
  (Member Tweak effs, Is k A_Traversal) =>
  Optic' k (WithIx is) TxSkel a ->
  (is -> a -> Sem effs a) ->
  Sem effs ()
itraverseTweak optic change = getTxSkel >>= itraverseOf optic change >>= putTxSkel

-- | A modification that can fail is sometimes best expressed by explicitly
-- stating which property the foci should satisfy to be eligible for a
-- modification that cannot fail. 'selectP' provides a prism to make such a
-- selection. The intended use case is @overTweak (optic % selectP prop) mod@
-- where @optic@ gives the candidate foci, @prop@ is the predicate to be
-- satisfied by the foci, and @mod@ is the modification to be applied to the
-- selected foci.
selectP ::
  (a -> Bool) ->
  Prism' a a
selectP prop = prism' id (mfilter prop . Just)

-- | Refines a traversal by selecting elements for which a given transformation
-- returns a non-empty foldable structure.
selectF ::
  ( Is k A_Traversal,
    Foldable t
  ) =>
  Optic' k is TxSkel a ->
  (a -> t b) ->
  Optic' A_Traversal is TxSkel a
selectF optic change = castOptic @A_Traversal optic % selectP (not . null . change)

-- | Embeds a foldable structure into an effect stack exposing 'NonDet', by
-- turning each of its elements into a separate non-deterministic branch.
embedFoldable ::
  ( Member NonDet effs,
    Foldable t
  ) =>
  t a ->
  Sem effs a
embedFoldable = foldr ((<|>) . pure) empty

-- | Embeds a type-changing operation into a type-preserving operation on a
-- container of the original elements. This is necessary because a simple call
-- to 'traverseOf' would be successful if the optic does not have any focus,
-- which we want to avoid (hence the @guard@ call).
embedTypeChange ::
  ( Is k An_AffineTraversal,
    Alternative f
  ) =>
  Optic k is a a b c ->
  (b -> f c) ->
  (a -> f a)
embedTypeChange (castOptic @An_AffineTraversal -> optic) mChange el =
  guard (isRight $ matching optic el)
    *> traverseOf optic mChange el

-- | When constructing a tweak from an optic and a modification of foci, there
-- are in principle two options for optics with many foci: (a) apply the
-- modification to all foci and return /one/ modified transaction (b) generate a
-- number of transactions that contain different combinations of modified and
-- un-modified foci.
--
-- This function is the most general building block for both strategies: its
-- first argument selects, per transaction, which foci are modified together,
-- so it can realise strategy (a), strategy (b), or anything in between. The
-- @overMods...@ helpers defined below specialise it to common cases. The
-- meaning of each argument and of the return value is documented on the
-- parameters themselves below.
--
--
-- __Shared setup for the examples__
--
-- Assume the optic has three foci, which we denote by @a, b, c :: x@, with
-- indices @1, 2, 3 :: Integer@ respectively.
--
-- __Example 1: modify every focus in a single transaction__
--
-- Choosing @(: [])@ for the @[is] -> [[is]]@ argument yields the single
-- grouping @[[1, 2, 3]]@, so all foci are modified together. Assuming the
-- modification does not itself branch (@changes@ returns exactly one result per
-- focus), this produces exactly /one/ modified transaction, in which @a@, @b@,
-- and @c@ are all modified. This is the grouping used by 'overModsTweakAll'.
--
-- __Example 2: one modification per transaction__
--
-- Now additionally assume that @changes@, of type @is -> x -> Sem effs (x,
-- l)@, branches into 2, 3, and 5 results on @a@, @b@, and @c@ respectively;
-- call those @a1, a2@ and @b1, b2, b3@ and @c1, c2, c3, c4, c5@. Choosing @map
-- (: [])@ for the @[is] -> [[is]]@ argument tries every modification on a
-- separate transaction, since
--
-- > map (: []) [1, 2, 3] = [[1], [2], [3]]  .
--
-- Thus there will be 2 + 3 + 5 = 10 modified transactions: for each element of
--
-- > [a1, a2, b1, b2, b3, c1, c2, c3, c4, c5]
--
-- you get one modified transaction that includes that value in place of the
-- original focus. This is the grouping used by 'overModsTweakAny'.
--
-- __Example 3: all combinations of modifications__
--
-- In the same setting, if you want to combine all possible modifications of one
-- focus with all possible modifications of the other foci, choose @tail .
-- subsequences@ for the @[is] -> [[is]]@ argument. We have
--
-- > tail (subsequences [1, 2, 3])
-- >   == [ [1], [2], [3],
-- >        [1, 2], [1, 3], [2, 3],
-- >        [1, 2, 3]
-- >      ]
--
-- This corresponds to the following 71 modified transactions, represented by
-- the list of modified foci they contain:
--
-- > [ -- one modified focus (the 10 cases from Example 2)
-- >   [a1],
-- >   [a2],
-- >   ...
-- >   [c4],
-- >   [c5],
-- >
-- >   -- two modifications of different foci (2*3 + 2*5 + 3*5 = 31 cases)
-- >   [a1, b1],
-- >   [a1, b2],
-- >   ...
-- >   [b3, c4],
-- >   [b3, c5],
-- >
-- >   -- three modified foci, one from each focus (2*3*5 = 30 cases)
-- >   [a1, b1, c1],
-- >   [a1, b1, c2],
-- >   ...
-- >   [a1, b3, c4],
-- >   [a1, b3, c5]
-- > ]
--
-- So you see that tweaks constructed like this can branch quite wildly. Use
-- with caution!
--
-- Note that if @changes@ branches to no result for a /targeted/ focus (one
-- whose index occurs in a grouping), for example via 'mzero' or 'embedFoldable'
-- of an empty structure, that entire grouping branch is dropped, since there is
-- no possible value to put in place of that focus.
overModsTweak ::
  ( Ord is,
    Is k A_Traversal,
    Members '[Tweak, NonDet] effs
  ) =>
  -- | Function that explains which subsets of targeted indexes will be
  -- simultaneously subject to being transformed. If you want to transform all
  -- foci in a single transaction (assuming the transformation itself does not
  -- branch), use @(: [])@. On the other end of the spectrum, if you want each
  -- focus to be transformed separately in their own transaction, use @fmap (:
  -- [])@. Everything in between is of course possible.
  ([is] -> [[is]]) ->
  -- | Optic targeting the various foci which should be subject to being
  -- transformed. This optic can be built manually, but can also be enlarged
  -- using convenience functions such as 'selectF' or 'elementsOf'.
  Optic' k (WithIx is) TxSkel x ->
  -- | Function that describes how the foci and their indexes can be transformed
  -- within the structure. Bear in mind that @effs@ contains @NonDet@ so this
  -- transformation can already branch. Use 'embedFoldable' to build such a
  -- transformation from simpler bricks.
  (is -> x -> Sem effs (x, l)) ->
  -- | Returns the list of all foci modified in the transaction, as they were
  -- before the modification was applied, represented by their label. In most
  -- cases, the label will be the element itself, but other use cases are
  -- allowed.
  Sem effs [l]
overModsTweak groupings optic changes = do
  -- The 'castOptic' call below is necessary: a polymorphic optic kind @k@
  -- constrained only by @Is k A_Traversal@ does not resolve @Is k A_Fold@
  -- ('itoListOf') or @Is k A_Setter@ ('ioverTweak') at the use site, so we
  -- concretise the kind to 'A_Traversal', for which those instances exist.
  let tOptic = castOptic @A_Traversal optic
  -- We retrieve all the sets of indexes that should be subject to modification
  -- in a separate computation.
  indexes <- viewTweak $ to $ groupings . fmap fst . itoListOf tOptic
  -- We make a separate branch for each of those groupings, in which we apply
  -- the modifications sequentially, for each of the targeted foci in the
  -- grouping.
  msum $
    indexes
      <&> \(Set.fromList -> grouping) -> do
        -- Before browsing through the target foci, we restrict the optics with
        -- the foci present in the grouping
        fmap fst $ runWriter $ itraverseTweak (indices (`Set.member` grouping) tOptic) $ \index el -> do
          -- For each of the foci, we perform the modification
          (el', lbl) <- raise $ changes index el
          -- We store the computed label
          tell [lbl]
          return el'

-- | 'overModsTweak' is more general than the usual use cases for tweaks. This
-- function reduces its scope and offers a more convenient signature, while
-- keeping quite a lot of expressiveness. It provides a more straightfoward way
-- to target and modify foci precisely within a transaction.
overModsSelectingTweak ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Traversal,
    Foldable f
  ) =>
  -- | Whether to branch on each targeted focus, or to modify all of them in a
  -- single transaction.
  Bool ->
  -- | Targeted foci
  Optic' k is TxSkel a ->
  -- | A transformation that both signals which of the targeted foci should be
  -- modified, and also describes the modification to apply.
  (a -> f a) ->
  -- | A predicate on the indexes of the targeted foci, after they've been
  -- filtered out by the optics above. To be used as a way to further make a
  -- distinction between foci.
  (Int -> Bool) ->
  Sem effs [a]
overModsSelectingTweak branch optic mChange select = do
  overModsTweak
    (if branch then fmap (: []) else (: []))
    (elementsOf (selectF optic mChange) select)
    (\_ a -> (,a) <$> embedFoldable (mChange a))

-- | Like 'overModsSelectingTweak' but does not branch, and does not use indexes
-- to further constrain the targeted foci.
overModsTweakAll ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Traversal,
    Foldable f
  ) =>
  Optic' k is TxSkel a ->
  (a -> f a) ->
  Sem effs [a]
overModsTweakAll optic mChange =
  overModsSelectingTweak False optic mChange (const True)

-- | Like 'overModsSelectingTweak' but always branches, and does not use indexes
-- to further constrain the targeted foci.
overModsTweakAny ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Traversal,
    Foldable f
  ) =>
  Optic' k is TxSkel a ->
  (a -> f a) ->
  Sem effs [a]
overModsTweakAny optic mChange =
  overModsSelectingTweak True optic mChange (const True)

-- | Appends an element within a semigroup focused in a 'TxSkel'
addTweak ::
  ( Member Tweak effs,
    Is k A_Setter,
    Semigroup a
  ) =>
  Optic' k is TxSkel a ->
  a ->
  Sem effs ()
addTweak optic el = overTweak optic (<> el)

-- | Appends an element at the end of a list focused in a 'TxSkel'
addLastTweak ::
  ( Member Tweak effs,
    Is k A_Setter
  ) =>
  Optic' k is TxSkel [a] ->
  a ->
  Sem effs ()
addLastTweak optic = addTweak optic . (: [])

-- | Appends an element at a specific position in a list focused in a 'TxSkel'
addThereTweak ::
  ( Member Tweak effs,
    Is k A_Setter
  ) =>
  Optic' k is TxSkel [a] ->
  Int ->
  a ->
  Sem effs ()
addThereTweak optic i a =
  overTweak optic (\(splitAt i -> (before, after)) -> before ++ (a : after))

-- | Appends an element at the end of a list focused in a 'TxSkel'
addFirstTweak ::
  ( Member Tweak effs,
    Is k A_Setter
  ) =>
  Optic' k is TxSkel [a] ->
  a ->
  Sem effs ()
addFirstTweak optic = addThereTweak optic 0

-- | Removes elements satisfying a predicate from a list focused in a 'TxSkel',
-- returning the elements that were removed.
removeIfTweak ::
  ( Member Tweak effs,
    Is k A_Lens
  ) =>
  Optic' k is TxSkel [a] ->
  (a -> Bool) ->
  Sem effs [a]
removeIfTweak (castOptic @A_Lens -> optic) removePred = do
  as <- viewTweak optic
  let (removed, kept) = partition removePred as
  setTweak optic kept
  return removed

-- | Removes an element at the specific index in a list focused in a 'TxSkel',
-- returning @Just@ the removed element if any, @Nothing@ otherwise.
removeAtTweak ::
  ( Member Tweak effs,
    Is k A_Lens
  ) =>
  Optic' k is TxSkel [a] ->
  Int ->
  Sem effs (Maybe a)
removeAtTweak (castOptic @A_Lens -> optic) i = do
  as <- viewTweak optic
  let (before, after) = splitAt i as
  case after of
    [] -> return Nothing
    x : xs -> do
      setTweak optic (before ++ xs)
      return $ Just x
