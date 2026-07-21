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

    -- * Modification parameters
    Branching (..),
    ModifyTweakParams (..),
    modifyTweakParamsAllIndexes,
    modifyTweakParamsNoTypeChange,
    modifyTweakParamsOneBranchForAllFoci,
    modifyTweakParamsOneBranchPerFoci,
    modifyTweakParamsOneBranchPerSubset,

    -- * Modifying tweaks
    modifyTweak,
    modifyTweakFromParams,

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

    -- * Helper to build optics
    selectP,
  )
where

import Control.Applicative (Alternative)
import Control.Monad
import Cooked.Skeleton
import Data.Either (isRight)
import Data.Either.Combinators (fromRight')
import Data.List (partition, subsequences)
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
--
-- Note that 'selectP' is not a lawful prism: its build side is 'id', so
-- reviewing a value that does not satisfy @prop@ breaks the
-- @preview o (review o b) ≡ Just b@ law. We nevertheless keep it as a 'Prism''
-- (rather than the lawful but read-only @filtered@, which is an @AffineFold@)
-- because we need the write capability: composing a traversal
-- with a prism stays a traversal, whereas composing it with an @AffineFold@
-- collapses to a read-only fold. This is safe in the intended
-- @overTweak (optic % selectP prop) mod@ pattern, where the unlawful build side
-- is never exercised: we only ever reach foci that already satisfy @prop@.
selectP ::
  (a -> Bool) ->
  Prism' a a
selectP prop = prism' id (mfilter prop . Just)

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
-- and @c@ are all modified. This is the grouping used by the 'All' 'Branching'
-- of 'modifyTweak'.
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
-- original focus. This is the grouping used by the 'Any' 'Branching' of
-- 'modifyTweak'.
--
-- __Example 3: all combinations of modifications__
--
-- In the same setting, if you want to combine all possible modifications of one
-- focus with all possible modifications of the other foci, choose @tail .
-- subsequences@ for the @[is] -> [[is]]@ argument. This is the grouping used by
-- the 'OneBranchPerSubset' 'Branching' of 'modifyTweak'. We have
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
-- whose index occurs in a grouping), that entire grouping branch is dropped,
-- since there is no possible value to put in place of that focus.
modifyTweak ::
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
  -- transformation can already branch.
  (is -> x -> Sem effs (x, l)) ->
  -- | Returns the list of all foci modified in the transaction, as they were
  -- before the modification was applied, represented by their label. In most
  -- cases, the label will be the element itself, but other use cases are
  -- allowed as the label is arbitrary data.
  Sem effs [l]
modifyTweak groupings optic changes = do
  -- The 'castOptic' call below is necessary: a polymorphic optic kind @k@
  -- constrained only by @Is k A_Traversal@ does not resolve @Is k A_Fold@
  -- ('itoListOf') or @Is k A_Setter@ ('ioverTweak') at the use site, so we
  -- concretise the kind to 'A_Traversal', for which those instances exist.
  let tOptic = castOptic @A_Traversal optic
  -- We retrieve all the sets of indexes that should be subject to modification
  -- in a separate computation, removing the empty groupings in the process,
  -- which would yield an unmodified transaction. NOTE: removing the empty
  -- groupings is a design decision, not a necessity.
  indexes <- viewTweak $ to $ filter (not . null) . groupings . fmap fst . itoListOf tOptic
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
          -- We return the modified foci
          return el'

-- | How to combine the branches generated when several foci are eligible for
-- modification in the same skeleton. See 'modifyTweak'.
data Branching
  = -- | Modify all eligible foci together, yielding a single modified skeleton.
    OneBranchForAllFoci
  | -- | Modify exactly one eligible focus per branch.
    OneBranchPerFoci
  | -- | Modify every non-empty subset of the eligible foci, yielding one branch
    -- per subset (the power set, minus the empty set).
    OneBranchPerSubset
  | -- | Create a new branch for all the subsets computed by the given function
    -- applied on the focused indexes.
    Manual ([Int] -> [[Int]])

-- | The set of parameters piloting the modification tweak
data ModifyTweakParams k k' is is' f a b c where
  ModifyTweakParams ::
    { -- | The branching policy to apply when several foci are targeted
      branching :: Branching,
      -- | A type-preserving optic traversing the 'TxSkel' and pinpointing a first
      -- layer of elements. Being type-preserving, it only chooses /where/ to act.
      outerOptic :: Optic' k is TxSkel a,
      -- | A second, type-changing 'AffineTraversal' reaching from within each
      -- selected element to the inner focus that is actually modified. It carries
      -- the @b -> c@ type change that the outer optic cannot, and its
      -- affine-ness (0 or 1 focus) acts as an additional selection layer.
      innerOptic :: Optic k' is' a a b c,
      -- | The modifying function, which can possibly fail through @f@, further
      -- selecting elements to modify.
      modification :: b -> f c,
      -- | A selection function based on the indexes of the selected foci. This is
      -- the last layer of selection, if all the others are insufficient.
      selection :: Int -> Bool
    } ->
    ModifyTweakParams k k' is is' f a b c

-- | A standard 'ModifyTweakParams' without the index filtering
modifyTweakParamsAllIndexes ::
  Branching ->
  Optic' k is TxSkel a ->
  Optic k' is' a a b c ->
  (b -> f c) ->
  ModifyTweakParams k k' is is' f a b c
modifyTweakParamsAllIndexes branching opticOut opticIn mChange =
  ModifyTweakParams branching opticOut opticIn mChange (const True)

-- | A standard 'ModifyTweakParams' without any index filtering or type changing
-- inner optic
modifyTweakParamsNoTypeChange ::
  Branching ->
  Optic' k is TxSkel a ->
  (a -> f a) ->
  ModifyTweakParams k An_Iso is NoIx f a a a
modifyTweakParamsNoTypeChange branching optic =
  modifyTweakParamsAllIndexes branching optic simple

-- | A standard 'ModifyTweakParams' without any index filtering or type changing
-- inner optic, modifying each foci in the same transaction.
modifyTweakParamsOneBranchForAllFoci ::
  Optic' k is TxSkel a ->
  (a -> f a) ->
  ModifyTweakParams k An_Iso is NoIx f a a a
modifyTweakParamsOneBranchForAllFoci =
  modifyTweakParamsNoTypeChange OneBranchForAllFoci

-- | A standard 'ModifyTweakParams' without any index filtering or type changing
-- inner optic, branching on each foci.
modifyTweakParamsOneBranchPerFoci ::
  Optic' k is TxSkel a ->
  (a -> f a) ->
  ModifyTweakParams k An_Iso is NoIx f a a a
modifyTweakParamsOneBranchPerFoci =
  modifyTweakParamsNoTypeChange OneBranchPerFoci

-- | A standard 'ModifyTweakParams' without any index filtering or type changing
-- inner optic, branching on each subset of foci.
modifyTweakParamsOneBranchPerSubset ::
  Optic' k is TxSkel a ->
  (a -> f a) ->
  ModifyTweakParams k An_Iso is NoIx f a a a
modifyTweakParamsOneBranchPerSubset =
  modifyTweakParamsNoTypeChange OneBranchPerSubset

-- | The most convenient and expressive way to build a focusing-and-modifying
-- 'Tweak'. It targets foci in a 'TxSkel' through /two/ optics and applies a
-- (possibly type-changing, possibly failing) modification to them, branching
-- over the eligible foci according to the requested 'Branching' strategy.
--
-- == Why two optics?
--
-- The underlying engine ('modifyTweak') can only apply /type-preserving/
-- modifications: each modified focus is written back into the 'TxSkel' in
-- place, so the skeleton's overall shape is fixed and the outer optic must be
-- an @'Optic'' ... TxSkel a@. That optic can therefore only decide /where/ in
-- the skeleton to act; it cannot express a change of type.
--
-- The modification we actually want to perform is finer-grained and
-- /type-changing/: within each selected element @a@, an inner part of type @b@
-- is replaced by a value of type @c@. This type change cannot ride on the
-- outer, type-preserving optic, which is exactly why a second optic is needed.
-- @opticIn@ localises and carries the type change inside each selected element,
-- and 'traverseOf' folds it back into a type-preserving operation @a -> f a@,
-- so that the outer traversal stays type-preserving while the /inner/ focus
-- still changes from @b@ to @c@.
--
-- The second optic serves a second purpose: being an 'AffineTraversal' (zero
-- or one focus), it doubles as an extra selection layer. We guard on
-- 'matching', so an element is only eligible when its inner focus actually
-- exists there. Together with the failure allowed by @f@ in @change@
-- and the index predicate @select@, this gives several independent layers of
-- selection: outer optic, inner-optic existence, modification success, and
-- index.
modifyTweakFromParams ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Traversal,
    Is k' An_AffineTraversal,
    Foldable f,
    Alternative f
  ) =>
  -- | The parameters piloting the modifications
  ModifyTweakParams k k' is is' f a b c ->
  -- | Returns the list of inner foci (as they were /before/ modification) that
  -- were modified.
  Sem effs [b]
modifyTweakFromParams
  ( ModifyTweakParams
      branching
      (castOptic @A_Traversal -> opticOut)
      (castOptic @An_AffineTraversal -> opticIn)
      change
      select
    ) =
    let -- This turns the inner, type-changing modification into a type-preserving
        -- @a -> f a@ operation. The @guard@ fails (in @f@) when the element has
        -- no inner focus, so the outer engine only ever sees type-preserving
        -- work, and 'traverseOf' rebuilds the same @a@ with its inner @b@
        -- replaced by a @c@.
        mChange a = guard (isRight $ matching opticIn a) *> traverseOf opticIn change a
     in modifyTweak
          ( case branching of
              OneBranchForAllFoci -> (: [])
              OneBranchPerFoci -> fmap (: [])
              OneBranchPerSubset -> tail . subsequences
              Manual f -> f
          )
          -- 'selectF' keeps only the outer foci where @mChange@ is non-empty, and
          -- @select@ further restricts them by index.
          (elementsOf (opticOut % selectP (not . null . mChange)) select)
          -- We pair each non-deterministically modified element with the
          -- original inner focus. 'matching' (not 'preview') is required
          -- here because @opticIn@ is type-changing; the 'fromRight'' is safe
          -- because 'selectF'/the @guard@ already guaranteed a focus.
          (\_ a -> (,fromRight' $ matching opticIn a) <$> msum (return <$> mChange a))

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
