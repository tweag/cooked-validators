-- | This module applies the `Cooked.Tweak.Common.Tweak` effect for the purpose
-- of modifying transaction skeleton before sending them for validation.
module Cooked.MockChain.Tweak
  ( -- * Modifying mockchain runs using tweaks
    reinterpretMockChainWriteWithTweak,

    -- * Typed and Untyped tweaks geared for `Cooked.Skeleton.TxSkel`

    --   modifications
    TypedTweak,
    UntypedTweak (..),

    -- * Modalities to deploy `UntypedTweak`s on time
    somewhere,
    everywhere,
    nowhere,
    whenAble,
    there,
    withTweak,
  )
where

import Control.Monad
import Cooked.Ltl
import Cooked.MockChain.Write
import Cooked.Tweak.Common
import Data.Coerce
import Polysemy
import Polysemy.Internal
import Polysemy.NonDet

-- | A stack of effects starting with `Tweak` and `NonDet`
type TypedTweak tweakEffs a = Sem (Tweak : NonDet : tweakEffs) a

-- | Wrapping up typed tweaks to existentially quantify on their return type
data UntypedTweak tweakEffs where
  UntypedTweak :: TypedTweak tweakEffs a -> UntypedTweak tweakEffs

fromTweak ::
  TypedTweak tweakEffs a ->
  Ltl (UntypedTweak tweakEffs)
fromTweak = LtlAtom . UntypedTweak

-- | Applies a 'Tweak' to every step in a trace where it is applicable,
-- branching at any such locations. The tweak must apply at least once.
somewhere ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
somewhere = modifyLtl . ltlEventually . fromTweak

-- | Applies a 'Tweak' to every transaction in a given trace. Fails if the tweak
-- fails anywhere in the trace.
everywhere ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
everywhere = modifyLtl . ltlAlways . fromTweak

-- | Ensures a given 'Tweak' can never successfully be applied in a computation,
-- and leaves the computation unchanged.
nowhere ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
nowhere = modifyLtl . ltlNever . fromTweak

-- | Apply a given 'Tweak' at every location in a computation where it does not
-- fail, which might never occur.
whenAble ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
whenAble = modifyLtl . ltlWhenPossible . fromTweak

-- | Apply a 'Tweak' to the (0-indexed) nth transaction in a given
-- trace. Successful when this transaction exists and can be modified.
--
-- See also `Cooked.Tweak.Labels.labelled` to select transactions based on
-- labels instead of their index.
there ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  Integer ->
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
there n = modifyLtl . ltlDelay n . fromTweak

-- | Apply a 'Tweak' to the next transaction in the given trace. The order of
-- arguments enables an idiom like
--
-- > do ...
-- >    endpoint arguments `withTweak` someModification
-- >    ...
--
-- where @endpoint@ builds and validates a single transaction depending on the
-- given @arguments@. Then `withTweak` says "I want to modify the transaction
-- returned by this endpoint in the following way".
withTweak ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  Sem effs a ->
  TypedTweak tweakEffs b ->
  Sem effs a
withTweak = flip (there 0)

-- | Reinterpretes `MockChainWrite` in itself, when the `ModifyLocally` effect
-- exists in the stack, applying the relevant modifications in the process.
reinterpretMockChainWriteWithTweak ::
  forall tweakEffs effs a.
  ( Members
      '[ ModifyLocally (UntypedTweak tweakEffs),
         NonDet
       ]
      effs,
    Subsume tweakEffs effs
  ) =>
  Sem (MockChainWrite : effs) a ->
  Sem (MockChainWrite : effs) a
reinterpretMockChainWriteWithTweak = reinterpret @MockChainWrite $ \case
  ValidateTxSkel skel -> do
    requirements <- getRequirements
    let sumTweak :: TypedTweak tweakEffs () =
          foldr
            ( \req acc -> case req of
                Apply (UntypedTweak tweak) -> tweak >> acc
                EnsureFailure (UntypedTweak tweak) -> do
                  txSkel' <- getTxSkel
                  results <- raise_ $ runNonDet @[] $ runTweak txSkel' tweak
                  guard $ null results
                  acc
            )
            (return ())
            requirements
    newTxSkel <- raise $ subsume_ $ fst <$> runTweak skel sumTweak
    validateTxSkel newTxSkel
  a -> send $ coerce a
