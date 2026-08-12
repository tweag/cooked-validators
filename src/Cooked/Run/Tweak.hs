-- | This module applies the `Cooked.Tweak.Common.Tweak` effect for the purpose
-- of modifying transaction skeleton before sending them for validation.
module Cooked.Run.Tweak
  ( -- * Modifying mockchain runs using tweaks
    reinterpretMockChainValidateWithTweak,

    -- * Tweaks geared for 'Cooked.Skeleton.TxSkel' modifications
    TypedTweak,
    UntypedTweak (..),

    -- * Modalities to deploy 'UntypedTweak's on time
    somewhere,
    everywhere,
    nowhere,
    whenAble,
    there,
    withTweak,
  )
where

import Control.Monad
import Cooked.Effect.Validation
import Cooked.Ltl
import Cooked.Tweak.Common
import Polysemy
import Polysemy.Internal
import Polysemy.NonDet

-- | A stack of effects starting with `Tweak` and `NonDet`
type TypedTweak tweakEffs a = Sem (Tweak : NonDet : tweakEffs) a

-- | Wrapping up typed tweaks to existentially quantify on their return type
data UntypedTweak tweakEffs where
  UntypedTweak :: TypedTweak tweakEffs a -> UntypedTweak tweakEffs

-- | Applies a 'Tweak' to every step in a trace where it is applicable,
-- branching at any such locations. The tweak must apply at least once.
somewhere ::
  (Member (ModifyGlobally (UntypedTweak tweakEffs)) effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
somewhere = modifyLtl . ltlEventually . LtlAtom . UntypedTweak

-- | Applies a 'Tweak' to every transaction in a given trace. Fails if the tweak
-- fails anywhere in the trace.
everywhere ::
  (Member (ModifyGlobally (UntypedTweak tweakEffs)) effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
everywhere = modifyLtl . ltlAlways . LtlAtom . UntypedTweak

-- | Ensures a given 'Tweak' can never successfully be applied in a computation,
-- and leaves the computation unchanged.
nowhere ::
  (Member (ModifyGlobally (UntypedTweak tweakEffs)) effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
nowhere = modifyLtl . ltlNever . LtlAtom . UntypedTweak

-- | Apply a given 'Tweak' at every location in a computation where it does not
-- fail, which might never occur.
whenAble ::
  (Member (ModifyGlobally (UntypedTweak tweakEffs)) effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
whenAble = modifyLtl . ltlWhenPossible . LtlAtom . UntypedTweak

-- | Apply a 'Tweak' to the (0-indexed) nth transaction in a given
-- trace. Successful when this transaction exists and can be modified.
--
-- See also `Cooked.Tweak.Labels.labelled` to select transactions based on
-- labels instead of their index.
there ::
  (Member (ModifyGlobally (UntypedTweak tweakEffs)) effs) =>
  Integer ->
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
there n = modifyLtl . ltlDelay n . LtlAtom . UntypedTweak

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
  (Member (ModifyGlobally (UntypedTweak tweakEffs)) effs) =>
  Sem effs a ->
  TypedTweak tweakEffs b ->
  Sem effs a
withTweak = flip (there 0)

-- | Reinterpretes `MockChainValidate` in itself, when the `ModifyLocally`
-- effect exists in the stack, applying the relevant modifications in the
-- process.
reinterpretMockChainValidateWithTweak ::
  forall tweakEffs effs a.
  ( Members
      '[ ModifyLocally (UntypedTweak tweakEffs),
         NonDet
       ]
      effs,
    Subsume tweakEffs effs
  ) =>
  Sem (MockChainValidate : effs) a ->
  Sem (MockChainValidate : effs) a
reinterpretMockChainValidateWithTweak = reinterpret @MockChainValidate $ \case
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
