{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes concrete instances to run a mockchain. There are 4 of
-- them :
--
-- - `DirectMockChain` exposes the minimal set of effects required to run a
--   mockchain, without the ability to branch or modify runs. Use this only if
--   you specifically want to disallow `Ltl` modifications.
--
-- - `StagedMockChain` exposes all the primitives required to run a mockchain,
--   with the addition of branching and `Ltl` modifications using tweaks. This
--   should be the environment to use in 99% of the cases.
--
-- - `ExtendedStagedMockChain` exposes the same primitives as `StagedMockChain`,
--   with an additional custom effect that can both be used in the main thread
--   and in the associated tweaks. This allows a mockchain run to depend on
--   arbitrary additional effects (if multiple effects are needed, this single
--   effect can be instantiated to a `Bundle` wrapping up those effects).
--
-- - `FullMockChain` exposes all the effects used to process a mockchain run,
--   including intermediate hidden in the other instances. This should only be
--   used when explicitly executing internal primitives of cooked, such as
--   balancing, is required.
module Cooked.MockChain.Instances
  ( -- * Direct, simple mockchain instance
    DirectMockChainEffs,
    DirectMockChain,

    -- * Staged mockchain instance with all effects
    FullTweakEffs,
    FullTweak,
    FullMockChainEffs,
    FullMockChain,

    -- * Staged mockchain instance with minimal effects
    StagedTweakEffs,
    StagedTweak,
    StagedMockChainEffs,
    StagedMockChain,

    -- * Staged mockchain instance with minimal effects and a custom effect
    InterpretAlone (..),
    ExtendedStagedTweakEffs,
    ExtendedStagedTweak,
    ExtendedStagedMockChainEffs,
    ExtendedStagedMockChain,
  )
where

import Cooked.Effect.Log
import Cooked.Effect.Misc
import Cooked.Effect.Override
import Cooked.Effect.Params
import Cooked.Effect.Query
import Cooked.Effect.Submission
import Cooked.Effect.Time
import Cooked.Effect.Validation
import Cooked.MockChain.Ltl
import Cooked.MockChain.Run
import Cooked.MockChain.Tweak
import Cooked.Runtime.Error
import Cooked.Runtime.Journal
import Cooked.Runtime.State
import Ledger.Tx qualified as P.Ledger
import Polysemy
import Polysemy.Bundle
import Polysemy.Error
import Polysemy.Fail
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer

-- | The most direct stack of effects to run a mockchain
type DirectMockChainEffs =
  '[ Validate,
     Override,
     Query,
     Time,
     Misc,
     Fail
   ]

-- | A mockchain computation built on top of the `DirectMockChainEffs` stack of
-- effects
type DirectMockChain a = Sem DirectMockChainEffs a

instance RunnableMockChain DirectMockChainEffs where
  runMockChain emInit ciInit =
    (: [])
      . run
      . runWriter
      . runMockChainLog
      . runState ciInit
      . runState emInit
      . runError
      . mapError CEToCardanoError
      . failToError CEFailure
      . runMockChainMisc
      . runMockChainParams
      . runMockChainTime
      . runMockChainQuery
      . runMockChainOverride
      . runMockChainSubmit
      . runChainValidate
      . insertAt @1
        @'[ Submit
          ]
      . insertAt @7
        @'[ Error P.Ledger.ToCardanoError,
            Error ChainError,
            State EmulatorState,
            State ChainIndex,
            Log,
            Writer ChainJournal
          ]
      . insertAt @4
        @'[ Params
          ]

-- | A stack of effects aimed at being used as modifications for a
-- `FullMockChain` computation
type FullTweakEffs =
  '[ Misc,
     Query,
     Time,
     Params,
     Fail,
     Error P.Ledger.ToCardanoError,
     Error ChainError,
     State EmulatorState,
     State ChainIndex,
     Log,
     Writer ChainJournal
   ]

-- | A tweak computation based on the `FullTweakEffs` stack of effects
type FullTweak a = TypedTweak FullTweakEffs a

-- | A stack of effects which allows everything allowed by `StagedMockChainEffs` with the
-- addition of all the lower level effects required to interpret it.
type FullMockChainEffs =
  '[ ModifyGlobally (UntypedTweak FullTweakEffs),
     Validate,
     Override,
     ModifyLocally (UntypedTweak FullTweakEffs),
     State [Ltl (UntypedTweak FullTweakEffs)],
     Misc,
     Query,
     Time,
     Params,
     Fail,
     Error P.Ledger.ToCardanoError,
     Error ChainError,
     State EmulatorState,
     State ChainIndex,
     Log,
     Writer ChainJournal,
     NonDet
   ]

-- | A mockchain computation built on top of the `FullMockChainEffs` stack of effects
type FullMockChain a = Sem FullMockChainEffs a

instance RunnableMockChain FullMockChainEffs where
  runMockChain emInit ciInit =
    run
      . runNonDet
      . runWriter
      . runMockChainLog
      . runState ciInit
      . runState emInit
      . runError
      . mapError CEToCardanoError
      . failToError CEFailure
      . runMockChainParams
      . runMockChainTime
      . runMockChainQuery
      . runMockChainMisc
      . evalState []
      . runModifyLocally
      . runMockChainOverride
      . runMockChainSubmit
      . runChainValidate
      . insertAt @1
        @'[ Submit
          ]
      . reinterpretMockChainValidateWithTweak @FullTweakEffs
      . runModifyGlobally

-- | A stack of effects aimed at being used as modifications for a
-- `StagedMockChain` computation
type ExtendedStagedTweakEffs extraEff =
  '[ extraEff,
     Misc,
     Query,
     Time,
     Fail
   ]

-- | A tweak computation based on the `ExtendedStagedTweakEffs` stack of effects
type ExtendedStagedTweak extraEff a = TypedTweak (ExtendedStagedTweakEffs extraEff) a

-- | A stack of effects which allows everything allowed by `DirectMockChainEffs`
-- with the addition of branching and `Ltl` modification with tweaks living in
-- `ExtendedStagedTweakEffs`
type ExtendedStagedMockChainEffs extraEff =
  '[ ModifyGlobally (UntypedTweak (ExtendedStagedTweakEffs extraEff)),
     Validate,
     Override,
     extraEff,
     Misc,
     Query,
     Time,
     Fail,
     NonDet
   ]

-- | A mockchain computation built on top of the `ExtendedStagedMockChainEffs` stack of
-- effects
type ExtendedStagedMockChain extraEff a = Sem (ExtendedStagedMockChainEffs extraEff) a

-- | The class of effects that can be interpreted on their own on top of an
-- arbitrary stack of effects
class InterpretAlone eff where
  runInterpretAlone :: Sem (eff : effs) a -> Sem effs a

instance (InterpretAlone extraEff) => RunnableMockChain (ExtendedStagedMockChainEffs extraEff) where
  runMockChain emInit ciInit =
    run
      . runNonDet
      . runWriter
      . runMockChainLog
      . runState ciInit
      . runState emInit
      . runError
      . mapError CEToCardanoError
      . failToError CEFailure
      . runMockChainParams
      . runMockChainTime
      . runMockChainQuery
      . runMockChainMisc
      . runInterpretAlone
      . evalState []
      . runModifyLocally
      . runMockChainOverride
      . runMockChainSubmit
      . runChainValidate
      . insertAt @1
        @'[ Submit
          ]
      . insertAt @10
        @'[ Error P.Ledger.ToCardanoError,
            Error ChainError,
            State EmulatorState,
            State ChainIndex,
            Log,
            Writer ChainJournal
          ]
      . reinterpretMockChainValidateWithTweak @(ExtendedStagedTweakEffs extraEff)
      . insertAt @8
        @'[ Params
          ]
      . runModifyGlobally
      . insertAt @3
        @'[ ModifyLocally (UntypedTweak (ExtendedStagedTweakEffs extraEff)),
            State [Ltl (UntypedTweak (ExtendedStagedTweakEffs extraEff))]
          ]

-- | A stack of effects aimed at being used as modifications for a
-- `StagedMockChain` computation
type StagedTweakEffs = ExtendedStagedTweakEffs (Bundle '[])

-- | A tweak computation based on the `StagedTweakEffs` stack of effects
type StagedTweak a = TypedTweak StagedTweakEffs a

-- | A stack of effects which allows everything allowed by `DirectMockChainEffs`
-- with the addition of branching and `Ltl` modification with tweaks living in
-- `StagedTweakEffs`
type StagedMockChainEffs = ExtendedStagedMockChainEffs (Bundle '[])

-- | A mockchain computation built on top of the `StagedMockChainEffs` stack of effects
type StagedMockChain a = Sem StagedMockChainEffs a

instance InterpretAlone (Bundle '[]) where
  runInterpretAlone = runBundle
