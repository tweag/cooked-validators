{-# LANGUAGE TemplateHaskell #-}

-- | This module defines primitives that offer quality of life features when
-- operating a mockchain without interacting with the mockchain state itself.
module Cooked.Effect.Misc
  ( -- * Misc effect
    Misc (..),
    runMockChainMisc,
    runBlockChainMisc,

    -- * Storing aliases for hashable elements
    define,
    define_,
    defineM,
    defineM_,

    -- * Taking notes in the notebook
    note,
    noteP,
    noteL,
    noteW,
    noteS,

    -- * Asserting properties
    assert,
    assert',
    assertP,
    assertL,
    assertW,
    assertS,
  )
where

import Control.Monad (void)
import Cooked.Pretty.Class
import Cooked.Pretty.Hashable
import Cooked.Pretty.Options
import Cooked.Runtime.Journal
import Data.Map qualified as Map
import Polysemy
import Polysemy.Fail
import Polysemy.State
import Polysemy.Writer
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP

-- | An effect that corresponds to extra QOL capabilities of the MockChain
data Misc :: Effect where
  Define :: (ToHash a) => String -> a -> Misc m a
  Note :: (PrettyCookedOpts -> DocCooked) -> Misc m ()
  Assert :: (PrettyCookedOpts -> DocCooked) -> Bool -> Misc m ()

makeSem_ ''Misc

-- | Stores an alias matching a hashable data for pretty printing purpose
define :: forall effs a. (Member Misc effs, ToHash a) => String -> a -> Sem effs a

-- | Like 'define', but discards the result
define_ :: forall effs a. (Member Misc effs, ToHash a) => String -> a -> Sem effs ()
define_ name = void . define name

-- | Like `define`, but binds the result of a monadic computation instead
defineM :: forall effs a. (Member Misc effs, ToHash a) => String -> Sem effs a -> Sem effs a
defineM name = (define name =<<)

-- | Like 'defineM', but discards the result
defineM_ :: forall effs a. (Member Misc effs, ToHash a) => String -> Sem effs a -> Sem effs ()
defineM_ name = void . defineM name

-- | Takes note of an element represented as its rendering function to trace at
-- the end of the run
note :: forall effs. (Member Misc effs) => (PrettyCookedOpts -> DocCooked) -> Sem effs ()

-- | Takes note of a pretty-printable element to trace at the end of the run
noteP :: forall effs s. (Member Misc effs, PrettyCooked s) => s -> Sem effs ()
noteP doc = note (`prettyCookedOpt` doc)

-- | Takes note of a pretty-printable element as list with a title, to trace at
-- the end of the run
noteL :: forall effs l. (Member Misc effs, PrettyCookedList l) => String -> l -> Sem effs ()
noteL title docs = note $ \opts -> prettyItemize opts (prettyCooked title) "-" docs

-- | Takes note of a showable element to trace at the end of the run
noteW :: forall effs s. (Member Misc effs, Show s) => s -> Sem effs ()
noteW = note . const . PP.viaShow

-- | Takes note of a String to trace at the end of the run
noteS :: forall effs. (Member Misc effs) => String -> Sem effs ()
noteS = noteP

-- | Ensures a specific property holds, rendering the provided message with the
-- ambient pretty-printing options otherwise
assert :: forall effs. (Member Misc effs) => (PrettyCookedOpts -> DocCooked) -> Bool -> Sem effs ()

-- | Like `assert`, but with a pretty-printable message
assertP :: forall effs s. (Member Misc effs, PrettyCooked s) => s -> Bool -> Sem effs ()
assertP doc = assert (`prettyCookedOpt` doc)

-- | Like `assert`, but with a pretty-printable message displayed as a list with
-- a title
assertL :: forall effs l. (Member Misc effs, PrettyCookedList l) => String -> l -> Bool -> Sem effs ()
assertL title docs = assert $ \opts -> prettyItemize opts (prettyCooked title) "-" docs

-- | Like `assert`, but with a showable message
assertW :: forall effs s. (Member Misc effs, Show s) => s -> Bool -> Sem effs ()
assertW = assert . const . PP.viaShow

-- | Like `assert`, but with a `String` message
assertS :: forall effs. (Member Misc effs) => String -> Bool -> Sem effs ()
assertS = assertP

-- | Like `assert`, but with a default error message
assert' :: forall effs. (Member Misc effs) => Bool -> Sem effs ()
assert' = assertS "Assertion"

-- | Interprets a `Misc` in terms of a writer in @j@ where @j@ can be
-- built from either of the three possible parameters of the 3 misc actions. The
-- 3 actions only update the state, which is only used at the end of the run.
runMockChainMisc ::
  forall effs a.
  (Member (Writer ChainJournal) effs) =>
  Sem (Misc : effs) a ->
  Sem effs a
runMockChainMisc = interpret $ \case
  (Define name hashable) -> tell (fromAlias name $ toHash hashable) >> return hashable
  (Note s) -> tell $ fromNote s
  (Assert s b) -> tell $ fromAssert s b

-- | Interprets a `Misc` in the context of a deployed node, running in a
-- stack featuring @IO@ (via `Embed`). Contrary to `runMockChainMisc`, which
-- gathers everything in a journal to be inspected at the end of the run, this
-- interpreter reacts immediately:
--
-- * `Define` registers an alias in the ambient `PrettyCookedOpts`, so that every
--   subsequent rendering (later notes and assertions) benefits from it.
--
-- * `Note` is rendered using the current `PrettyCookedOpts` and printed to the
--   standard output right away, then the run proceeds.
--
-- * `Assert` prints the associated message. When the asserted property holds,
--   the run proceeds; otherwise the whole computation is stopped.
runBlockChainMisc ::
  forall effs a.
  ( Members
      '[ Embed IO,
         State PrettyCookedOpts,
         Fail
       ]
      effs
  ) =>
  Sem (Misc : effs) a ->
  Sem effs a
runBlockChainMisc = interpret $ \case
  Define name hashable -> do
    modify $ addHashNames $ Map.singleton (toHash hashable) name
    return hashable
  Note s -> gets s >>= embed . PP.putDoc . (<> PP.line) . ("⁕" <+>)
  Assert s b -> do
    doc <- gets s
    if b
      then embed $ PP.putDoc $ "✔" <+> doc <> PP.line
      else fail $ renderString id $ "✘" <+> doc <> PP.line
