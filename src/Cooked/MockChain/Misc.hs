{-# LANGUAGE TemplateHaskell #-}

-- | This module defines primitives that offer quality of life features when
-- operating a mockchain without interacting with the mockchain state itself.
module Cooked.MockChain.Misc
  ( -- * Misc effect
    MockChainMisc (..),
    runMockChainMisc,

    -- * Storing aliases for hashable elements
    define,
    defineM,

    -- * Taking notes in the notebook
    note,
    noteP,
    noteL,
    noteS,

    -- * Asserting properties
    assert,
    assert',
  )
where

import Cooked.Pretty.Class
import Cooked.Pretty.Hashable
import Cooked.Pretty.Options
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Writer
import Prettyprinter qualified as PP

-- | An effect that corresponds to extra QOL capabilities of the MockChain
data MockChainMisc :: Effect where
  Define :: (ToHash a) => String -> a -> MockChainMisc m a
  Note :: (PrettyCookedOpts -> DocCooked) -> MockChainMisc m ()
  Assert :: String -> Bool -> MockChainMisc m ()

makeSem_ ''MockChainMisc

-- | Interpreting a `MockChainMisc` in terms of a writer of @Map
-- BuiltinByteString String@
runMockChainMisc ::
  forall effs a j.
  (Member (Writer j) effs) =>
  (String -> Api.BuiltinByteString -> j) ->
  ((PrettyCookedOpts -> DocCooked) -> j) ->
  (String -> Bool -> j) ->
  Sem (MockChainMisc : effs) a ->
  Sem effs a
runMockChainMisc injectAlias injectNote injectPred = interpret $ \case
  (Define name hashable) -> tell (injectAlias name $ toHash hashable) >> return hashable
  (Note s) -> tell $ injectNote s
  (Assert s b) -> tell $ injectPred s b

-- | Stores an alias matching a hashable data for pretty printing purpose
define :: forall effs a. (Member MockChainMisc effs, ToHash a) => String -> a -> Sem effs a

-- | Like `define`, but binds the result of a monadic computation instead
defineM :: (Member MockChainMisc effs, ToHash a) => String -> Sem effs a -> Sem effs a
defineM name = (define name =<<)

-- | Takes note of a showable element to trace at the end of the run
note :: forall effs. (Member MockChainMisc effs) => (PrettyCookedOpts -> DocCooked) -> Sem effs ()

-- | Takes note of a pretty-printable element to trace at the end of the run
noteP :: forall effs s. (Member MockChainMisc effs, PrettyCooked s) => s -> Sem effs ()
noteP doc = note (`prettyCookedOpt` doc)

-- | Takes note of a pretty-printable element as list with a title, to trace at
-- the end of the run
noteL :: forall effs l. (Member MockChainMisc effs, PrettyCookedList l) => String -> l -> Sem effs ()
noteL title docs = note $ \opts -> prettyItemize opts (prettyCooked title) "-" docs

-- | Takes note of a showable element to trace at the end of the run
noteS :: forall effs s. (Member MockChainMisc effs, Show s) => s -> Sem effs ()
noteS doc = note $ const (PP.viaShow doc)

-- | Ensures a specific property holds, sending the provided error message otherwise
assert :: forall effs. (Member MockChainMisc effs) => String -> Bool -> Sem effs ()

-- | Ensures a specific property holds, with a default error message otherwise
assert' :: forall effs. (Member MockChainMisc effs) => Bool -> Sem effs ()
assert' = assert "Assertion error"
