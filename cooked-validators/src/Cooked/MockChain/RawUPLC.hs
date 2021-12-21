{-# LANGUAGE RankNTypes #-}

module Cooked.MockChain.RawUPLC where

import Data.ByteString
import qualified Flat
import Ledger.Typed.Scripts
import Plutus.V1.Ledger.Scripts
import qualified PlutusCore as P
import qualified UntypedPlutusCore.Core.Type as UPLC

-- | Loads a untyped PLC program from a bytestring that was produced with the
--   [getPlc](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Code.hs#L84)
--   method.
uplcFromBS :: ByteString -> Either String (TypedValidator Any)
uplcFromBS = fmap (unsafeMkTypedValidator . Validator . Script . unameDeBruijn) . uplcDecoder
  where
    uplcDecoder ::
      ByteString ->
      Either String (UPLC.Program P.NamedDeBruijn P.DefaultUni P.DefaultFun ())
    uplcDecoder = either (Left . show) Right . Flat.unflat

-- | Erases the names everywhere
unameDeBruijn :: UPLC.Program P.NamedDeBruijn uni fun a -> UPLC.Program P.DeBruijn uni fun a
unameDeBruijn (UPLC.Program a v term) = UPLC.Program a v (go term)
  where
    go :: UPLC.Term P.NamedDeBruijn uni fun a -> UPLC.Term P.DeBruijn uni fun a
    go (UPLC.Var ann name) = UPLC.Var ann (P.DeBruijn $ P.ndbnIndex name)
    go (UPLC.LamAbs ann name t) = UPLC.LamAbs ann (P.DeBruijn $ P.ndbnIndex name) (go t)
    go (UPLC.Apply ann t u) = UPLC.Apply ann (go t) (go u)
    go (UPLC.Force ann t) = UPLC.Force ann (go t)
    go (UPLC.Delay ann t) = UPLC.Delay ann (go t)
    go (UPLC.Constant ann u) = UPLC.Constant ann u
    go (UPLC.Builtin ann fun) = UPLC.Builtin ann fun
    go (UPLC.Error ann) = UPLC.Error ann
