module Plutus.Script.Utils.V1.Scripts
  ( UntypedValidator,
    UntypedStakeValidator,
    UntypedMintingPolicy,
    mkUntypedValidator,
    mkUntypedStakeValidator,
    mkUntypedMintingPolicy,
  )
where

import PlutusTx (BuiltinData, FromData, fromBuiltinData)
import PlutusTx.Prelude (BuiltinString, BuiltinUnit, check, trace)
import PlutusTx.Prelude qualified as PlutusTx

type UntypedValidator = BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit

type UntypedMintingPolicy = BuiltinData -> BuiltinData -> BuiltinUnit

type UntypedStakeValidator = BuiltinData -> BuiltinData -> BuiltinUnit

{-# INLINEABLE tracedSafeFrom #-}
tracedSafeFrom :: (FromData a) => BuiltinString -> BuiltinData -> Maybe a
tracedSafeFrom label builtinData = do
  dat <- fromBuiltinData builtinData
  -- We trace after the decoding is actually successful
  return $ trace label dat

-- | Converts a typed to an untyped validator
{-# INLINEABLE mkUntypedValidator #-}
mkUntypedValidator ::
  (FromData datum, FromData redeemer, FromData scriptContext) =>
  (datum -> redeemer -> scriptContext -> Bool) ->
  UntypedValidator
mkUntypedValidator f d r sc =
  check $
    PlutusTx.fromMaybe
      False
      ( do
          dat <- tracedSafeFrom "Datum decoded successfully" d
          red <- tracedSafeFrom "Redeemer decoded successfully" r
          ctx <- tracedSafeFrom "Script context decoded successfully" sc
          return $ f dat red ctx
      )

-- | Converts a typed to an untyped stake validator
{-# INLINEABLE mkUntypedStakeValidator #-}
mkUntypedStakeValidator ::
  (FromData redeemer, FromData scriptContext) =>
  (redeemer -> scriptContext -> Bool) ->
  UntypedStakeValidator
mkUntypedStakeValidator f r sc =
  check $
    PlutusTx.fromMaybe
      False
      ( do
          red <- tracedSafeFrom "Redeemer decoded successfully" r
          ctx <- tracedSafeFrom "Script context decoded successfully" sc
          return $ f red ctx
      )

-- | Converts a typed to an untyped minting policy
{-# INLINEABLE mkUntypedMintingPolicy #-}
mkUntypedMintingPolicy ::
  (FromData redeemer, FromData scriptContext) =>
  (redeemer -> scriptContext -> Bool) ->
  UntypedMintingPolicy
mkUntypedMintingPolicy = mkUntypedStakeValidator
