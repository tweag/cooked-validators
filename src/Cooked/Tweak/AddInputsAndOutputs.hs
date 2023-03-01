{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Some 'Tweak's that add or remove inputs and outputs from transactions.
module Cooked.Tweak.AddInputsAndOutputs
  ( ensureInputTweak,
    addInputTweak,
    removeInputTweak,
    ensureOutputTweak,
    addOutputTweak,
    removeOutputTweak,
    addMintTweak,
    removeMintTweak,
  )
where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.List
import qualified Data.Map as Map
import qualified Ledger as Pl
import Optics.Core
import qualified Plutus.V2.Ledger.Api as Pl

-- * Adding and removing transaction inputs

-- | Ensure that a given 'Pl.TxOutRef' is being spent with a given
-- 'TxSkelRedeemer'. The return value will be @Just@ the added data, if anything
-- changed.
ensureInputTweak :: MonadTweak m => Pl.TxOutRef -> TxSkelRedeemer -> m (Maybe (Pl.TxOutRef, TxSkelRedeemer))
ensureInputTweak oref howConsumed = do
  presentInputs <- viewTweak txSkelInsL
  if presentInputs Map.!? oref == Just howConsumed
    then return Nothing
    else do
      overTweak txSkelInsL (Map.insert oref howConsumed)
      return $ Just (oref, howConsumed)

-- | Add an input to a transaction. If the given 'Pl.TxOutRef' is already being
-- consumed by the transaction, fail.
addInputTweak :: MonadTweak m => Pl.TxOutRef -> TxSkelRedeemer -> m ()
addInputTweak oref howConsumed = do
  presentInputs <- viewTweak txSkelInsL
  guard (Map.notMember oref presentInputs)
  overTweak txSkelInsL (Map.insert oref howConsumed)

-- | Remove transaction inputs according to a given predicate. The returned list
-- contains all removed inputs.
removeInputTweak :: MonadTweak m => (Pl.TxOutRef -> TxSkelRedeemer -> Bool) -> m [(Pl.TxOutRef, TxSkelRedeemer)]
removeInputTweak removePred = do
  presentInputs <- viewTweak txSkelInsL
  let (removed, kept) = Map.partitionWithKey removePred presentInputs
  setTweak txSkelInsL kept
  return $ Map.toList removed

-- * Adding and removing transaction outputs

-- | Ensure that a certain output is produced by a transaction. The return value
-- will be @Just@ the added output, if there was any change.
ensureOutputTweak :: MonadTweak m => TxSkelOut -> m (Maybe TxSkelOut)
ensureOutputTweak txSkelOut = do
  presentOutputs <- viewTweak txSkelOutsL
  if txSkelOut `elem` presentOutputs
    then return Nothing
    else do
      addOutputTweak txSkelOut
      return $ Just txSkelOut

-- | Add a transaction output, at the end of the current list of outputs,
-- thus retaining the order in which they have been specified.
addOutputTweak :: MonadTweak m => TxSkelOut -> m ()
addOutputTweak txSkelOut = overTweak txSkelOutsL (++ [txSkelOut])

-- | Remove transaction outputs according to some predicate. The returned list
-- contains all the removed outputs.
removeOutputTweak :: MonadTweak m => (TxSkelOut -> Bool) -> m [TxSkelOut]
removeOutputTweak removePred = do
  presentOutputs <- viewTweak txSkelOutsL
  let (removed, kept) = partition removePred presentOutputs
  setTweak txSkelOutsL kept
  return removed

-- * Adding and removing minted values

-- | Add a new entry to the 'TxSkelMints' of the transaction skeleton under
-- modification. As this is implemented in terms of 'addToTxSkelMints', the same
-- caveats apply as do to that function!
addMintTweak :: MonadTweak m => (Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, Integer) -> m ()
addMintTweak mint = overTweak txSkelMintsL $ addToTxSkelMints mint

-- | Remove some entries from the 'TxSkelMints' of a transaction, according to
-- some predicate. The returned list holds the removed entries.
removeMintTweak ::
  MonadTweak m =>
  ((Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, Integer) -> Bool) ->
  m [(Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, Integer)]
removeMintTweak removePred = do
  presentMints <- viewTweak $ txSkelMintsL % to txSkelMintsToList
  let (removed, kept) = partition removePred presentMints
  setTweak txSkelMintsL $ txSkelMintsFromList kept
  return removed
