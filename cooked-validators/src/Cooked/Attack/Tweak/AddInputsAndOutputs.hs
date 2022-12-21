{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Some 'Tweak's that add or remove inputs and outputs from transactions.
module Cooked.Attack.Tweak.AddInputsAndOutputs where

import Control.Monad
import Cooked.Attack.Tweak.Common
import Cooked.Tx.Constraints.Type
import Data.List
import qualified Data.Map as Map
import qualified Ledger as Pl
import Optics.Core
import Test.QuickCheck (NonZero (..))

-- * Adding and removing transaction inputs

-- | Ensure that a given 'SpendableOut' is being spent with a given
-- 'TxSkelIn'. The return value will be @Just@ the added data, if anything
-- changed.
ensureInputTweak :: SpendableOut -> TxSkelIn -> Tweak (Maybe (SpendableOut, TxSkelIn))
ensureInputTweak sOut howConsumed = do
  presentInputs <- viewTweak txSkelInsL
  if presentInputs Map.!? sOut == Just howConsumed
    then return Nothing
    else do
      overTweak txSkelInsL (Map.insert sOut howConsumed)
      return $ Just (sOut, howConsumed)

-- | Add an input to a transaction. If the given 'SpendableOut' is already being
-- consumed by the transaction, fail.
addInputTweak :: SpendableOut -> TxSkelIn -> Tweak ()
addInputTweak sOut howConsumed = do
  presentInputs <- viewTweak txSkelInsL
  guard (Map.notMember sOut presentInputs)
  overTweak txSkelInsL (Map.insert sOut howConsumed)

-- | Remove transaction inputs according to a given predicate. The returned list
-- contains all removed inputs.
removeInputTweak :: (SpendableOut -> TxSkelIn -> Bool) -> Tweak [(SpendableOut, TxSkelIn)]
removeInputTweak removePred = do
  presentInputs <- viewTweak txSkelInsL
  let (removed, kept) = Map.partitionWithKey removePred presentInputs
  setTweak txSkelInsL kept
  return $ Map.toList removed

-- * Adding and removing transaction outputs

-- | Ensure that a certain output is produced by a transaction. The return value
-- will be @Just@ the added output, if there was any change.
ensureOutputTweak :: TxSkelOut -> Tweak (Maybe TxSkelOut)
ensureOutputTweak txSkelOut = do
  presentOutputs <- viewTweak txSkelOutsL
  if txSkelOut `elem` presentOutputs
    then return Nothing
    else do
      addOutputTweak txSkelOut
      return $ Just txSkelOut

addOutputTweak :: TxSkelOut -> Tweak ()
addOutputTweak txSkelOut = overTweak txSkelOutsL (++ [txSkelOut])

-- | Remove transaction outputs according to some predicate. The returned list
-- contains all the removed outputs.
removeOutputTweak :: (TxSkelOut -> Bool) -> Tweak [TxSkelOut]
removeOutputTweak removePred = do
  presentOutputs <- viewTweak txSkelOutsL
  let (removed, kept) = partition removePred presentOutputs
  setTweak txSkelOutsL kept
  return removed

-- * Adding and removing minted values

-- | Add a new entry to the 'TxSkelMints' of the transaction skeleton under
-- modification. As this is implemented in terms of 'addToTxSkelMints', the same
-- caveats apply as do to that function!
addMintTweak :: (Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer) -> Tweak ()
addMintTweak mint = overTweak txSkelMintsL $ addToTxSkelMints mint

-- | Remove some entries from the 'TxSkelMints' of a transaction, according to
-- some predicate. The returned list holds the removed entries.
removeMintTweak ::
  ((Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer) -> Bool) ->
  Tweak [(Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer)]
removeMintTweak removePred = do
  presentMints <- viewTweak $ txSkelMintsL % mintsListIso
  let (removed, kept) = partition removePred presentMints
  setTweak (txSkelMintsL % mintsListIso) kept
  return removed
