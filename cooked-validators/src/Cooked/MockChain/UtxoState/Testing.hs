{-# LANGUAGE OverloadedStrings #-}

module Cooked.MockChain.UtxoState.Testing where

import Cooked.MockChain.Testing
import Cooked.MockChain.UtxoPredicate (hasOnlyAda)
import Cooked.MockChain.UtxoState
import qualified Cooked.PlutusDeps as Pl
import qualified Data.Map.Strict as M

-- * Relations between states

-- | Returns whether two states are equal except for arbitrary differences in Ada.
--  This implies that the set of addresses in each state is the same, hence, the "equal" prefix.
--  If you don't care about that, check 'equivModuloAtMost'.
equalModuloAda :: (IsProp prop) => UtxoState -> UtxoState -> prop
equalModuloAda a b = testAll (eqModAda . snd) $ M.toList $ utxoStateDiff a b
  where
    -- If the address was modified, the states are equal modulo Ada if the modifications
    -- contain only ada
    eqModAda (Modified del ins _) =
      let valDiff = utxoValueSetTotal ins <> Pl.negate (utxoValueSetTotal del)
       in hasOnlyAdaWithMsg valDiff
    -- If the address was deleted or inserted, the states are different in other ways
    -- other than just ada.
    eqModAda _ = testSuccess

    hasOnlyAdaWithMsg val =
      testCounterexample ("Has more than just Ada: " ++ show val) $ testBool $ hasOnlyAda val

-- | Returns whether the difference in value between two states is at most
--  a given bound. Note that the difference in value between states can be negative,
--  hence, the order of parameters matters here. See 'utxoStateDiffTotal' for more.
equivModuloAtMost :: (IsProp prop) => Pl.Value -> UtxoState -> UtxoState -> prop
equivModuloAtMost maxV a b =
  noNegativeToken . (maxV <>) . Pl.negate $ utxoStateDiffTotal $ utxoStateDiff a b
  where
    noNegativeToken :: IsProp prop => Pl.Value -> prop
    noNegativeToken = testAll (\(_, _, i) -> testBool $ i >= 0) . Pl.flattenValue
