{-# LANGUAGE OverloadedStrings #-}

module Cooked.MockChain.UtxoState.Testing where

import Cooked.MockChain.Testing
import Cooked.MockChain.UtxoState
import qualified Data.Map.Strict as M
import qualified Ledger as Pl
import qualified Ledger.Value as Pl
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified PlutusTx.Numeric as Pl

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
       in hasOnlyAda valDiff
    -- If the address was deleted or inserted, the states are different in other ways
    -- other than just ada.
    eqModAda _ = testSuccess

    hasOnlyAda :: (IsProp prop) => Pl.Value -> prop
    hasOnlyAda v = case Pl.flattenValue v of
      [] -> testSuccess
      [(sym, tok, _)] -> sym .==. Ada.adaSymbol .&&. tok .==. Ada.adaToken
      _ -> testFailureMsg "Value has more than just Ada"

-- | Returns whether the difference in value between two states is at most
--  a given bound. Note that the difference in value between states can be negative,
--  hence, the order of parameters matters here. See 'utxoStateDiffTotal' for more.
equivModuloAtMost :: (IsProp prop) => Pl.Value -> UtxoState -> UtxoState -> prop
equivModuloAtMost maxV a b =
  noNegativeToken . (maxV <>) . Pl.negate $ utxoStateDiffTotal $ utxoStateDiff a b
  where
    noNegativeToken :: IsProp prop => Pl.Value -> prop
    noNegativeToken = testAll (\(_, _, i) -> testBool $ i >= 0) . Pl.flattenValue
