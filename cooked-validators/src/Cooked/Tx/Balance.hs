{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cooked.Tx.Balance where

import Control.Arrow ((***))
import Data.Function (on)
import Data.Kind
import Data.List (foldl', sortBy)
import qualified Ledger.Ada as Pl
import qualified Ledger.Contexts as Pl
import qualified Ledger.Value as Pl
import PlutusTx.Numeric ((+), (-))
import Prelude hiding ((+), (-))

class (Show out, Show (BOutRef out), Eq (BOutRef out)) => BalancableOut out where
  -- | The 'BOutRef's are only used to keep track of what outputs were used during balancing.
  -- They might as well be identity (type) functions if 'out' is cheap and comparable.
  type BOutRef out = (result :: Type) | result -> out

  outValue :: out -> Pl.Value
  outRef :: out -> BOutRef out

instance BalancableOut (Pl.TxOutRef, Pl.TxOut) where
  type BOutRef (Pl.TxOutRef, Pl.TxOut) = Pl.TxOutRef

  outValue = Pl.txOutValue . snd
  outRef = fst

-- | This function will try to select a selection of outputs from the set of available
--  outputs given to it in order to have enough to balance out the given value.
balanceWithUTxOs ::
  forall out.
  (BalancableOut out) =>
  Pl.Value ->
  [out] ->
  ([BOutRef out], Pl.Value, ExcessiveValue)
balanceWithUTxOs val =
  -- HACK: We'll order outputs and try to use those with the most ada first;
  -- should hopefully help with: https://github.com/tweag/plutus-libs/issues/71#issuecomment-1016406041
  -- Nevertheless, we might eventually need a better solution
  spendValueFrom val . sortBy (flip compare `on` (adaValueOf . outValue))
  where
    adaValueOf = Pl.getLovelace . Pl.fromValue

type ExcessiveValue = Pl.Value

-- | A helper to make sure that @input + mint = output + fees@ holds for a transaction.
--
-- Namely, @val@ is @output - input@, and @utxos@ is the list of the unspent outputs
-- that can be used to balance the transaction.
--
-- If a token value in @val@ is negative,
-- it means that the transaction got more of that token as input than as output.
-- We don't balance it in this case,
-- since we don't want to (and can't, really) choose where that extra output goes.
--
-- This function returns the UTXOs that were spent (a subset of @utxos@), any leftover value
-- (in case some UTXOs had more output than we actually need to balance the transaction)
-- and the excess value which needs to be routed somewhere.
spendValueFrom ::
  forall out.
  (BalancableOut out) =>
  Pl.Value ->
  [out] ->
  ([BOutRef out], Pl.Value, ExcessiveValue)
spendValueFrom val utxos =
  -- We then go through the output value and investigate for each token
  -- if there is a way to balance it.
  foldl'
    (\st (sym, tok, i) -> addInputToBalance sym tok i st)
    ([], mempty, mempty)
    (Pl.flattenValue val)
  where
    addInputToBalance ::
      Pl.CurrencySymbol ->
      Pl.TokenName ->
      Integer ->
      ([BOutRef out], Pl.Value, ExcessiveValue) ->
      ([BOutRef out], Pl.Value, ExcessiveValue)
    addInputToBalance curr token i (usedUTxO, leftOver, excess) =
      if i < 0
        then -- If the input of the transaction is already too big,
        -- then we can't solve it by adding inputs to it. Instead, we keep the
        -- excess value separate from the leftover and let the user decide what to
        -- do with it later.
          (usedUTxO, leftOver, excess <> Pl.singleton curr token (negate i))
        else
          let thisTokLeftOver = Pl.valueOf leftOver curr token
           in -- Here, the output of the transaction is bigger in i units of the (curr,token) asset class.
              -- The first thing we check is if the leftOver happens to have enough of the needed token,
              -- which can happen by pure accident.
              if i <= thisTokLeftOver
                then (usedUTxO, leftOver - Pl.singleton curr token i, excess)
                else -- The leftover does not have enough tokens. We need to select some utxos from the
                -- pool of available utxos; the 'necessaryUtxosFor' makes sure to never select
                -- a repeated utxo for us.

                  let (newUsedUTxOs, additionalLeftover) =
                        necessaryUtxosFor curr token (i - thisTokLeftOver) utxos usedUTxO
                   in -- The part taken form the leftover, is not in the previous leftover anymore.
                      let prevLeftOver = leftOver - Pl.singleton curr token thisTokLeftOver
                       in (usedUTxO ++ newUsedUTxOs, prevLeftOver <> additionalLeftover, excess)

-- Given an asset name (both currency and token),
-- an integer n, a list of UTxOs and the subset of those which are already used,
-- return the TxOutRef we need to consume
-- in order to cover n and returns any potential leftover that need to be
-- transfered to n's owner again.
--
-- We perform no sorting nor optimization, so we'll just select the first k necessary outrefs:
--
-- > necessaryUtxosFor a t 100 [(o1, 50, dh1), (o2, 30, dh2), (o3, 30, dh3), (o4, 120, dh4)] []
-- >    == ([o1, o2, o3], 10)
--
necessaryUtxosFor ::
  BalancableOut out =>
  Pl.CurrencySymbol ->
  Pl.TokenName ->
  Integer ->
  [out] ->
  [BOutRef out] ->
  ([BOutRef out], Pl.Value)
necessaryUtxosFor _ _ 0 _ _ = ([], mempty)
necessaryUtxosFor curr token n (o : os) usedUTxO
  -- If an output has already been added to the transaction previously,
  -- it is considered in the leftover, which had been taken into account earlier in the process.
  | oref `elem` usedUTxO = necessaryUtxosFor curr token n os usedUTxO
  -- If the output has more than required, we add it to the transaction
  -- and update the leftover accordingly.
  | valOf (outValue o) > n = ([oref], outValue o - Pl.singleton curr token n)
  -- If the output does not contain the token we are looking for at all,
  -- then it is pointless to add it in the transaction.
  | valOf (outValue o) == 0 = necessaryUtxosFor curr token n os usedUTxO
  -- If the output partially fulfill our expectation, we grab it into the transaction,
  -- and continue our investigation.
  | otherwise =
    let foundAmount = valOf (outValue o)
        valLeftover = outValue o - Pl.singleton curr token foundAmount
     in ((oref :) *** (valLeftover +))
          (necessaryUtxosFor curr token (n - foundAmount) os usedUTxO)
  where
    valOf val = Pl.valueOf val curr token
    oref = outRef o
-- If there is not enough funds, the transaction will fail.
-- Even if we are already aware of it at this point,
-- we do not want to interfere with the error generated by the contract,
-- hence we let the wrong transaction happen.
necessaryUtxosFor _ _ _ [] _ = ([], mempty)
