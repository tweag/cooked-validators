module Cooked.Balance where

import Data.Default
import Control.Arrow (second, (***))
import Control.Monad.Identity
import qualified Data.Set as S
import Ledger.Contexts
import Ledger.Index
import Ledger.Value
import Ledger.Constraints
import Ledger.Constraints.OffChain hiding (tx)
import Ledger.Tx
import Ledger.Orphans                   ()
import Ledger.TimeSlot (posixTimeRangeToContainedSlotRange)

import Plutus.Contract.Trace
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential      (Credential (..))
import qualified PlutusTx.Numeric as Num

import Cooked.MockChain

-- |Balances a transaction with money from a given wallet. For every transaction,
-- it must be the case that @inputs + mint == outputs + fee@.
balanceTxFrom :: (Monad m) => Wallet -> UnbalancedTx -> MockChainT m Tx
balanceTxFrom w (UnbalancedTx tx0 _reqSigs _uindex slotRange) = do
  -- We start by gathering all the inputs and summing it
  let tx = tx0 { txFee = minFee tx0 }
  lhsInputs <- mapM (outsFromRef . txInRef) (S.toList (txInputs tx))
  let lhs = mappend (mconcat $ map txOutValue lhsInputs)      (txMint tx)
  let rhs = mappend (mconcat $ map txOutValue $ txOutputs tx) (txFee tx)
  -- We then go through the output value and investigate for each token
  -- if there is a way to balance it.
  (leftover, usedUTxO) <-
    foldM (\(leftOver, usedUTxO) (a,t,_) -> addInputToEquilibrate lhs rhs a t leftOver usedUTxO)
      (mempty, [])
      (flattenValue rhs)
  -- All the UTxOs signed by the sender of the transaction and useful to balance it
  -- are added to the inputs.
  let txIns' = map (`TxIn` Just ConsumePublicKeyAddress) usedUTxO
  -- A new output is opened with the leftover of the added inputs.
  let txOut' =
        TxOut
          (Address (PubKeyCredential $ pubKeyHash $ walletPubKey w) Nothing)
          leftover Nothing
  return tx{ txInputs  = txInputs tx <> S.fromList txIns'
           , txOutputs = txOutputs tx ++ [txOut']
           , txValidRange = posixTimeRangeToContainedSlotRange def slotRange
           }
  where
    addInputToEquilibrate lhs rhs asset token leftOver usedUTxO =
      let delta = valueOf rhs asset token - valueOf lhs asset token in
      case compare delta 0 of
      EQ -> return (leftOver, usedUTxO)
      -- If the input of the transaction is already too big,
      -- then no matter which inputs we add, the transaction will fail.
      -- We still send it, in order to get the error of the chain.
      -- However, we know it will be a "MCEValidation(...,ValueNotPreserved(...))".
      -- We could have try to balance automatically more transaction
      -- by simply sending the leftover to the creator of the transaction.
      -- However, in most of the case, allocating the whole money exchanged in the transaction
      GT -> return (leftOver, usedUTxO)
      LT ->
        let thisTokLeftOver = valueOf leftOver asset token in
        if delta > thisTokLeftOver
        then do
          -- Even while using the leftover, we still are looking for some money.
          -- Hence, we go through all the outputs of the transaction creator.
          refsFromW <- utxosFromPK' (pubKeyHash $ walletPubKey w)
          (neededRefsFromW, additionalLeftover) <-
            necessaryUtxosFor asset token (delta - thisTokLeftOver) refsFromW usedUTxO
          -- The part taken form the leftover, is not in the previous leftover anymore.
          let prevLeftOver = leftOver Num.- singleton asset token thisTokLeftOver
          return (prevLeftOver <> additionalLeftover, usedUTxO ++ neededRefsFromW)
        -- If the leftover contains more (or exactly) money than required,
        -- then it is simply taken from the left-over.
        else return (leftOver Num.- singleton asset token delta, usedUTxO)

-- Given an the asset name (both currency and token),
-- an integer n, a list of UTxOs and the subset of those, which are already used,
-- return the TxOutRes we need to consume
-- in order to cover n and returns any potential leftover that need to be
-- transfered to n's owner again.
--
-- We perform no sorting nor optimization, so we'll just select the first k necessary outrefs:
--
-- > necessaryUtxosFor a t 100 [(o1, 50, dh1), (o2, 30, dh2), (o3, 30, dh3), (o4, 120, dh4)] []
-- >    == ([o1, o2, o3], 10)
--
necessaryUtxosFor :: (Monad m)
                  => CurrencySymbol -> TokenName
                  -> Integer -> [(TxOutRef, TxOut)]
                  -> [TxOutRef]
                  -> MockChainT m ([TxOutRef], Value)
necessaryUtxosFor _     _     0 _  _ = return ([], mempty)
necessaryUtxosFor asset token n ((oref, o):os) usedUTxO
  -- If an output has already been added to the transaction previously,
  -- it is considered in the leftover, which had been taken into account earlier in the process.
  | oref `elem` usedUTxO = necessaryUtxosFor asset token n os usedUTxO
  -- If the output has more than required, we add it to the transaction
  -- and update the leftover accordingly.
  | valOf (txOutValue o) > n = return ([oref], txOutValue o Num.- singleton asset token n)
  -- If the output does not contain the token we are looking for at all,
  -- then it is pointless to add it in the transaction.
  | valOf (txOutValue o) == 0 = necessaryUtxosFor asset token n os usedUTxO
  -- If the output partially fulfill our expectation, we grab it into the transaction,
  -- and continue our investigation.
  | otherwise =
      let foundAmount = valOf (txOutValue o) in
      let valLeftover = txOutValue o Num.- singleton asset token foundAmount in
      ((oref :) *** (valLeftover Num.+ ))
        <$> necessaryUtxosFor asset token (n - foundAmount) os usedUTxO

  where valOf val = valueOf val asset token
-- If there is not enough funds, the transaction will fail.
-- Even if we are already aware of it at this point,
-- we do not want to interfere with the error generated by the contract,
-- hence we let the wrong transaction happen.
necessaryUtxosFor _     _     _ [] _ = return ([], mempty)