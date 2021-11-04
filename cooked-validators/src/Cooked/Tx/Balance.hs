module Cooked.Tx.Balance where

import Control.Arrow ((***))
import Control.Monad.State.Class
import Cooked.MockChain
import qualified Data.Set as S
import qualified Ledger.Constraints as Pl
import qualified Ledger.Constraints.OffChain as Pl
import qualified Ledger.Contexts as Pl
import qualified Ledger.Index as Pl
import qualified Ledger.TimeSlot as Pl
import qualified Ledger.Tx as Pl
import qualified Ledger.Value as Pl
import qualified Plutus.V1.Ledger.Address as Pl
import qualified Plutus.V1.Ledger.Credential as Pl
import qualified Plutus.V1.Ledger.Crypto as Pl
import qualified PlutusTx.Numeric as Pl

-- | Balances a transaction with money from a given wallet. For every transaction,
--  it must be the case that @inputs + mint == outputs + fee@.
balanceTxFrom :: (Monad m) => Wallet -> Pl.UnbalancedTx -> MockChainT m Pl.Tx
balanceTxFrom w (Pl.UnbalancedTx tx0 _reqSigs _uindex slotRange) = do
  -- We start by gathering all the inputs and summing it
  let tx = tx0 {Pl.txFee = Pl.minFee tx0}
  lhsInputs <- mapM (outFromOutRef . Pl.txInRef) (S.toList (Pl.txInputs tx))
  let lhs = mappend (mconcat $ map Pl.txOutValue lhsInputs) (Pl.txMint tx)
  let rhs = mappend (mconcat $ map Pl.txOutValue $ Pl.txOutputs tx) (Pl.txFee tx)
  let wPKH = walletPKHash w
  (usedUTxOs, leftOver) <- balanceWithUTxOsOf (rhs Pl.- lhs) wPKH
  -- All the UTxOs signed by the sender of the transaction and useful to balance it
  -- are added to the inputs.
  let txIns' = map (`Pl.TxIn` Just Pl.ConsumePublicKeyAddress) usedUTxOs
  -- A new output is opened with the leftover of the added inputs.
  let txOut' = Pl.TxOut (Pl.Address (Pl.PubKeyCredential wPKH) Nothing) leftOver Nothing
  config <- gets (slotConfig . mcstSlotCtr)
  return
    tx
      { Pl.txInputs = Pl.txInputs tx <> S.fromList txIns'
      , Pl.txOutputs = Pl.txOutputs tx ++ [txOut']
      , Pl.txValidRange = Pl.posixTimeRangeToContainedSlotRange config slotRange
      }

balanceWithUTxOsOf ::
  (Monad m) =>
  Pl.Value ->
  Pl.PubKeyHash ->
  MockChainT m ([Pl.TxOutRef], Pl.Value)
balanceWithUTxOsOf val wPKH =
  spendValueFrom val <$> pkUtxos' wPKH

spendValueFrom :: Pl.Value -> [(Pl.TxOutRef, Pl.TxOut)] -> ([Pl.TxOutRef], Pl.Value)
spendValueFrom val utxos =
  -- We then go through the output value and investigate for each token
  -- if there is a way to balance it.
  foldl
    (\(spentTx, leftO) (curr, tok, i) -> addInputToBalance curr tok i spentTx leftO)
    ([], mempty)
    (Pl.flattenValue val)
  where
    addInputToBalance ::
      Pl.CurrencySymbol ->
      Pl.TokenName ->
      Integer ->
      [Pl.TxOutRef] ->
      Pl.Value ->
      ([Pl.TxOutRef], Pl.Value)
    addInputToBalance curr token i usedUTxO leftOver =
      if i < 0
        then -- If the input of the transaction is already too big,
        -- then no matter which inputs we add, the transaction will fail.
        -- We still send it, in order to get the error of the chain.
        -- However, we know it will be a "MCEValidation(...,ValueNotPreserved(...))".
        -- We could have try to balance automatically more transaction
        -- by simply sending the leftover to the creator of the transaction.
        -- However, in most of the case, allocating the whole money exchanged in the transaction
        -- is the expected situation, hence we want the author to be aware of this oversight.
          (usedUTxO, leftOver)
        else
          let thisTokLeftOver = Pl.valueOf leftOver curr token
           in if i > thisTokLeftOver
                then -- Even while using the leftover, we still are looking for some money.

                  let (newUsedUTxOs, additionalLeftover) =
                        necessaryUtxosFor curr token (i - thisTokLeftOver) utxos usedUTxO
                   in -- The part taken form the leftover, is not in the previous leftover anymore.
                      let prevLeftOver = leftOver Pl.- Pl.singleton curr token thisTokLeftOver
                       in (usedUTxO ++ newUsedUTxOs, prevLeftOver <> additionalLeftover)
                else -- If the leftover contains more (or exactly) money than required,
                -- then it is simply taken from the left-over.
                  (usedUTxO, leftOver Pl.- Pl.singleton curr token i)

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
  Pl.CurrencySymbol ->
  Pl.TokenName ->
  Integer ->
  [(Pl.TxOutRef, Pl.TxOut)] ->
  [Pl.TxOutRef] ->
  ([Pl.TxOutRef], Pl.Value)
necessaryUtxosFor _ _ 0 _ _ = ([], mempty)
necessaryUtxosFor curr token n ((oref, o) : os) usedUTxO
  -- If an output has already been added to the transaction previously,
  -- it is considered in the leftover, which had been taken into account earlier in the process.
  | oref `elem` usedUTxO = necessaryUtxosFor curr token n os usedUTxO
  -- If the output has more than required, we add it to the transaction
  -- and update the leftover accordingly.
  | valOf (Pl.txOutValue o) > n = ([oref], Pl.txOutValue o Pl.- Pl.singleton curr token n)
  -- If the output does not contain the token we are looking for at all,
  -- then it is pointless to add it in the transaction.
  | valOf (Pl.txOutValue o) == 0 = necessaryUtxosFor curr token n os usedUTxO
  -- If the output partially fulfill our expectation, we grab it into the transaction,
  -- and continue our investigation.
  | otherwise =
    let foundAmount = valOf (Pl.txOutValue o)
     in let valLeftover = Pl.txOutValue o Pl.- Pl.singleton curr token foundAmount
         in ((oref :) *** (valLeftover Pl.+)) $
              necessaryUtxosFor curr token (n - foundAmount) os usedUTxO
  where
    valOf val = Pl.valueOf val curr token
-- If there is not enough funds, the transaction will fail.
-- Even if we are already aware of it at this point,
-- we do not want to interfere with the error generated by the contract,
-- hence we let the wrong transaction happen.
necessaryUtxosFor _ _ _ [] _ = ([], mempty)
