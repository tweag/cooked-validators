-- | This module provides an automated attack to duplicate tokens minted in a
-- transaction.
module Cooked.Attack.DupToken (dupTokenAttack, DupTokenLbl (..)) where

import Control.Monad
import Cooked.Pretty
import Cooked.Skeleton
import Cooked.Tweak
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api

-- | A token duplication attack increases values in 'Mint' constraints of a
-- 'TxSkel' according to some conditions, and pays the extra minted value to a
-- given recipient wallet. This adds a 'DupTokenLbl' to the labels of the
-- transaction using 'addLabelTweak'. Returns the 'Value' by which the minted
-- value was increased.
dupTokenAttack ::
  (MonadTweak m, OwnerConstrs o) =>
  -- | A function describing how the amount of tokens specified by a 'Mint'
  -- constraint should be changed, depending on the asset class and the amount
  -- specified by the constraint. The given function @f@ should probably satisfy
  -- @f ac i > i@ for all @ac@ and @i@, i.e. it should increase the minted
  -- amount. If it does *not* increase the minted amount, the amount will be
  -- left unchanged.
  (Api.AssetClass -> Integer -> Integer) ->
  -- | The wallet of the attacker. Any additional tokens that are minted by the
  -- modified transaction but were not minted by the original transaction are
  -- paid to this wallet.
  o ->
  m Api.Value
dupTokenAttack change attacker = do
  oldMintsList <- viewTweak $ txSkelMintsL % txSkelMintsListI
  let (newMintsList, totalIncrement) =
        foldl
          ( \(newMs, addVal) (Mint mp@(Script.toCurrencySymbol . Script.toVersioned @Script.MintingPolicy -> cs) red tks) ->
              let (newTokensList, addValTokens) =
                    foldl
                      ( \(newTks, addVal') (tn, n) ->
                          let newAmount = change (Api.assetClass cs tn) n
                           in if newAmount > n
                                then ((tn, newAmount) : newTks, addVal' <> Api.singleton cs tn (newAmount - n))
                                else ((tn, n) : newTks, addVal')
                      )
                      ([], mempty)
                      tks
               in (Mint mp red newTokensList : newMs, addValTokens <> addVal)
          )
          ([], mempty)
          oldMintsList
  guard (totalIncrement /= mempty)
  setTweak (txSkelMintsL % txSkelMintsListI) newMintsList
  addOutputTweak $ attacker `receives` Value totalIncrement
  addLabelTweak DupTokenLbl
  return totalIncrement

-- | A label that is added to a 'TxSkel' that has successfully been modified by
-- the 'dupTokenAttack'
data DupTokenLbl = DupTokenLbl
  deriving (Eq, Show, Ord)

instance PrettyCooked DupTokenLbl where
  prettyCooked _ = "DupToken"
