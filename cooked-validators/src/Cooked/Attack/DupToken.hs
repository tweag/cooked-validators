{-# LANGUAGE FlexibleContexts #-}

module Cooked.Attack.DupToken where

import Control.Monad
import Cooked.Attack.Tweak
import Cooked.MockChain.Wallet
import Cooked.Skeleton
import qualified Ledger as L
import qualified Ledger.Scripts as Pl
import qualified Ledger.Value as Pl
import Optics.Core
import qualified PlutusTx.Numeric as Pl
import Test.QuickCheck.Modifiers (NonZero (..))

-- | A token duplication attack increases values in 'Mints'-constraints of a
-- 'TxSkel' according to some conditions, and pays the extra minted value to a
-- given recipient wallet. This adds a 'DupTokenLbl' to the labels of the
-- transaction using 'addLabel'. Returns the 'Value' by which the minted value
-- was increased.
dupTokenAttack ::
  MonadTweak m =>
  -- | A function describing how the amount of tokens specified by a 'Mints'
  -- constraint should be changed, depending on the asset class and the amount
  -- specified by the constraint. The given function @f@ should probably satisfy
  -- @f ac i > i@ for all @ac@ and @i@, i.e. it should increase the minted
  -- amount. If it does *not* increase the minted amount, the amount will be
  -- left unchanged.
  (L.AssetClass -> Integer -> Integer) ->
  -- | The wallet of the attacker. Any additional tokens that are minted by the
  -- modified transaction but were not minted by the original transaction are
  -- paid to this wallet.
  Wallet ->
  m L.Value
dupTokenAttack change attacker = do
  totalIncrement <- changeMintAmountsTweak
  addOutputTweak $ paysPK (walletPKHash attacker) totalIncrement
  addLabelTweak DupTokenLbl
  return totalIncrement
  where
    changeMintAmountsTweak :: MonadTweak m => m Pl.Value
    changeMintAmountsTweak = do
      oldMintsList <- viewTweak $ txSkelMintsL % mintsListIso
      let newMintsList =
            map
              ( \(policy, redeemer, tName, NonZero oldAmount) ->
                  let ac = Pl.assetClass (Pl.mpsSymbol $ Pl.mintingPolicyHash policy) tName
                      newAmount = change ac oldAmount
                   in (policy, redeemer, tName, NonZero $ max newAmount oldAmount)
              )
              oldMintsList
      guard $ newMintsList /= oldMintsList
      let newMints = review mintsListIso newMintsList
          newValue = txSkelMintsValue newMints
          oldValue = txSkelMintsValue $ txSkelMintsFromList oldMintsList
      setTweak txSkelMintsL newMints
      return $ newValue <> Pl.negate oldValue

data DupTokenLbl = DupTokenLbl
  deriving (Eq, Show, Ord)
