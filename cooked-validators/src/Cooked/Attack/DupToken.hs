{-# LANGUAGE FlexibleContexts #-}

module Cooked.Attack.DupToken (dupTokenAttack, DupTokenLbl (..)) where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak
import Cooked.Wallet
import Optics.Core
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.Script.Utils.V2.Scripts as Pl
import qualified Plutus.V1.Ledger.Value as Pl
import qualified PlutusTx.Numeric as Pl

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
  (Pl.AssetClass -> Integer -> Integer) ->
  -- | The wallet of the attacker. Any additional tokens that are minted by the
  -- modified transaction but were not minted by the original transaction are
  -- paid to this wallet.
  Wallet ->
  m Pl.Value
dupTokenAttack change attacker = do
  totalIncrement <- changeMintAmountsTweak
  addOutputTweak $ paysPK (walletPKHash attacker) totalIncrement
  addLabelTweak DupTokenLbl
  return totalIncrement
  where
    changeMintAmountsTweak :: MonadTweak m => m Pl.Value
    changeMintAmountsTweak = do
      oldMintsList <- viewTweak $ txSkelMintsL % to txSkelMintsToList
      let newMintsList =
            map
              ( \(Pl.Versioned policy version, redeemer, tName, oldAmount) ->
                  let ac = Pl.assetClass (Pl.mpsSymbol $ Pl.mintingPolicyHash policy) tName
                      newAmount = change ac oldAmount
                   in (Pl.Versioned policy version, redeemer, tName, max newAmount oldAmount)
              )
              oldMintsList
      guard $ newMintsList /= oldMintsList
      let newMints = txSkelMintsFromList newMintsList
          newValue = txSkelMintsValue newMints
          oldValue = txSkelMintsValue $ txSkelMintsFromList oldMintsList
      setTweak txSkelMintsL newMints
      return $ newValue <> Pl.negate oldValue

data DupTokenLbl = DupTokenLbl
  deriving (Eq, Show, Ord)
