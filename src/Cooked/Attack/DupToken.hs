-- | This module provides an automated attack to duplicate tokens minted in a
-- transaction.
module Cooked.Attack.DupToken (dupTokenAttack, DupTokenLbl (..)) where

import Control.Monad
import Cooked.Pretty
import Cooked.Skeleton
import Cooked.Tweak
import Cooked.Wallet
import Optics.Core
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V3.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Numeric qualified as PlutusTx

-- | A token duplication attack increases values in 'Mints'-constraints of a
-- 'TxSkel' according to some conditions, and pays the extra minted value to a
-- given recipient wallet. This adds a 'DupTokenLbl' to the labels of the
-- transaction using 'addLabel'. Returns the 'Value' by which the minted value
-- was increased.
dupTokenAttack ::
  (MonadTweak m) =>
  -- | A function describing how the amount of tokens specified by a 'Mints'
  -- constraint should be changed, depending on the asset class and the amount
  -- specified by the constraint. The given function @f@ should probably satisfy
  -- @f ac i > i@ for all @ac@ and @i@, i.e. it should increase the minted
  -- amount. If it does *not* increase the minted amount, the amount will be
  -- left unchanged.
  (Script.AssetClass -> Integer -> Integer) ->
  -- | The wallet of the attacker. Any additional tokens that are minted by the
  -- modified transaction but were not minted by the original transaction are
  -- paid to this wallet.
  Wallet ->
  m Api.Value
dupTokenAttack change attacker = do
  totalIncrement <- changeMintAmountsTweak
  addOutputTweak $ paysPK (walletPKHash attacker) totalIncrement
  addLabelTweak DupTokenLbl
  return totalIncrement
  where
    changeMintAmountsTweak :: (MonadTweak m) => m Api.Value
    changeMintAmountsTweak = do
      oldMintsList <- viewTweak $ txSkelMintsL % to txSkelMintsToList
      let newMintsList =
            map
              ( \(Script.Versioned policy version, redeemer, tName, oldAmount) ->
                  let ac = Script.assetClass (Script.mpsSymbol $ Script.mintingPolicyHash policy) tName
                      newAmount = change ac oldAmount
                   in (Script.Versioned policy version, redeemer, tName, max newAmount oldAmount)
              )
              oldMintsList
      guard $ newMintsList /= oldMintsList
      let newMints = txSkelMintsFromList newMintsList
          newValue = txSkelMintsValue newMints
          oldValue = txSkelMintsValue $ txSkelMintsFromList oldMintsList
      setTweak txSkelMintsL newMints
      return $ newValue <> PlutusTx.negate oldValue

data DupTokenLbl = DupTokenLbl
  deriving (Eq, Show, Ord)

instance PrettyCooked DupTokenLbl where
  prettyCooked _ = "DupToken"
