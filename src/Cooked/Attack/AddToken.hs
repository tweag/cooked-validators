-- | This module provides two automated attacks to mint and give extra tokens to
-- a certain target.
module Cooked.Attack.AddToken
  ( addTokenAttack,
    AddTokenLbl (..),
    dupTokenAttack,
    DupTokenLbl (..),
  )
where

import Control.Monad
import Cooked.Pretty
import Cooked.Skeleton
import Cooked.Tweak
import Data.Map qualified as Map
import Optics.Core
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Numeric qualified as PlutusTx
import Prettyprinter qualified as PP

-- | This attack adds extra tokens of any kind for minting policies already
-- present in the minted value. The additional minted value is redirected to a
-- certain owner in a dedicated output.
--
-- This attack adds an 'AddTokenLbl' label.
addTokenAttack ::
  (MonadTweak m, IsTxSkelOutAllowedOwner o) =>
  -- | For each policy that occurs in some 'Mint' constraint, return a list of
  -- token names together with how many tokens with that name should be minted.
  (VScript -> [(Api.TokenName, Integer)]) ->
  -- | The wallet of the attacker where extra tokens will be paid to
  o ->
  m Api.Value
addTokenAttack extraTokens attacker = do
  currencies <- viewTweak (txSkelMintsL % txSkelMintsAssetClassesG % to (fmap fst))
  oldMintsValue <- viewTweak (txSkelMintsL % to Script.toValue)
  forM_ [(mp, tk, n) | mp <- currencies, (tk, n) <- extraTokens mp] $ \(mp, tk, n) ->
    overTweak (txSkelMintsL % txSkelMintsAssetClassAmountL mp tk % _2) (+ n)
  totalIncrement <- viewTweak (txSkelMintsL % to Script.toValue % to (<> PlutusTx.negate oldMintsValue))
  guard (totalIncrement /= mempty)
  addOutputTweak $ attacker `receives` Value totalIncrement
  addLabelTweak AddTokenLbl
  return totalIncrement

-- | This attack is similar to 'addTokenAttack' with the exception that it only
-- tampers with token names already present.
--
-- This attack adds an 'DupTokenLbl' label
dupTokenAttack ::
  (MonadTweak m, IsTxSkelOutAllowedOwner o) =>
  -- | A function describing how the amount of tokens specified by a 'Mint'
  -- constraint should be changed, depending on the asset class and the amount
  -- specified by the constraint. The given function @f@ should probably satisfy
  -- @f ac i > i@ for all @ac@ and @i@, i.e. it should increase the minted
  -- amount. If it does not, the tweak will still succeed but this might result
  -- in negative portions in the value paid to the attacker.
  (VScript -> Api.TokenName -> Integer -> Integer) ->
  -- | The target of the extra tokens. Any additional tokens that are minted by
  -- the modified transaction but were not minted by the original transaction
  -- are paid to this wallet.
  o ->
  m Api.Value
dupTokenAttack change attacker = do
  mints <- viewTweak txSkelMintsL
  res <-
    addTokenAttack
      ( \s ->
          maybe
            []
            (\(_, subMap) -> [(tk, change s tk n - n) | (tk, n) <- Map.toList subMap])
            (view (txSkelMintsPolicyTokensL s) mints)
      )
      attacker
  removeLabelTweak AddTokenLbl
  addLabelTweak DupTokenLbl
  return res

-- | A label that is added to a 'TxSkel' that has successfully been modified by
-- 'addTokenAttack'
data AddTokenLbl = AddTokenLbl deriving (Show, Eq, Ord)

instance PrettyCooked AddTokenLbl where
  prettyCooked = PP.viaShow

-- | A label that is added to a 'TxSkel' that has successfully been modified by
-- the 'dupTokenAttack'
data DupTokenLbl = DupTokenLbl
  deriving (Eq, Show, Ord)

instance PrettyCooked DupTokenLbl where
  prettyCooked _ = "DupToken"
