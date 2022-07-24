module Cooked.Attack.DupToken where

import Cooked.Attack.Common
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.Maybe
import qualified Ledger as L
import Optics.Core
import qualified PlutusTx.Numeric as Pl

-- | A token duplication attack which modifies every 'Mints'-constraint of a
-- 'TxSkel' that satisfies some conditions. This adds a 'DupTokenLbl' to the
-- labels of the transaction using 'addLabel'.
dupTokenAttack ::
  -- | A function describing how the amount of minted tokens of an asset class
  -- should be changed by the attack. The given function @f@ should probably satisfy
  -- > f ac i == Just j -> i < j
  -- for all @ac@ and @i@, i.e. it should increase in the minted amount.
  -- If it does *not* increase the minted amount, or if @f ac i == Nothing@, the
  -- minted amount will be left unchanged.
  (L.AssetClass -> Integer -> Maybe Integer) ->
  -- | The wallet of the attacker. Any additional tokens that are minted by the
  -- modified transaction but were not minted by the original transaction are
  -- paid to this wallet.
  Wallet ->
  Attack
dupTokenAttack change attacker mcst skel =
  addLabel DupTokenLbl . paySurplusTo attacker skel
    <$> mkAttack (mintsConstraintsT % valueL) increaseValue mcst skel
  where
    increaseValue :: L.Value -> Maybe L.Value
    increaseValue v =
      case someJust
        ( \(ac, i) -> case change ac i of
            Nothing -> Nothing
            Just j -> if i < j then Just (ac, j) else Nothing
        )
        (view flattenValueI v) of
        Just l -> Just $ review flattenValueI l
        Nothing -> Nothing

    paySurplusTo :: Wallet -> TxSkel -> TxSkel -> TxSkel
    paySurplusTo w skelOld skelNew = over outConstraintsL (++ [paysPK (walletPKHash w) surplus]) skelNew
      where
        txSkelMintValue s = foldOf (mintsConstraintsT % valueL) s
        surplus = txSkelMintValue skelNew <> Pl.negate (txSkelMintValue skelOld)

    -- Try to use the given function to modify the elements of the given list. If
    -- at least one modification is successful, return 'Just' the modified list,
    -- otherwise fail returning 'Nothing'.
    someJust :: (a -> Maybe a) -> [a] -> Maybe [a]
    someJust _ [] = Nothing
    someJust f (x : xs) = case f x of
      Just y -> Just $ y : map (\a -> fromMaybe a (f a)) xs
      Nothing -> (x :) <$> someJust f xs

data DupTokenLbl = DupTokenLbl
  deriving (Eq, Show)
