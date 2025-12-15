{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes the withdrawing constructs used in a
-- 'Cooked.Skeleton.TxSkel' and their associated utilities. To issue withdrawals
-- in a skeleton, the usual way is to invoke @txSkelWithdrawals =
-- txSkelWithdrawalsFromList [pubKeyWithdrawal pk amount, scriptWithdrawal
-- script redeemer amount, ...]@
module Cooked.Skeleton.Withdrawal
  ( -- * Data types
    Withdrawal (..),
    TxSkelWithdrawals,

    -- * Optics
    withdrawalUserL,
    withdrawalAmountL,
    txSkelWithdrawalsListI,

    -- * Smart constructors
    pubKeyWithdrawal,
    scriptWithdrawal,
    txSkelWithdrawalsFromList,
  )
where

import Cooked.Skeleton.Redeemer
import Cooked.Skeleton.User
import Data.Bifunctor (bimap)
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Typeable (Typeable, cast)
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Withdrawals associate either a script or a private key with a redeemer and
-- a certain amount of ada.
type TxSkelWithdrawals = Map Api.BuiltinByteString (User IsEither Redemption, Api.Lovelace)

-- | A single 'Withdrawal', owned by a pubkey or redeemed script
data Withdrawal where
  Withdrawal ::
    { withdrawalUser :: User IsEither Redemption,
      withdrawalAmount :: Api.Lovelace
    } ->
    Withdrawal

deriving instance Show Withdrawal

instance Eq Withdrawal where
  Withdrawal user amount == Withdrawal user' amount' = cast user == Just user' && amount == amount'

-- | Focuses on the amount in a 'Withdrawal'
makeLensesFor [("withdrawalAmount", "withdrawalAmountL")] ''Withdrawal

-- | Focuses on the user owning a 'Withdrawal'
makeLensesFor [("withdrawalUser", "withdrawalUserL")] ''Withdrawal

-- | Transforms a @[Withdrawal]@ to a 'TxSkelWithdrawals and vice
-- versa. Accumulates amount of withdrawals with similar owners, and keep the
-- latest found redeemer in the case of scripts, discarding the previous ones.
txSkelWithdrawalsListI :: Iso' TxSkelWithdrawals [Withdrawal]
txSkelWithdrawalsListI =
  iso
    (fmap (uncurry Withdrawal) . Map.elems)
    ( foldl
        ( \withdrawals (Withdrawal user@(view userHashG -> hash) amount) ->
            over
              (at hash)
              (maybe (Just (user, amount)) (Just . bimap (const user) (+ amount)))
              withdrawals
        )
        mempty
    )

-- | Creates a 'Withdrawal' from a private key hash and lovelace amount
pubKeyWithdrawal :: (Script.ToPubKeyHash pkh, Typeable pkh) => pkh -> Integer -> Withdrawal
pubKeyWithdrawal pkh = Withdrawal (UserPubKey pkh) . Api.Lovelace

-- | Creates a 'Withdrawal' from a redeemed script and lovelace amount
scriptWithdrawal :: (ToVScript script, Typeable script, RedeemerConstrs red) => script -> red -> Integer -> Withdrawal
scriptWithdrawal script red = Withdrawal (UserRedeemedScript script (someTxSkelRedeemer red)) . Api.Lovelace

-- | Builds a 'TxSkelWithdrawals' from a list of 'Withdrawal'. This is
-- equivalent to calling @review txSkelWithdrawalsListI@
txSkelWithdrawalsFromList :: [Withdrawal] -> TxSkelWithdrawals
txSkelWithdrawalsFromList = review txSkelWithdrawalsListI

-- | Retrieves the total value withdrawn is this 'TxSkelWithdrawals'
instance Script.ToValue TxSkelWithdrawals where
  toValue = foldl (\val -> (val <>) . Script.toValue . snd) mempty

instance {-# OVERLAPPING #-} Semigroup TxSkelWithdrawals where
  txSkelW <> txSkelW' =
    review txSkelWithdrawalsListI $
      view txSkelWithdrawalsListI txSkelW
        <> view txSkelWithdrawalsListI txSkelW'

instance {-# OVERLAPPING #-} Monoid TxSkelWithdrawals where
  mempty = Map.empty

instance Default TxSkelWithdrawals where
  def = mempty
