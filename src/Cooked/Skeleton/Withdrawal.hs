{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes the withdrawing constructs used in a
-- 'Cooked.Skeleton.TxSkel' and their associated utilities. To issue withdrawals
-- in a skeleton, the usual way is to invoke @txSkelWithdrawals =
-- txSkelWithdrawalsFromList [pubKeyWithdrawal pk amount, scriptWithdrawal
-- script redeemer amount, ...]@
module Cooked.Skeleton.Withdrawal
  ( -- * Data types
    Withdrawal (..),
    TxSkelWithdrawals (unTxSkelWithdrawals),

    -- * Optics
    withdrawalUserL,
    withdrawalMAmountL,
    withdrawalAmountAT,
    txSkelWithdrawalsListI,

    -- * Smart constructors
    pubKeyWithdrawal,
    scriptWithdrawal,
    txSkelWithdrawalsFromList,

    -- * Utilities
    autoFillAmount,
  )
where

import Cooked.Skeleton.Redeemer
import Cooked.Skeleton.User
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Typeable (Typeable)
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Withdrawals associate either a script or a private key with a redeemer and
-- a certain amount of ada. They are uniquely identified by the hash of either.
newtype TxSkelWithdrawals = TxSkelWithdrawals {unTxSkelWithdrawals :: Map Api.Credential Withdrawal}
  deriving (Show, Eq)

-- | A single 'Withdrawal', owned by a pubkey or redeemed script
data Withdrawal where
  Withdrawal ::
    { -- | The user making a withdrawals of their rewards
      withdrawalUser :: User IsEither Redemption,
      -- | The amount of lovelace to withdraw. If set to 'Nothing', cooked will
      -- attempt to fill this out with the current rewards for the user, which
      -- is the only acceptable amount ledger-wise. Manually setting this value
      -- is only left as a possibility for testing purposes.
      withdrawalAmount :: Maybe Api.Lovelace
    } ->
    Withdrawal
  deriving (Show, Eq)

-- | Focuses on the optional amount in a 'Withdrawal'
makeLensesFor [("withdrawalAmount", "withdrawalMAmountL")] ''Withdrawal

-- | Focuses on the user owning a 'Withdrawal'
makeLensesFor [("withdrawalUser", "withdrawalUserL")] ''Withdrawal

-- | Focuses on the amount in a 'Withdrawal'
withdrawalAmountAT :: AffineTraversal' Withdrawal Api.Lovelace
withdrawalAmountAT = withdrawalMAmountL % _Just

-- | Transforms a @[Withdrawal]@ to a 'TxSkelWithdrawals and vice
-- versa. Accumulates amount of withdrawals with similar owners, and keep the
-- latest found redeemer in the case of scripts, discarding the previous ones.
txSkelWithdrawalsListI :: Iso' TxSkelWithdrawals [Withdrawal]
txSkelWithdrawalsListI =
  iso
    (Map.elems . unTxSkelWithdrawals)
    ( foldl
        ( \(TxSkelWithdrawals withdrawals) withdrawal@(Withdrawal (view userCredentialG -> cred) amount) ->
            TxSkelWithdrawals $
              over
                (at cred)
                (maybe (Just withdrawal) (Just . over withdrawalMAmountL (maybe amount (Just . (+ fromMaybe 0 amount)))))
                withdrawals
        )
        mempty
    )

-- | Creates a 'Withdrawal' from a private key hash
pubKeyWithdrawal :: (Script.ToPubKeyHash pkh, Typeable pkh) => pkh -> Withdrawal
pubKeyWithdrawal = (`Withdrawal` Nothing) . UserPubKey

-- | Creates a 'Withdrawal' from a redeemed script and lovelace amount
scriptWithdrawal :: (ToVScript script, Typeable script, RedeemerConstrs red) => script -> red -> Withdrawal
scriptWithdrawal script red = Withdrawal (UserRedeemedScript script (someTxSkelRedeemer red)) Nothing

-- | Builds a 'TxSkelWithdrawals' from a list of 'Withdrawal'. This is
-- equivalent to calling @review txSkelWithdrawalsListI@
txSkelWithdrawalsFromList :: [Withdrawal] -> TxSkelWithdrawals
txSkelWithdrawalsFromList = review txSkelWithdrawalsListI

-- | Fills a given amount of lovelace to withdraw, if not already set
autoFillAmount :: Api.Lovelace -> Withdrawal -> Withdrawal
autoFillAmount newAmount = over withdrawalMAmountL (maybe (Just newAmount) Just)

-- | Retrieves the total value withdrawn is this 'TxSkelWithdrawals'
instance Script.ToValue TxSkelWithdrawals where
  toValue = foldl (\val -> (val <>) . maybe mempty Script.toValue . view withdrawalMAmountL) mempty . unTxSkelWithdrawals

instance Semigroup TxSkelWithdrawals where
  txSkelW <> txSkelW' =
    review txSkelWithdrawalsListI $
      view txSkelWithdrawalsListI txSkelW
        <> view txSkelWithdrawalsListI txSkelW'

instance Monoid TxSkelWithdrawals where
  mempty = TxSkelWithdrawals mempty
