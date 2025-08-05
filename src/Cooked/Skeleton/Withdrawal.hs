{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes the withdrawing constructs used in a
-- 'Cooked.Skeleton.TxSkel' and their associated utilities. To issue withdrawals
-- in a skeleton, the usual way is to invoke @txSkelWithdrawals =
-- txSkelWithdrawalsFromList [pubKeyWithdrawal pk amount, scriptWithdrawal
-- script redeemer amount, ...]@
module Cooked.Skeleton.Withdrawal
  ( -- * Data types
    Withdrawal (..),
    TxSkelWithdrawals (..),

    -- * Optics
    withdrawalUserL,
    withdrawalAmountL,
    txSkelWithdrawalsByPubKeysL,
    txSkelWithdrawalsByScriptsL,
    txSkelWithdrawalsByScriptL,
    txSkelWithdrawalsByPubKeyL,
    txSkelWithdrawalsListI,

    -- * Smart constructors
    pubKeyWithdrawal,
    scriptWithdrawal,
    txSkelWithdrawalsFromList,
  )
where

import Cooked.Skeleton.Redeemer
import Cooked.Skeleton.User
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
data TxSkelWithdrawals = TxSkelWithdrawals
  { txSkelWithdrawalsByPubKeys :: Map Api.PubKeyHash Api.Lovelace,
    txSkelWithdrawalsByScripts :: Map VScript (TxSkelRedeemer, Api.Lovelace)
  }
  deriving (Show, Eq)

-- | Focuses on the pubkey withdrawals part of this 'TxSkelWithdrawals'
makeLensesFor [("txSkelWithdrawalsByPubKeys", "txSkelWithdrawalsByPubKeysL")] ''TxSkelWithdrawals

-- | Focuses on the script withdrawals part of this 'TxSkelWithdrawals'
makeLensesFor [("txSkelWithdrawalsByScripts", "txSkelWithdrawalsByScriptsL")] ''TxSkelWithdrawals

-- | Focuses on the deposit and redeemer for a given 'VScript'
txSkelWithdrawalsByScriptL :: (ToVScript script) => script -> Lens' TxSkelWithdrawals (Maybe (TxSkelRedeemer, Api.Lovelace))
txSkelWithdrawalsByScriptL = (txSkelWithdrawalsByScriptsL %) . at . toVScript

-- | Focuses on the deposit of a given 'Api.PubKeyHash'
txSkelWithdrawalsByPubKeyL :: (Script.ToPubKeyHash pkh) => pkh -> Lens' TxSkelWithdrawals (Maybe Api.Lovelace)
txSkelWithdrawalsByPubKeyL = (txSkelWithdrawalsByPubKeysL %) . at . Script.toPubKeyHash

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
    ( \TxSkelWithdrawals {..} ->
        fmap (\(pkh, amount) -> Withdrawal (UserPubKey pkh) amount) (Map.toList txSkelWithdrawalsByPubKeys)
          ++ fmap (\(script, (red, amount)) -> Withdrawal (UserRedeemedScript script red) amount) (Map.toList txSkelWithdrawalsByScripts)
    )
    ( foldl
        ( \withdrawals (Withdrawal user amount) -> case user of
            UserPubKey pkh ->
              over
                (txSkelWithdrawalsByPubKeyL pkh)
                (maybe (Just amount) (Just . (amount +)))
                withdrawals
            UserRedeemedScript script red ->
              over
                (txSkelWithdrawalsByScriptL script)
                (maybe (Just (red, amount)) (Just . (red,) . (amount +) . snd))
                withdrawals
        )
        (TxSkelWithdrawals mempty mempty)
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
  toValue (TxSkelWithdrawals pkW scW) =
    foldl (\val -> (val <>) . Script.toValue) mempty pkW
      <> foldl (\val -> (val <>) . Script.toValue . snd) mempty scW

instance Semigroup TxSkelWithdrawals where
  txSkelW <> txSkelW' =
    review txSkelWithdrawalsListI $
      view txSkelWithdrawalsListI txSkelW
        <> view txSkelWithdrawalsListI txSkelW'

instance Monoid TxSkelWithdrawals where
  mempty = TxSkelWithdrawals mempty mempty

instance Default TxSkelWithdrawals where
  def = mempty
