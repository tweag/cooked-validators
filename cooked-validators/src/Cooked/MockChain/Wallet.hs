{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Wallet where

import qualified Cardano.Crypto.Wallet as Crypto
import Data.Default
import Data.Function (on)
import qualified Data.Map.Strict as M
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.CardanoWallet as CW
import qualified Ledger.Credential as Pl
import qualified Ledger.Crypto as Crypto
import qualified Ledger.Value as Pl
import qualified PlutusTx.Builtins.Class as Pl
import Unsafe.Coerce

-- * MockChain Wallets

-- $mockchainwallets
--
-- Because the mock wallets from the plutus-apps changes somewhat often, we will
-- provide our own wrapper on top of them to make sure that we can easily deal
-- changes from Plutus.

type Wallet = CW.MockWallet

instance Eq Wallet where
  (==) = (==) `on` CW.mwWalletId

instance Ord Wallet where
  compare = compare `on` CW.mwWalletId

knownWallets :: [Wallet]
knownWallets = CW.knownMockWallets

wallet :: Int -> Wallet
wallet j
  | j > 0 && j <= 10 = let i = j - 1 in knownWallets !! i
  | otherwise = CW.fromWalletNumber (CW.WalletNumber $ fromIntegral j)

walletPKHashToId :: Pl.PubKeyHash -> Maybe Int
walletPKHashToId = flip M.lookup walletPKHashToIdMap
  where
    walletPKHashToIdMap = M.fromList . flip zip [1 ..] . map walletPKHash $ knownWallets

walletPK :: Wallet -> Pl.PubKey
walletPK = Pl.unPaymentPubKey . CW.paymentPubKey

walletStakingPK :: Wallet -> Maybe Pl.PubKey
walletStakingPK = fmap Crypto.toPublicKey . walletStakingSK

walletPKHash :: Wallet -> Pl.PubKeyHash
walletPKHash = Pl.pubKeyHash . walletPK

walletStakingPKHash :: Wallet -> Maybe Pl.PubKeyHash
walletStakingPKHash = fmap Crypto.pubKeyHash . walletStakingPK

walletAddress :: Wallet -> Pl.Address
walletAddress w =
  Pl.Address
    (Pl.PubKeyCredential $ walletPKHash w)
    (Pl.StakingHash . Pl.PubKeyCredential <$> walletStakingPKHash w)

walletSK :: CW.MockWallet -> Pl.PrivateKey
walletSK = Pl.unPaymentPrivateKey . CW.paymentPrivateKey

-- Massive hack to be able to open a MockPrivateKey
newtype HACK = HACK {please :: Crypto.XPrv}

walletStakingSK :: Wallet -> Maybe Pl.PrivateKey
walletStakingSK = fmap (please . unsafeCoerce) . CW.mwStakeKey

toPKHMap :: [Wallet] -> M.Map Pl.PubKeyHash Wallet
toPKHMap ws = M.fromList [(walletPKHash w, w) | w <- ws]

-- * Signs a transaction

txAddSignature :: Wallet -> Pl.Tx -> Pl.Tx
txAddSignature w = Pl.addSignature' (walletSK w)

-- * Initial distribution of funds

-- $initfundsdistr
--
-- Are nothing but is a map from Wallet to Value; we'll just proxy
-- the underlying plutus definitions to make it easer when we have
-- to plug our own, if we ever have the need

newtype InitialDistribution = InitialDistribution {distribution :: M.Map Wallet Pl.Value}
  deriving (Eq, Show)

instance Semigroup InitialDistribution where
  (InitialDistribution i) <> (InitialDistribution j) = InitialDistribution $ M.unionWith (<>) i j

instance Monoid InitialDistribution where
  mempty = InitialDistribution M.empty

instance Default InitialDistribution where
  def = InitialDistribution $ M.fromList $ zip knownWallets (repeat defLovelace)
    where
      defLovelace = Pl.lovelaceValueOf 100_000_000

distributionFromList :: [(Wallet, Pl.Value)] -> InitialDistribution
distributionFromList = InitialDistribution . M.fromList

initialTxFor :: InitialDistribution -> Pl.Tx
initialTxFor initDist =
  mempty
    { Pl.txMint = mconcat (map snd initDist'),
      Pl.txOutputs = map (\wv -> Pl.TxOut (walletAddress $ fst wv) (snd wv) Nothing) initDist'
    }
  where
    initDist' = M.toList $ distribution initDist

-- "Quick" currency is a convenience to manipulate assets that are supposed to
-- be already in the wild when running a mock chain. For example, a market
-- maker would exchange Ada against other assets called "coins". Defining a
-- minting policy for those coins is tedious and not interesting. The following
-- functions make it easy to manipulate such assets identified by a token name.
--
-- For example, `runMockChainWithDistribution (initialDistribution' [wallet 1,
-- quickValue "coin" 50])` provides 50 coins to wallet 1 alongside the default
-- 100_000_000 lovelace in the initial state.

-- | Extension of the default initial distribution with additional value in
-- some wallets.
initialDistribution' :: [(Wallet, Pl.Value)] -> InitialDistribution
initialDistribution' = (def <>) . distributionFromList

-- | The currency symbol of the "quick" is empty. Like Ada.
quickCurrencySymbol :: Pl.CurrencySymbol
quickCurrencySymbol = Pl.CurrencySymbol ""

-- | Token name of a "quick" asset class
quickTokenName :: String -> Pl.TokenName
quickTokenName = Pl.TokenName . Pl.stringToBuiltinByteString

-- | "Quick" asset class from a token name
quickAssetClass :: String -> Pl.AssetClass
quickAssetClass = curry Pl.AssetClass quickCurrencySymbol . quickTokenName

-- | Constructor for "quick" values from token name and amount
quickValue :: String -> Integer -> Pl.Value
quickValue = Pl.assetClassValue . quickAssetClass
