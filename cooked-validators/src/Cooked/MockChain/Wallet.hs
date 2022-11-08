{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Wallet where

import qualified Cardano.Api as C
import qualified Cardano.Crypto.Wallet as CWCrypto
import Control.Arrow
import Cooked.MockChain.Misc
import Data.Default
import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.CardanoWallet as CW
import qualified Ledger.Credential as Pl
import qualified Ledger.Crypto as Crypto
import qualified Ledger.Value as Pl
import Unsafe.Coerce

-- * MockChain Wallets

-- $mockchainwallets
--
-- Because the mock wallets from the plutus-apps changes somewhat often, we will
-- provide our own wrapper on top of them to make sure that we can easily deal
-- changes from Plutus.

type Wallet = CW.MockWallet

type PrivateKey = CWCrypto.XPrv

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

walletSK :: CW.MockWallet -> PrivateKey
walletSK = Pl.unPaymentPrivateKey . CW.paymentPrivateKey

-- Massive hack to be able to open a MockPrivateKey; this is needed because
-- the constructor and accessor to MockPrivateKey are not exported. Hence,
-- we make an isomorphic datatype, unsafeCoerce to this datatype then extract
-- whatever we need from it.
newtype HACK = HACK {please :: CWCrypto.XPrv}

-- | Don't use this; its a hack and will be deprecated once we have time
--  to make a PR into plutus exporting the things we need. If you use this anyway,
--  make sure that you only apply it to @MockPrivateKey@; the function is polymorphic
--  because @MockPrivateKey@ is not exported either; having a dedicated function makes
--  it easy to test that this works: check the @Cooked.MockChain.WalletSpec@ test module.
hackUnMockPrivateKey :: a -> CWCrypto.XPrv
hackUnMockPrivateKey = please . unsafeCoerce

walletStakingSK :: Wallet -> Maybe PrivateKey
walletStakingSK = fmap hackUnMockPrivateKey . CW.mwStakeKey

toPKHMap :: [Wallet] -> M.Map Pl.PubKeyHash Wallet
toPKHMap ws = M.fromList [(walletPKHash w, w) | w <- ws]

-- * Signs a transaction

txAddSignature :: Wallet -> Pl.Tx -> Pl.Tx
txAddSignature w = Pl.addSignature' (walletSK w)

txAddSignatureAPI :: Wallet -> C.Tx C.BabbageEra -> C.Tx C.BabbageEra
txAddSignatureAPI w tx = case tx' of
  Pl.CardanoApiTx (Pl.CardanoApiEmulatorEraTx tx'') -> tx''
  Pl.EmulatorTx _ -> error "Expected CardanoApiTx but got EmulatorTx"
  -- looking at the implementation of Pl.addCardanoTxSignature
  -- it never changes the constructor used, so the above branch
  -- shall never happen
  where
    tx' = Pl.addCardanoTxSignature (walletSK w) (Pl.CardanoApiTx $ Pl.CardanoApiEmulatorEraTx tx)

-- * Initial distribution of funds

-- $initfundsdistr
--
-- Are nothing but is a map from Wallet to Value; we'll just proxy
-- the underlying plutus definitions to make it easer when we have
-- to plug our own, if we ever have the need

-- | Describes the initial distribution of /UTxOs/ per wallet. This is important since
--  transaction validation must specify a /collateral/, hence, wallets must posses more
--  than one UTxO to begin with in order to execute a transaction and have some collateral
--  option. The @txCollateral@ is transfered to the node operator in case the transaction
--  fails to validate.
--
--  An initial distribution defined by:
--
--  > i0 = InitialDistribution $ M.fromList
--  >        [ (wallet 1 , [ Pl.lovelaveValueOf 42000000
--  >                      , Pl.lovelaceValueOf 2000000 <> quickValue "TOK" 1
--  >                      ]
--  >        , (wallet 2 , [Pl.lovelaveValueOf 10000000])
--  >        , (wallet 3 , [Pl.lovelaceValueOf 10000000 <> permanentValue "XYZ" 10])
--  >        ]
--
--  Specifies a starting state where @wallet 1@ contains two /UTxOs/, one with 42 Ada
--  and one with 2 Ada and one "TOK" token; @wallet 2@ contains a single /UTxO/ with 10 Ada and
--  @wallet 3@ has 10 Ada and a permanent value. Check #quickvalues for more on quick
--  and permanent values. (Remember: 1 Ada = 1000000 Lovelace)
--
--  Check the corresponding @Default InitialDistribution@ instance for the default value.
newtype InitialDistribution = InitialDistribution {distribution :: M.Map Wallet [Pl.Value]}
  deriving (Eq, Show)

-- | An initial distribution is valid if all utxos being created contain at least
--  a minimum amount of Ada: 'minAda'.
validInitialDistribution :: InitialDistribution -> Bool
validInitialDistribution = all (all hasMinAda . snd) . M.toList . distribution
  where
    hasMinAda vl = minAda `Pl.leq` vl

-- | Proxy to 'Pl.minAdaTxOut' as a 'Pl.Value'
minAda :: Pl.Value
minAda = Pl.toValue Pl.minAdaTxOut

ensureHasMinAda :: Pl.Value -> Pl.Value
ensureHasMinAda val = val <> Pl.toValue missingAda
  where
    missingAda = max 0 $ Pl.minAdaTxOut - Pl.fromValue val

instance Semigroup InitialDistribution where
  (InitialDistribution i) <> (InitialDistribution j) = InitialDistribution $ M.unionWith (<>) i j

instance Monoid InitialDistribution where
  mempty = InitialDistribution M.empty

instance Default InitialDistribution where
  def = InitialDistribution $ M.fromList $ zip knownWallets (repeat $ replicate 10 defLovelace)
    where
      defLovelace = Pl.lovelaceValueOf 100_000_000

-- | Ensures the distribution is valid by adding any missing Ada to all utxos.
distributionFromList :: [(Wallet, [Pl.Value])] -> InitialDistribution
distributionFromList = InitialDistribution . M.fromList . fmap (second $ fmap ensureHasMinAda)

-- | Extension of the default initial distribution with additional value in
-- some wallets.
initialDistribution' :: [(Wallet, [Pl.Value])] -> InitialDistribution
initialDistribution' = (def <>) . distributionFromList

initialTxFor :: InitialDistribution -> Pl.Tx
initialTxFor initDist
  | not $ validInitialDistribution initDist =
    error "Not all UTxOs have at least minAda; this initial distribution is unusable"
  | otherwise =
    mempty
      { Pl.txMint = mconcat (map (mconcat . snd) initDist'),
        Pl.txOutputs = concatMap (\(w, vs) -> map (initUtxosFor w) vs) initDist'
      }
  where
    -- initUtxosFor w v = Pl.TxOut $ Api.TxOut addr val Api.TxOutDatumNone Api.ReferenceScriptNone
    initUtxosFor w v = toPlTxOut @() (walletAddress w) v Nothing

    initDist' = M.toList $ distribution initDist

valuesForWallet :: InitialDistribution -> Wallet -> [Pl.Value]
valuesForWallet d w = fromMaybe [] $ w `M.lookup` distribution d
