{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.Wallet where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Wallet as Cardano
import Data.Default
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.CardanoWallet as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Tx.CardanoAPI.Internal as Pl
import qualified Plutus.V2.Ledger.Tx as Pl2 (OutputDatum (..))
import qualified PlutusTx as Pl
import Unsafe.Coerce

-- * MockChain Wallets

-- $mockchainwallets
--
-- Because the mock wallets from the plutus-apps changes somewhat often, we will
-- provide our own wrapper on top of them to make sure that we can easily deal
-- changes from Plutus.

type Wallet = Pl.MockWallet

type PrivateKey = Cardano.XPrv

instance Eq Wallet where
  (==) = (==) `on` Pl.mwWalletId

instance Ord Wallet where
  compare = compare `on` Pl.mwWalletId

knownWallets :: [Wallet]
knownWallets = Pl.knownMockWallets

wallet :: Int -> Wallet
wallet j
  | j > 0 && j <= 10 = let i = j - 1 in knownWallets !! i
  | otherwise = Pl.fromWalletNumber (Pl.WalletNumber $ fromIntegral j)

walletPKHashToId :: Pl.PubKeyHash -> Maybe Int
walletPKHashToId = flip Map.lookup walletPKHashToIdMap
  where
    walletPKHashToIdMap = Map.fromList . flip zip [1 ..] . map walletPKHash $ knownWallets

walletPK :: Wallet -> Pl.PubKey
walletPK = Pl.unPaymentPubKey . Pl.paymentPubKey

walletStakingPK :: Wallet -> Maybe Pl.PubKey
walletStakingPK = fmap Pl.toPublicKey . walletStakingSK

walletPKHash :: Wallet -> Pl.PubKeyHash
walletPKHash = Pl.pubKeyHash . walletPK

walletStakingPKHash :: Wallet -> Maybe Pl.PubKeyHash
walletStakingPKHash = fmap Pl.pubKeyHash . walletStakingPK

walletAddress :: Wallet -> Pl.Address
walletAddress w =
  Pl.Address
    (Pl.PubKeyCredential $ walletPKHash w)
    (Pl.StakingHash . Pl.PubKeyCredential <$> walletStakingPKHash w)

walletSK :: Pl.MockWallet -> PrivateKey
walletSK = Pl.unPaymentPrivateKey . Pl.paymentPrivateKey

-- Massive hack to be able to open a MockPrivateKey; this is needed because
-- the constructor and accessor to MockPrivateKey are not exported. Hence,
-- we make an isomorphic datatype, unsafeCoerce to this datatype then extract
-- whatever we need from it.
newtype HACK = HACK Cardano.XPrv

walletStakingSK :: Wallet -> Maybe PrivateKey
walletStakingSK = fmap hackUnMockPrivateKey . Pl.mwStakeKey
  where
    -- Don't use this; its a hack and will be deprecated once we have time
    -- to make a PR into plutus exporting the things we need. If you use this anyway,
    -- make sure that you only apply it to @MockPrivateKey@; the function is polymorphic
    -- because @MockPrivateKey@ is not exported either; having a dedicated function makes
    -- it easy to test that this works: check the @Cooked.WalletSpec@ test module.
    hackUnMockPrivateKey :: a -> Cardano.XPrv
    hackUnMockPrivateKey x = let HACK y = unsafeCoerce x in y

toPKHMap :: [Wallet] -> Map Pl.PubKeyHash Wallet
toPKHMap ws = Map.fromList [(walletPKHash w, w) | w <- ws]

-- * Signs a transaction

txAddSignature :: Wallet -> Pl.Tx -> Pl.Tx
txAddSignature w = Pl.addSignature' (walletSK w)

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
newtype InitialDistribution = InitialDistribution {distribution :: Map Wallet [Pl.Value]}
  deriving (Eq, Show)

instance Semigroup InitialDistribution where
  (InitialDistribution i) <> (InitialDistribution j) = InitialDistribution $ Map.unionWith (<>) i j

instance Monoid InitialDistribution where
  mempty = InitialDistribution Map.empty

instance Default InitialDistribution where
  def = InitialDistribution $ Map.fromList $ zip knownWallets (repeat $ replicate 10 defLovelace)
    where
      defLovelace = Pl.lovelaceValueOf 100_000_000

-- | Ensures the distribution is valid by adding any missing Ada to all utxos.
distributionFromList :: [(Wallet, [Pl.Value])] -> InitialDistribution
distributionFromList = InitialDistribution . Map.fromList

-- | Extension of the default initial distribution with additional value in
-- some wallets.
initialDistribution' :: [(Wallet, [Pl.Value])] -> InitialDistribution
initialDistribution' = (def <>) . distributionFromList

initialTxFor :: InitialDistribution -> Pl.Tx
initialTxFor initDist =
  mempty
    { Pl.txMint = mconcat (map (mconcat . snd) initDist'),
      Pl.txOutputs = concatMap (\(w, vs) -> map (initUtxosFor w) vs) initDist'
    }
  where
    -- initUtxosFor w v = Pl.TxOut $ Api.TxOut addr val Api.TxOutDatumNone Api.ReferenceScriptNone
    initUtxosFor w v = toPlTxOut @() (walletAddress w) v Nothing

    initDist' = Map.toList $ distribution initDist

    toPlTxOut :: Pl.ToData a => Pl.Address -> Pl.Value -> Maybe a -> Pl.TxOut
    toPlTxOut addr value datum = toPlTxOut' addr value datum'
      where
        datum' = maybe Pl2.NoOutputDatum (Pl2.OutputDatumHash . Pl.datumHash . Pl.Datum . Pl.toBuiltinData) datum

    toPlTxOut' :: Pl.Address -> Pl.Value -> Pl2.OutputDatum -> Pl.TxOut
    toPlTxOut' addr value datum = Pl.TxOut $ toCardanoTxOut' addr value datum

    toCardanoTxOut' :: Pl.Address -> Pl.Value -> Pl2.OutputDatum -> Cardano.TxOut Cardano.CtxTx Cardano.BabbageEra
    toCardanoTxOut' addr value datum = Cardano.TxOut cAddr cValue cDatum Cardano.ReferenceScriptNone
      where
        fromRight' x = case x of
          Left err -> error $ show err
          Right res -> res
        cAddr = fromRight' $ Pl.toCardanoAddressInEra theNetworkId addr
        cValue = fromRight' $ Pl.toCardanoTxOutValue value
        cDatum = fromRight' $ Pl.toCardanoTxOutDatum datum

    theNetworkId :: Cardano.NetworkId
    theNetworkId = Cardano.Testnet $ Cardano.NetworkMagic 42 -- TODO PORT what's magic?

valuesForWallet :: InitialDistribution -> Wallet -> [Pl.Value]
valuesForWallet d w = fromMaybe [] $ w `Map.lookup` distribution d
