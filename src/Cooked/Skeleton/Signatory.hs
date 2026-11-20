{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes the notion of signatory for out 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Signatory
  ( -- * Data types
    TxSkelSignatory (..),

    -- * Optics
    txSkelSignatoryPubKeyHashL,
    txSkelSignatoryPrivateKeyAT,
    txSkelSignatoryPrivateKeyL,

    -- * Smart constructors
    signatoryPubKey,
    signatoryWallet,
    signatoryPoolCold,
    txSkelSignatoriesFromList,
    signatoryFromFile,
  )
where

import Cardano.Api qualified as Cardano
import Control.Monad.Extra (eitherM)
import Cooked.Skeleton.Output
import Cooked.Skeleton.User
import Cooked.Utilities.Codec
import Cooked.Utilities.Pools
import Cooked.Utilities.Wallet
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Address qualified as P.Ledger
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Builtins qualified as PlutusTx

-- | Signatories in skeletons. Their JSON serialization currently includes
-- their private key in plain text, if any.
data TxSkelSignatory where
  TxSkelSignatory ::
    { -- | Identifying the signatory with their pubkey hash
      txSkelSignatoryPubKeyHash :: Api.PubKeyHash,
      -- | The private key with which this signatory should sign. If set to
      -- @Nothing@ the signature won't be added (but will be needed later on).
      txSkelSignatoryPrivateKey :: Maybe Cardano.ShelleyWitnessSigningKey
    } ->
    TxSkelSignatory
  deriving (Generic, FromJSON, ToJSON)

instance HasTypeKey TxSkelSignatory where
  typeKey = "TxSkelSignatory"

-- | Focuses on the private key of a 'TxSkelSignatory'
makeLensesFor [("txSkelSignatoryPrivateKey", "txSkelSignatoryPrivateKeyL")] ''TxSkelSignatory

-- | Focuses on the public key hash of a 'TxSkelSignatory'
makeLensesFor [("txSkelSignatoryPubKeyHash", "txSkelSignatoryPubKeyHashL")] ''TxSkelSignatory

-- | Focuses on the optional signing key of a 'TxSkelSignatory'
txSkelSignatoryPrivateKeyAT :: AffineTraversal' TxSkelSignatory Cardano.ShelleyWitnessSigningKey
txSkelSignatoryPrivateKeyAT = txSkelSignatoryPrivateKeyL % _Just

instance Show TxSkelSignatory where
  show (TxSkelSignatory pkh Nothing) = "Only pubkey hash: " <> show pkh
  show (TxSkelSignatory pkh _) = "Pubkey hash: " <> show pkh <> " accompanied by a private key."

-- There is no Eq instance for the private key, so we only compare the pubkey
-- hashes and the length of the private keys (if they are present). This should
-- be sound because the pkh should correspond to the private key.
instance Eq TxSkelSignatory where
  (TxSkelSignatory pkh _) == (TxSkelSignatory pkh1 _) =
    Script.toPubKeyHash pkh == Script.toPubKeyHash pkh1

instance Script.ToPubKeyHash TxSkelSignatory where
  toPubKeyHash = view txSkelSignatoryPubKeyHashL

instance IsTxSkelOutAllowedOwner TxSkelSignatory where
  toPKHOrVScript = UserPubKey . view txSkelSignatoryPubKeyHashL

-- | Builds a signatory from a wallet, which will be able to actually sign the
-- transaction.
signatoryWallet :: Wallet -> TxSkelSignatory
signatoryWallet w =
  TxSkelSignatory
    (Script.toPubKeyHash w)
    (Just $ P.Ledger.toWitness $ P.Ledger.PaymentPrivateKey $ walletSK w)

-- | Builds a signatory from a pool's cold key, which can sign pool certificates.
signatoryPoolCold :: Pool -> TxSkelSignatory
signatoryPoolCold p =
  TxSkelSignatory (poolId p) (Just $ Cardano.WitnessStakePoolKey $ poolColdSK p)

instance P.Ledger.ToWitness (Cardano.SigningKey Cardano.GenesisUTxOKey) where
  toWitness = Cardano.WitnessGenesisUTxOKey

-- | Builds a signatory from a signing key file.
signatoryFromFile ::
  forall keyRole.
  ( Cardano.Key keyRole,
    Cardano.HasTypeProxy keyRole,
    P.Ledger.ToWitness (Cardano.SigningKey keyRole)
  ) =>
  FilePath ->
  IO TxSkelSignatory
signatoryFromFile file =
  eitherM
    (fail . ("Failed to read signing key from file: " ++) . show)
    ( \skey ->
        return $
          TxSkelSignatory
            ( Api.PubKeyHash $
                PlutusTx.toBuiltin $
                  Cardano.serialiseToRawBytes $
                    Cardano.verificationKeyHash $
                      Cardano.getVerificationKey skey
            )
            (Just $ P.Ledger.toWitness skey)
    )
    (Cardano.readFileTextEnvelope @(Cardano.SigningKey keyRole) (Cardano.File file))

-- | Builds a signatory from a pubkey, which will no be able to actually sign
-- the transaction, but will act as a requirement.
signatoryPubKey :: (Script.ToPubKeyHash pkh) => pkh -> TxSkelSignatory
signatoryPubKey = (`TxSkelSignatory` Nothing) . Script.toPubKeyHash

-- | Builds a list of signatories from a list of wallets
txSkelSignatoriesFromList :: [Wallet] -> [TxSkelSignatory]
txSkelSignatoriesFromList = map signatoryWallet
