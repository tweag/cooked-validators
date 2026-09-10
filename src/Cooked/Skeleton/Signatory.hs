{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes the notion of signatory for out 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Signatory
  ( -- * Data types
    TxSkelSignatoryWitness (..),
    TxSkelSignatory (..),

    -- * Optics
    txSkelSignatoryPubKeyHashL,
    txSkelSignatoryWitnessSigningKeyAF,
    txSkelSignatoryPrivateKeyL,
    txSkelSignatorySigningKeyAF,

    -- * Smart constructors
    signatoryPubKey,
    signatoryWallet,
    txSkelSignatoriesFromList,
    signatoryFromFile,
  )
where

import Cardano.Api qualified as Cardano
import Control.Monad.Extra (eitherM)
import Cooked.Skeleton.Output
import Cooked.Skeleton.User
import Cooked.Utilities.Wallet
import Ledger.Address qualified as P.Ledger
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Builtins qualified as PlutusTx

-- | A possible witness for a signatory in a skeleton.
data TxSkelSignatoryWitness where
  NoTxSkelSignatoryWitness :: TxSkelSignatoryWitness
  SomeTxSkelSignatoryWitness :: (P.Ledger.ToWitness sKey) => sKey -> TxSkelSignatoryWitness

-- | Focuses on the optional signing key of a 'TxSkelSignatoryWitness'
txSkelSignatoryWitnessSigningKeyAF :: AffineFold TxSkelSignatoryWitness Cardano.ShelleyWitnessSigningKey
txSkelSignatoryWitnessSigningKeyAF =
  afolding $ \case
    NoTxSkelSignatoryWitness -> Nothing
    (SomeTxSkelSignatoryWitness skey) -> Just $ P.Ledger.toWitness skey

-- | Signatories in skeletons
data TxSkelSignatory where
  TxSkelSignatory ::
    (Script.ToPubKeyHash pkh, Show pkh) =>
    { -- | Identifying the signatory with their pubkey hash
      txSkelSignatoryPubKeyHash :: pkh,
      -- | The private key with which this signatory should sign. If set to
      -- @Nothing@ the signature won't be added (but will be needed later on).
      txSkelSignatoryPrivateKey :: TxSkelSignatoryWitness
    } ->
    TxSkelSignatory

-- | Focuses on the private key of a 'TxSkelSignatory'
makeLensesFor [("txSkelSignatoryPrivateKey", "txSkelSignatoryPrivateKeyL")] ''TxSkelSignatory

-- | Focuses on the public key hash of a 'TxSkelSignatory'
txSkelSignatoryPubKeyHashL :: Lens' TxSkelSignatory Api.PubKeyHash
txSkelSignatoryPubKeyHashL =
  lens
    (\(TxSkelSignatory pkh _) -> Script.toPubKeyHash pkh)
    (\sig pkh -> sig {txSkelSignatoryPubKeyHash = pkh})

-- | Focuses on the optional signing key of a 'TxSkelSignatory'
txSkelSignatorySigningKeyAF :: AffineFold TxSkelSignatory Cardano.ShelleyWitnessSigningKey
txSkelSignatorySigningKeyAF = txSkelSignatoryPrivateKeyL % txSkelSignatoryWitnessSigningKeyAF

instance Show TxSkelSignatory where
  show (TxSkelSignatory pkh NoTxSkelSignatoryWitness) = "Only pubkey hash: " <> show pkh
  show (TxSkelSignatory pkh (SomeTxSkelSignatoryWitness _)) = "Pubkey hash: " <> show pkh <> " accompanied by a private key."

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
    (SomeTxSkelSignatoryWitness $ P.Ledger.PaymentPrivateKey $ walletSK w)

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
            (SomeTxSkelSignatoryWitness skey)
    )
    (Cardano.readFileTextEnvelope @(Cardano.SigningKey keyRole) (Cardano.File file))

-- | Builds a signatory from a pubkey, which will no be able to actually sign
-- the transaction, but will act as a requirement.
signatoryPubKey :: (Script.ToPubKeyHash pkh, Show pkh) => pkh -> TxSkelSignatory
signatoryPubKey = (`TxSkelSignatory` NoTxSkelSignatoryWitness)

-- | Builds a list of signatories from a list of wallets
txSkelSignatoriesFromList :: [Wallet] -> [TxSkelSignatory]
txSkelSignatoriesFromList = map signatoryWallet
