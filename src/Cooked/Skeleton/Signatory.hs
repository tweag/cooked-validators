-- | This module exposes the notion of signer for out 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Signatory
  ( -- * Data types
    TxSkelSignatory (..),

    -- * Optics
    txSkelSignatoryMPrivateKeyL,
    txSkelSignatoryPrivateKeyAT,
    txSkelSignatoryPubKeyHashL,

    -- * Smart constructors
    signerWallet,
    signerPubKey,
    txSkelSignatoriesFromList,
  )
where

import Cardano.Crypto.Wallet qualified as Crypto
import Cooked.Wallet
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Signatories in skeletons
data TxSkelSignatory where
  TxSkelSignatory ::
    (Script.ToPubKeyHash pkh, Show pkh) =>
    { -- | Identifying the signer with their pubkey hash
      txSkelSignatoryPubKeyHash :: pkh,
      -- | The private key with which this signer should sign. If set to
      -- @Nothing@ the signature won't be added (but will be needed later on).oiu clairement
      txSkelSignatoryPrivateKey :: Maybe Crypto.XPrv
    } ->
    TxSkelSignatory

-- | A lens focusing on the option private key for this signatory
makeLensesFor [("txSkelSignatoryPrivateKey", "txSkelSignatoryMPrivateKeyL")] ''TxSkelSignatory

-- | An affine traversal focusing on the existing private key for this signatory
txSkelSignatoryPrivateKeyAT :: AffineTraversal' TxSkelSignatory Crypto.XPrv
txSkelSignatoryPrivateKeyAT = txSkelSignatoryMPrivateKeyL % _Just

-- | A lens focusing on the public key hash of this signatory
txSkelSignatoryPubKeyHashL :: Lens' TxSkelSignatory Api.PubKeyHash
txSkelSignatoryPubKeyHashL =
  lens
    (\(TxSkelSignatory pkh _) -> Script.toPubKeyHash pkh)
    (\sig pkh -> sig {txSkelSignatoryPubKeyHash = pkh})

instance Show TxSkelSignatory where
  show (TxSkelSignatory pkh Nothing) = "Only pubkey hash: " <> show pkh
  show (TxSkelSignatory pkh (Just _)) = "Pubkey hash: " <> show pkh <> " accompanied by a private key."

instance Eq TxSkelSignatory where
  (TxSkelSignatory pkh sk) == (TxSkelSignatory pkh1 sk1) =
    Script.toPubKeyHash pkh == Script.toPubKeyHash pkh1
      && (fmap Crypto.unXPrv sk == fmap Crypto.unXPrv sk1)

-- | Builds a signatory from a wallet, which will be able to actually sign the
-- transaction.
signerWallet :: Wallet -> TxSkelSignatory
signerWallet w = TxSkelSignatory (Script.toPubKeyHash w) (Just $ walletSK w)

-- | Builds a signatory from a pubkey, which will no be able to actually sign
-- the transaction, but will act as a requirement.
signerPubKey :: Api.PubKeyHash -> TxSkelSignatory
signerPubKey = (`TxSkelSignatory` Nothing)

-- | Builds a list of signers from a list of wallets
txSkelSignatoriesFromList :: [Wallet] -> [TxSkelSignatory]
txSkelSignatoriesFromList = map signerWallet
