-- | This module exposes the generation of a transaction minted value
module Cooked.MockChain.GenerateTx.Mint (toMintValue) where

import Cardano.Api qualified as Cardano
import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Data.Map qualified as Map
import Data.Map.NonEmpty qualified as NEMap
import Data.Map.Strict qualified as SMap
import GHC.Exts (fromList)
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Builtins.Internal qualified as PlutusTx
import Test.QuickCheck.Modifiers (NonZero (NonZero))

-- | Converts a 'TxSkelMints' into a 'Cardano.TxMintValue'
toMintValue :: (MonadBlockChainBalancing m) => TxSkelMints -> m (Cardano.TxMintValue Cardano.BuildTx Cardano.ConwayEra)
toMintValue mints | null mints = return Cardano.TxMintNone
toMintValue mints = fmap (Cardano.TxMintValue Cardano.MaryEraOnwardsConway . SMap.fromList) $
  forM (Map.toList mints) $ \(policy, (red, Map.toList . NEMap.toMap -> assets)) -> do
    policyId <-
      throwOnToCardanoError
        "toMintValue: Unable to translate minting policy hash"
        (Ledger.toCardanoPolicyId $ Script.toMintingPolicyHash $ Script.toScriptHash policy)
    mintWitness <- Cardano.BuildTxWith <$> toScriptWitness policy red Cardano.NoScriptDatumForMint
    return
      ( policyId,
        ( fromList
            [ (Cardano.UnsafeAssetName name, Cardano.Quantity quantity)
            | (Api.TokenName (PlutusTx.BuiltinByteString name), NonZero quantity) <- assets
            ],
          mintWitness
        )
      )
