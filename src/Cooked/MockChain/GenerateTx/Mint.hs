-- | This module exposes the generation of a transaction minted value
module Cooked.MockChain.GenerateTx.Mint (toMintValue) where

import Cardano.Api qualified as Cardano
import Control.Monad
import Cooked.MockChain.Error
import Cooked.MockChain.GenerateTx.Witness
import Cooked.MockChain.Read
import Cooked.Skeleton.Mint
import Cooked.Skeleton.User
import Data.Map qualified as Map
import Data.Map.Strict qualified as SMap
import GHC.Exts (fromList)
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Builtins.Internal qualified as PlutusTx
import Polysemy
import Polysemy.Error

-- | Converts a 'TxSkelMints' into a 'Cardano.TxMintValue'
toMintValue ::
  (Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError] effs) =>
  TxSkelMints ->
  Sem effs (Cardano.TxMintValue Cardano.BuildTx Cardano.ConwayEra)
toMintValue txSkelMints | txSkelMints == mempty = return Cardano.TxMintNone
toMintValue (unTxSkelMints -> mints) = fmap (Cardano.TxMintValue Cardano.MaryEraOnwardsConway . SMap.fromList) $
  forM (Map.toList mints) $ \(policyHash, (UserRedeemedScript policy red, Map.toList -> assets)) -> do
    policyId <- fromEither $ Ledger.toCardanoPolicyId $ Script.toMintingPolicyHash policyHash
    mintWitness <- Cardano.BuildTxWith <$> toScriptWitness policy red Cardano.NoScriptDatumForMint
    return
      ( policyId,
        ( fromList
            [ (Cardano.UnsafeAssetName name, Cardano.Quantity quantity)
              | (Api.TokenName (PlutusTx.BuiltinByteString name), quantity) <- assets
            ],
          mintWitness
        )
      )
