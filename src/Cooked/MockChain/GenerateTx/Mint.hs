module Cooked.MockChain.GenerateTx.Mint
  ( toMintValue,
  )
where

import Cardano.Api qualified as Cardano
import Control.Monad
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty qualified as NEMap
import Data.Map.Strict qualified as SMap
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Builtins.Internal qualified as PlutusTx
import Test.QuickCheck.Modifiers (NonZero (NonZero))

type MintGen a = TxGen (Map Api.TxOutRef Api.TxOut) a

-- | Converts the 'TxSkelMints' into a 'TxMintValue'
toMintValue :: TxSkelMints -> MintGen (Cardano.TxMintValue Cardano.BuildTx Cardano.ConwayEra)
toMintValue mints | null mints = return Cardano.TxMintNone
toMintValue mints = fmap (Cardano.TxMintValue Cardano.MaryEraOnwardsConway . SMap.fromList) $
  forM (Map.toList mints) $ \(policy, (red, assets)) -> do
    policyId <-
      throwOnToCardanoError
        "toMintValue: Unable to translate minting policy hash"
        (Ledger.toCardanoPolicyId $ Script.toMintingPolicyHash policy)
    assetsMinted <- forM (Map.toList $ NEMap.toMap assets) $ \(Api.TokenName (PlutusTx.BuiltinByteString name), NonZero quantity) -> do
      mintWitness <- toScriptWitness policy red Cardano.NoScriptDatumForMint
      return (Cardano.AssetName name, Cardano.Quantity quantity, Cardano.BuildTxWith mintWitness)
    return (policyId, assetsMinted)
