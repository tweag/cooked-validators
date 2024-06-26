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
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

type MintGen a = TxGen (Map Api.TxOutRef Api.TxOut) a

-- | Converts the 'TxSkelMints' into a 'TxMintValue'
toMintValue :: TxSkelMints -> MintGen (Cardano.TxMintValue Cardano.BuildTx Cardano.ConwayEra)
toMintValue mints =
  if null mints
    then return Cardano.TxMintNone
    else do
      let mintValue = txSkelMintsValue mints
      mintVal <-
        throwOnToCardanoError
          ("toMintValue: Unable to translate minted value " <> show mintValue)
          (Ledger.toCardanoValue mintValue)
      (Map.fromList -> witnessMap) <-
        forM (txSkelMintsToList mints) $
          \(policy, redeemer, _, _) -> do
            policyId <-
              throwOnToCardanoError
                "toMintValue: Unable to translate minting policy hash"
                (Ledger.toCardanoPolicyId (Script.mintingPolicyHash policy))
            mintWitness <- toScriptWitness policy redeemer Cardano.NoScriptDatumForMint
            return (policyId, mintWitness)
      return $ Cardano.TxMintValue Cardano.MaryEraOnwardsConway mintVal (Cardano.BuildTxWith witnessMap)
