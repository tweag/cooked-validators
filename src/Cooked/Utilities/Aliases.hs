-- | This module exposes some type aliases common to our library
module Cooked.Utilities.Aliases
  ( -- * Type aliases
    Fee,
    CollateralIns,
    Collaterals,
    Utxo,
    Utxos,
    UtxoSearchResult,
    BodyContent,
    Body,
    Transaction,
    SubmissionFailures,
    ExUnitsFailures,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Alonzo.Plutus.Evaluate qualified as Alonzo
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Conway.Rules qualified as Conway
import Cooked.Skeleton.Output
import Cooked.Utilities.TypedSearch
import Data.Map (Map)
import Data.Set (Set)
import PlutusLedgerApi.V3 qualified as Api

-- * Type aliases

-- | An alias for Integers used as fees
type Fee = Integer

-- | An alias for sets of utxos used as collateral inputs
type CollateralIns = Set Api.TxOutRef

-- | An alias for optional pairs of collateral inputs and optional return
-- collateral output
type Collaterals = (CollateralIns, Maybe TxSkelOut)

-- | An alias for an output and its reference
type Utxo = (Api.TxOutRef, TxSkelOut)

-- | An alias for Maps of 'TxSkelOut' with 'Api.TxOutRef' as keys
type Utxos = Map Api.TxOutRef TxSkelOut

-- | An alias for searches returning a 'TxSkelOut' within maps with
-- 'Api.TxOutRef' as keys
type UtxoSearchResult = SearchResult (Map Api.TxOutRef) '[TxSkelOut]

-- | An alias for a transaction body content
type BodyContent = Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra

-- | An alias for a transaction body
type Body = Cardano.TxBody Cardano.ConwayEra

-- | An alias for errors occurring when computing execution units. These contain
-- Phase2 failures, but also errors uncovered when building a proper context to
-- execute the scripts.
type ExUnitsFailures = Map Cardano.ScriptWitnessIndex (Alonzo.TransactionScriptFailure Conway.ConwayEra)

-- | An alias for errors occurring at submission
type SubmissionFailures = [Conway.ConwayLedgerPredFailure Conway.ConwayEra]

-- | An alias for a Cardano transaction
type Transaction = Cardano.Tx Cardano.ConwayEra
