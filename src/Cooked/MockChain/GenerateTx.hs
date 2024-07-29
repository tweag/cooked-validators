-- | This module provides primitives to translate elements from our skeleton to
-- actual transaction elements, including the transaction itself. Ideally, this
-- module should only export `generateTx` but we need to make visible a few
-- other primitives that will be used in balancing.
module Cooked.MockChain.GenerateTx
  ( GenerateTxError (..),
    generateBodyContent,
    generateTxOut,
    generateTx,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad.Reader
import Cooked.MockChain.GenerateTx.Body
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Output
import Cooked.Skeleton
import Cooked.Wallet
import Data.Map (Map)
import Data.Set (Set)
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Generates a Cardano `TxOut` from a `TxSkelOut`
generateTxOut ::
  -- | The network Id
  Cardano.NetworkId ->
  -- | The output to translate
  TxSkelOut ->
  Either GenerateTxError (Cardano.TxOut Cardano.CtxTx Cardano.ConwayEra)
generateTxOut networkId txSkelOut = runReaderT (toCardanoTxOut txSkelOut) networkId

-- | Generates a transaction body for a skeleton
generateBodyContent ::
  -- | fee to apply to body generation
  Integer ->
  -- | parameters of the emulator
  Emulator.Params ->
  -- | datums present in our environment
  Map Api.DatumHash Api.Datum ->
  -- | txouts present in our environment
  Map Api.TxOutRef Api.TxOut ->
  -- | validators present in our environment
  Map Script.ValidatorHash (Script.Versioned Script.Validator) ->
  -- | Possible collaterals to use
  Maybe (Set Api.TxOutRef, Wallet) ->
  -- | The skeleton to translate
  TxSkel ->
  Either GenerateTxError (Cardano.TxBodyContent Cardano.BuildTx Cardano.ConwayEra)
generateBodyContent fee params managedData managedTxOuts managedValidators mCollaterals =
  flip runReaderT TxContext {..} . txSkelToBodyContent

-- | Generates a transaction from a skeleton. Shares the same parameters as
-- `generateTxOut`. It consists of generating the body and then signing it.
generateTx ::
  -- | fee to apply to body generation
  Integer ->
  -- | parameters of the emulator
  Emulator.Params ->
  -- | datums present in our environment
  Map Api.DatumHash Api.Datum ->
  -- | txouts present in our environment
  Map Api.TxOutRef Api.TxOut ->
  -- | validators present in our environment
  Map Script.ValidatorHash (Script.Versioned Script.Validator) ->
  -- | The collateral inputs and associated collateral wallet
  Maybe (Set Api.TxOutRef, Wallet) ->
  -- | The skeleton to translate
  TxSkel ->
  Either GenerateTxError (Cardano.Tx Cardano.ConwayEra)
generateTx fee params managedData managedTxOuts managedValidators mCollaterals =
  flip runReaderT TxContext {..} . txSkelToCardanoTx
