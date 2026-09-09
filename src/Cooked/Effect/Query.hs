{-# OPTIONS_GHC -Wno-deprecations #-}

-- | This module exposes the user-facing primitives to query the current state
-- of the blockchain, such as the available UTxOs, and the current constitution
-- or rewards.
module Cooked.Effect.Query
  ( -- * The 'Query' effect and interpreters
    Query,
    runMockChainQuery,
    runBlockChainQuery,

    -- * Queries related to `Cooked.Skeleton.TxSkel`
    txSkelAllScripts,
    txSkelInputScripts,
    txSkelInputValue,

    -- * Queries related to fetching UTxOs
    allUtxos,
    utxosAt,
    txSkelOutByRef,
    utxosFromCardanoTx,
    utxosFromRefs,
    previewByRef,
    viewByRef,

    -- * Query fetching the current reward amount
    getCurrentReward,

    -- * Query fetching the current full constitution script
    getConstitutionScript,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano hiding (TxIn)
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Cooked.Automation.GenerateTx.Credential
import Cooked.Effect.Params
import Cooked.Runtime.Error
import Cooked.Runtime.State
import Cooked.Skeleton
import Cooked.Utilities.HList
import Cooked.Utilities.TypedSearch
import Data.Coerce (coerce)
import Data.Map qualified as Map
import Data.Maybe
import Data.Maybe.Strict
import Data.Set qualified as Set
import Ledger.Address qualified as P.Ledger
import Ledger.Tx qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State

-- | An effect that offers primitives to query the current state of the
-- mockchain. As its name suggests, this effect is read-only and does not alter
-- the state in any way. This is the user-facing read effect; its interpreters
-- rely on the internal
-- 'Cooked.Effect.Params.Params' effect to resolve the
-- fixed chain configuration.
data Query :: Effect where
  TxSkelOutByRef :: Api.TxOutRef -> Query m TxSkelOut
  AllUtxos :: Query m (SearchResult Api.TxOutRef '[TxSkelOut])
  UtxosAt :: (Script.ToAddress a) => a -> Query m (SearchResult Api.TxOutRef '[TxSkelOut])
  GetConstitutionScript :: Query m (Maybe VScript)
  GetCurrentReward :: (Script.ToCredential c) => c -> Query m (Maybe Api.Lovelace)

makeSem_ ''Query

-- | Returns all scripts involved in this 'TxSkel'
txSkelAllScripts ::
  (Member Query effs) =>
  TxSkel ->
  Sem effs [VScript]
txSkelAllScripts txSkel = do
  txSkelSpendingScripts <- txSkelInputScripts txSkel
  return $
    toListOf (txSkelRedeemedScriptsT % userVScriptL) txSkel
      <> txSkelSpendingScripts

-- | Returns all scripts which guard transaction inputs
txSkelInputScripts ::
  (Member Query effs) =>
  TxSkel ->
  Sem effs [VScript]
txSkelInputScripts =
  fmap catMaybes
    . mapM (previewByRef (txSkelOutOwnerL % userVScriptAT))
    . Map.keys
    . txSkelInputs

-- | look up the UTxOs the transaction consumes, and sum their values.
txSkelInputValue ::
  (Member Query effs) =>
  TxSkel ->
  Sem effs Api.Value
txSkelInputValue =
  fmap mconcat
    . mapM (viewByRef txSkelOutValueL)
    . Map.keys
    . txSkelInputs

-- | Returns a list of all currently known outputs
allUtxos ::
  (Member Query effs) =>
  Sem effs (SearchResult Api.TxOutRef '[TxSkelOut])

-- | Returns a list of all UTxOs at a certain address.
utxosAt ::
  ( Member Query effs,
    Script.ToAddress cred
  ) =>
  cred ->
  Sem effs (SearchResult Api.TxOutRef '[TxSkelOut])

-- | Returns an output given a reference to it
txSkelOutByRef ::
  (Member Query effs) =>
  Api.TxOutRef ->
  Sem effs TxSkelOut

-- | Retrieves the ordered list of outputs of the given "CardanoTx".
--
-- This is useful when writing endpoints and/or traces to fetch utxos of
-- interest right from the start and avoid querying the chain for them
-- afterwards using 'allUtxos' or similar functions.
utxosFromCardanoTx ::
  (Member Query effs) =>
  P.Ledger.CardanoTx ->
  Sem effs (SearchResult Api.TxOutRef '[TxSkelOut])
utxosFromCardanoTx =
  utxosFromRefs
    . fmap (P.Ledger.fromCardanoTxIn . snd)
    . P.Ledger.getCardanoTxOutRefs

-- | Go through all of the 'Api.TxOutRef's in the list and look them up in the
-- state of the blockchain, throwing an error if one of them cannot be resolved.
utxosFromRefs ::
  ( Foldable f,
    Member Query effs
  ) =>
  f Api.TxOutRef ->
  Sem effs (SearchResult Api.TxOutRef '[TxSkelOut])
utxosFromRefs =
  foldM
    (\m oRef -> flip (Map.insert oRef) m . hSingleton <$> txSkelOutByRef oRef)
    Map.empty

-- | Retrieves an output and views a specific element out of it
viewByRef ::
  ( Member Query effs,
    Is g A_Getter
  ) =>
  Optic' g is TxSkelOut c ->
  Api.TxOutRef ->
  Sem effs c
viewByRef optic = (view optic <$>) . txSkelOutByRef

-- | Retrieves an output and previews a specific element out of it
previewByRef ::
  ( Member Query effs,
    Is af An_AffineFold
  ) =>
  Optic' af is TxSkelOut c ->
  Api.TxOutRef ->
  Sem effs (Maybe c)
previewByRef optic = (preview optic <$>) . txSkelOutByRef

-- | Gets the current official constitution script
getConstitutionScript ::
  (Member Query effs) =>
  Sem effs (Maybe VScript)

-- | Gets the current reward associated with a credential
getCurrentReward ::
  ( Member Query effs,
    Script.ToCredential c
  ) =>
  c ->
  Sem effs (Maybe Api.Lovelace)

-- | The interpretation for read-only effect with a stored 'EmulatorState' and
-- 'ChainIndex'
runMockChainQuery ::
  forall effs a.
  ( Members
      '[ State EmulatorState,
         State ChainIndex,
         Error P.Ledger.ToCardanoError,
         Error ChainError
       ]
      effs
  ) =>
  Sem (Query : effs) a ->
  Sem effs a
runMockChainQuery = interpret $ \case
  TxSkelOutByRef oRef -> do
    res <- gets $ Map.lookup oRef . chainIndexOutputs
    case res of
      Just (txSkelOut, True) -> return txSkelOut
      _ -> throw $ CEUnknownOutRef oRef
  AllUtxos -> gets $ extractOutputs $ \_ _ -> True
  UtxosAt (Script.toAddress -> addr) -> gets $ extractOutputs $ \_ -> (== addr) . Script.toAddress
  GetConstitutionScript -> gets $ view chainIndexConstitutionL
  GetCurrentReward (Script.toCredential -> cred) -> do
    stakeCredential <- toStakeCredential cred
    gets $ preview $ emulatorStateLedgerStateL % to (Emulator.getReward stakeCredential) % _Just % to coerce

-- | Interpret the `Query` effect by talking to a deployed node
-- through a `Cardano.LocalNodeConnectInfo` (socket path and network id)
-- provided via a `Reader`, running in a stack featuring @IO@ (via `Embed`). The
-- fixed chain configuration is resolved through the internal
-- 'Cooked.Effect.Params.Params' effect.
runBlockChainQuery ::
  forall effs a.
  ( Members
      '[ Embed IO,
         Params,
         Error Cardano.UnsupportedNtcVersionError,
         Error Cardano.EraMismatch,
         Error Cardano.AcquiringFailure,
         Error P.Ledger.ToCardanoError,
         Error ChainError,
         Reader Cardano.LocalNodeConnectInfo,
         State ChainIndex
       ]
      effs
  ) =>
  Sem (Query : effs) a ->
  Sem effs a
runBlockChainQuery = interpret $ \case
  AllUtxos -> queryUtxosAndHandleErrors Cardano.QueryUTxOWhole
  UtxosAt (Script.toAddress -> addr) -> do
    networkId <- getNetworkId
    (Cardano.AddressInEra _ cAddr) <- fromEither $ P.Ledger.toCardanoAddressInEra networkId addr
    queryUtxosAndHandleErrors $ Cardano.QueryUTxOByAddress $ Set.singleton $ Cardano.toAddressAny cAddr
  TxSkelOutByRef oRef -> do
    txIn <- fromEither $ P.Ledger.toCardanoTxIn oRef
    utxo <- queryUtxosAndHandleErrors $ Cardano.QueryUTxOByTxIn $ Set.singleton txIn
    maybe (throw $ CEUnknownOutRef oRef) (return . hHead) $ Map.lookup oRef utxo
  GetConstitutionScript -> do
    -- We retrieve the official optional script hash of the current constitution
    Cardano.Constitution _ mScriptHash <-
      queryAndHandleErrors $ Cardano.queryConstitution Cardano.ConwayEraOnwardsConway
    -- We retrieve the optional constitution already stored in the chain index
    mStoredConstitution <- gets chainIndexConstitution
    -- We inspect the current option constitution script hash
    case mScriptHash of
      -- There is no official constitution (should not happen). We just set our
      -- own constitution to @Nothing@ accordingly.
      SNothing -> do
        modify' $ set chainIndexConstitutionL Nothing
        return Nothing
      -- There is an official constitution, and it matches the stored one, which
      -- we directly return.
      SJust (Cardano.ScriptHash -> scriptHash)
        | Just storedConstitution <- mStoredConstitution,
          Script.toScriptHash scriptHash == Script.toScriptHash storedConstitution ->
            return $ Just storedConstitution
      -- There is an official constitution, and it does not match the stored one
      -- (it has changed, or it's the first time it's been queried). We fetch
      -- the actual constitution from a reference script at its own address,
      -- where it should live, according to a governance convention. We store
      -- the script we find there after verifying its hash, and return it.
      SJust (Cardano.ScriptHash -> scriptHash) -> do
        networkId <- getNetworkId
        utxo <-
          queryUtxosAndHandleErrors $
            Cardano.QueryUTxOByAddress $
              Set.singleton $
                Cardano.AddressShelley $
                  Cardano.makeShelleyAddress
                    networkId
                    (Cardano.PaymentCredentialByScript scriptHash)
                    Cardano.NoStakeAddress
        let newConstitution =
              listToMaybe $
                [ script
                | (_, preview txSkelOutReferenceScriptAT . hHead -> Just script) <- Map.toList utxo,
                  Script.toScriptHash script == Script.toScriptHash scriptHash
                ]
        modify' $ set chainIndexConstitutionL newConstitution
        return newConstitution
  GetCurrentReward (Script.toCredential -> cred) -> do
    networkId <- getNetworkId
    stakeCred <- toStakeCredential cred
    (rewards, _) <-
      queryAndHandleErrors $
        Cardano.queryStakeAddresses
          Cardano.ShelleyBasedEraConway
          (Set.singleton (Cardano.fromShelleyStakeCredential stakeCred))
          networkId
    return $ Api.Lovelace . Cardano.unCoin <$> Map.lookup (Cardano.StakeAddress (Cardano.toShelleyNetwork networkId) stakeCred) rewards
  where
    -- Fetches the local node info, embeds a query in IO and handles errors
    query q = do
      conn <- ask
      response <- embed $ Cardano.executeLocalStateQueryExpr conn Cardano.VolatileTip q
      fromEither response
    -- Handles one more layer of errors from the response of a query
    queryAndHandleError q = query q >>= fromEither
    -- Handles a second layer of error from the response of a query
    queryAndHandleErrors q = queryAndHandleError q >>= fromEither
    -- Queries the Utxos present on-chain, handling the errors, and returns the
    -- query result in terms of @Utxos@, updated with the known chain index,
    -- updating the chain index with the newly discovered UTxOs in the process.
    queryUtxosAndHandleErrors utxoFilter = do
      -- we execute the query to the node, and handle the errors
      utxo <- queryAndHandleErrors $ Cardano.queryUtxo Cardano.ShelleyBasedEraConway utxoFilter
      -- We convert the UTxOs to our own representation
      let utxo' = Map.mapKeysMonotonic P.Ledger.fromCardanoTxIn $ convertUtxo <$> Cardano.unUTxO utxo
      -- We execute the same query on our local chain index, to remove utxos
      -- that do not exist anymore on-chain
      knownUtxosFromQuery <- case utxoFilter of
        Cardano.QueryUTxOWhole -> gets $ extractOutputs $ \_ _ -> True
        Cardano.QueryUTxOByAddress addrs ->
          let plutusAddrs = P.Ledger.fromCardanoAddressInEra . Cardano.anyAddressInShelleyBasedEra Cardano.ShelleyBasedEraConway <$> Set.toList addrs
           in gets $ extractOutputs (const $ (`elem` plutusAddrs) . view txSkelOutAddressG)
        Cardano.QueryUTxOByTxIn txIns ->
          let plutusIns = P.Ledger.fromCardanoTxIn <$> Set.toList txIns
           in gets $ extractOutputs $ \oRef _ -> oRef `elem` plutusIns
      forM_ (Map.keysSet knownUtxosFromQuery) $ \oRef ->
        -- If the utxo is not present in the query result, it means it has been
        -- spent, and we remove it from the known utxos
        unless (Map.member oRef utxo') $ modify' $ removeOutput oRef
      -- We update the known UTxOs with the newly discovered ones,
      -- leaving untouched the ones that were already known.
      modify' $ over chainIndexOutputsL (`Map.union` ((,True) <$> utxo'))
      -- we return the UTxOs, but we replace the outputs with the known ones, if they exist
      return $ Map.mapWithKey (\oRef txSkelOut -> hSingleton $ maybe txSkelOut hHead $ Map.lookup oRef knownUtxosFromQuery) utxo'
    -- Converts a Cardano TxOut to a TxSkelOut
    convertUtxo :: Cardano.TxOut Cardano.CtxUTxO Cardano.ConwayEra -> TxSkelOut
    convertUtxo (Cardano.TxOut (P.Ledger.toPlutusAddress -> (Api.Address cred stCred)) val dat refScript) =
      TxSkelOut
        (review userCredentialI cred)
        stCred
        ( dat & \case
            Cardano.TxOutDatumNone -> NoTxSkelOutDatum
            Cardano.TxOutDatumHash _ hash ->
              SomeTxSkelOutDatumHash $ Api.DatumHash $ Api.toBuiltin $ Cardano.serialiseToRawBytes hash
            Cardano.TxOutDatumInline _ datum ->
              SomeTxSkelOutDatum (P.Ledger.fromCardanoScriptData datum) Inline
        )
        (P.Ledger.fromCardanoValue $ P.Ledger.fromCardanoTxOutValue val)
        False
        (P.Ledger.fromCardanoReferenceScript refScript)
