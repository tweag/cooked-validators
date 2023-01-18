{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Various Optics on 'TxSkels' and all the other types defined in
-- 'Cooked.Tx.Constraints.Type'.
module Cooked.Tx.Constraints.Optics where

import Cooked.Tx.Constraints.Type
import qualified Ledger.Value as Pl
import Optics.Core
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import Type.Reflection

-- | Decide if a transaction output has a certain owner and datum type.
txSkelOutOwnerTypeP ::
  forall ownerType.
  ( ToCredential ownerType,
    Show ownerType,
    IsTxSkelOutAllowedOwner ownerType,
    Typeable ownerType
  ) =>
  Prism' TxSkelOut (ConcreteOutput ownerType TxSkelOutDatum Pl.Value (Pl.Versioned Pl.Script))
txSkelOutOwnerTypeP =
  prism'
    Pays
    ( \(Pays output) ->
        let owner = output ^. outputOwnerL
         in case typeOf owner `eqTypeRep` typeRep @ownerType of
              Just HRefl ->
                Just $
                  ConcreteOutput
                    owner
                    (output ^. outputStakingCredentialL)
                    (output ^. outputValueL)
                    (output ^. outputDatumL)
                    (toScript <$> output ^. outputReferenceScriptL)
              Nothing -> Nothing
    )

txSkelOutputDatumTypeAT ::
  (Pl.FromData a, Typeable a) =>
  AffineTraversal' TxSkelOut a
txSkelOutputDatumTypeAT =
  atraversal
    ( \txSkelOut -> case txSkelOutDatumComplete txSkelOut of
        Nothing -> Left txSkelOut
        Just (Pl.Datum datum, _) -> case Pl.fromBuiltinData datum of
          Just tyDatum -> Right tyDatum
          Nothing -> Left txSkelOut
    )
    ( \(Pays output) newTyDatum ->
        Pays $
          over
            outputDatumL
            ( \case
                TxSkelOutNoDatum -> TxSkelOutNoDatum
                TxSkelOutDatum tyDatum -> TxSkelOutDatum $ replaceDatumOnCorrectType tyDatum newTyDatum
                TxSkelOutDatumHash tyDatum -> TxSkelOutDatumHash $ replaceDatumOnCorrectType tyDatum newTyDatum
                TxSkelOutInlineDatum tyDatum -> TxSkelOutInlineDatum $ replaceDatumOnCorrectType tyDatum newTyDatum
            )
            output
    )
  where
    replaceDatumOnCorrectType :: (Typeable b, Typeable a) => b -> a -> b
    replaceDatumOnCorrectType old new = case typeOf old `eqTypeRep` typeOf new of
      Just HRefl -> new
      Nothing -> old
