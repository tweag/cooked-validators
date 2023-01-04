{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Various Optics on 'TxSkels' and all the other types defined in
-- 'Cooked.Tx.Constraints.Type'.
module Cooked.Tx.Constraints.Optics where

-- import Cooked.Tx.Constraints.Type
-- import qualified Data.Map as Map
-- import qualified Ledger as Pl
-- import qualified Ledger.Typed.Scripts as Pl
-- import Optics.Core
-- import Type.Reflection

-- -- * Working with transaction outputs

-- -- | Does the given 'TxSkelOut' an output to a validator of the provided type?
-- -- The type of this prism is ambiguous, so you might need to type-apply it.
-- txOutScriptTypeP ::
--   forall a.
--   PaysScriptConstrs a =>
--   Prism'
--     TxSkelOut
--     ( Pl.TypedValidator a,
--       Maybe Pl.StakingCredential,
--       Pl.DatumType a,
--       Pl.Value
--     )
-- txOutScriptTypeP =
--   prism'
--     (\(validator, mStCred, datum, value) -> PaysScript validator mStCred datum value)
--     ( \case
--         PaysScript validator mStCred datum value ->
--           case typeOf validator `eqTypeRep` typeRep @(Pl.TypedValidator a) of
--             Just HRefl -> Just (validator, mStCred, datum, value)
--             Nothing -> Nothing
--         _ -> Nothing
--     )

-- -- | Go through all transaction outputs that pay a typed validator of a certain
-- -- type. This traversal has an ambiguous type, so you might have to type-apply
-- -- it.
-- paysScriptTypeT ::
--   PaysScriptConstrs a =>
--   Traversal'
--     TxSkel
--     ( Pl.TypedValidator a,
--       Maybe Pl.StakingCredential,
--       Pl.DatumType a,
--       Pl.Value
--     )
-- paysScriptTypeT = txSkelOutsL % traversed % txOutScriptTypeP

-- -- | Does the given 'TxSkelOut' go to a public key, and does it have no datum?
-- paysPKNoDatumP :: Prism' TxSkelOut (Pl.PubKeyHash, Maybe Pl.StakePubKeyHash, Pl.Value)
-- paysPKNoDatumP =
--   prism'
--     (\(pkh, mStPkh, value) -> PaysPK pkh mStPkh (Nothing @()) value)
--     ( \case
--         PaysPK pkh mStPkh Nothing value -> Just (pkh, mStPkh, value)
--         _ -> Nothing
--     )

-- -- | Go through all transaction outputs that pay a plublic key without using a datum.
-- paysPKNoDatumT :: Traversal' TxSkel (Pl.PubKeyHash, Maybe Pl.StakePubKeyHash, Pl.Value)
-- paysPKNoDatumT = txSkelOutsL % traversed % paysPKNoDatumP

-- -- * Working with transaction inputs

-- -- | Does the given transaction input (as a pair of 'SpendbleOut' and
-- -- 'TxSkelIn') come from a script of a specific type? This prism has an
-- -- ambiguous type, you might need to type-apply it.
-- spendsScriptTypeP ::
--   forall a.
--   SpendsScriptConstrs a =>
--   Prism' (SpendableOut, TxSkelIn) (SpendableOut, Pl.TypedValidator a, Pl.RedeemerType a)
-- spendsScriptTypeP =
--   prism'
--     (\(sOut, validator, redeemer) -> (sOut, SpendsScript validator redeemer))
--     ( \(sOut, txSkelIn) ->
--         case txSkelIn of
--           SpendsScript validator redeemer ->
--             case typeOf validator `eqTypeRep` typeRep @(Pl.TypedValidator a) of
--               Just HRefl -> Just (sOut, validator, redeemer)
--               Nothing -> Nothing
--           _ -> Nothing
--     )

-- -- | Go through all transaction inputs that come from some script of a given
-- -- type. The type of this traversal is ambiguous, you might need to type-apply.
-- spendsScriptTypeF ::
--   SpendsScriptConstrs a =>
--   Fold TxSkel (SpendableOut, Pl.TypedValidator a, Pl.RedeemerType a)
-- spendsScriptTypeF = txSkelInsL % folding Map.toList % spendsScriptTypeP
