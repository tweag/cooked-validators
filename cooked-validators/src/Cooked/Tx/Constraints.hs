{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Cooked.Tx.Constraints where

import Cooked.MockChain.Misc
import Cooked.Tx.Constraints.Type
import Data.Function
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Ledger.Address as Pl
import qualified Ledger.Tx as Pl
import qualified Ledger.Typed.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl hiding (TxOut, txOutAddress)

-- -- | Generate the 'Pl.TxOut' transaction output associated to a given output
-- -- constraint 'OutConstraint'.
-- outConstraintToTxOut :: OutConstraint -> Pl.TxOut
-- outConstraintToTxOut (PaysPK pkh mStakePkh mDatum value) =
--   toPlTxOut
--     ( Pl.Address
--         (Pl.PubKeyCredential pkh)
--         (Pl.StakingHash . Pl.PubKeyCredential . Pl.unStakePubKeyHash <$> mStakePkh)
--     )
--     value
--     mDatum
-- outConstraintToTxOut (PaysScript validator msc datum value) =
--   let outAddr = appendStakingCredential msc . Pl.scriptHashAddress . Pl.validatorHash $ validator
--    in toPlTxOut
--         outAddr
--         value
--         (Just datum)
--   where
--     appendStakingCredential :: Maybe Pl.StakingCredential -> Pl.Address -> Pl.Address
--     appendStakingCredential Nothing addr = addr
--     appendStakingCredential (Just sc) addr = addr {Pl.addressStakingCredential = Just sc}

-- -- | Reorders the outputs of a transaction according to the ordered list of
-- -- output constraints that generate them. Fails in case of mismatch. The
-- -- reordered outputs are put at the beginning of the list.
-- -- orderTxOutputs :: MonadFail m => [OutConstraint] -> [Pl.TxOut] -> m [Pl.TxOut]
-- -- -- TODO Check staking credentials
-- -- orderTxOutputs [] txOuts = return txOuts
-- -- orderTxOutputs (oc : ocs) txOuts =
-- --   case findGeneratedTxOutput oc txOuts of
-- --     Nothing ->
-- --       fail $
-- --         "Could not locate output corresponding to constraint "
-- --           <> show (prettyOutConstraint oc)
-- --     Just txOut -> (txOut :) <$> orderTxOutputs ocs (List.delete txOut txOuts)
-- orderTxOutputs :: [OutConstraint] -> [Pl.TxOut] -> [Pl.TxOut]
-- orderTxOutputs expected given =
--   let res = map outConstraintToTxOut expected
--    in -- TODO: this should just be `res ++ (given List.\\ res)`. However, there is a bug
--       -- in plutus-apps where the StakingCredential is erased. Thus, we need to do this
--       -- custom subtraction.
--       res ++ List.deleteFirstsBy ((==) `on` Pl.addressCredential . Pl.txOutAddress) given res

-- extractDatumStr :: TxSkel -> Map Pl.DatumHash String
-- extractDatumStr = Map.map show . txSkelData
