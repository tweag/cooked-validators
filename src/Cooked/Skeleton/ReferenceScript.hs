-- | This module exposes the notion of reference scripts used in our
-- 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.ReferenceScript
  ( ReferenceScriptConstrs,
    TxSkelOutReferenceScript (..),
    txSkelOutTypedRefScriptAT,
    txSkelOutRefScriptVersioned,
    txSkelOutRefScriptHash,
  )
where

import Data.Typeable
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Type constraints over the reference script in a
-- 'Cooked.Skeleton.Ouput.TxSkelOut'
type ReferenceScriptConstrs refScript =
  ( Script.ToVersioned Script.Script refScript,
    Show refScript,
    Typeable refScript
  )

-- | Reference scripts in 'Cooked.Skeleton.Ouput.TxSkelOut'
data TxSkelOutReferenceScript where
  TxSkelOutNoReferenceScript :: TxSkelOutReferenceScript
  TxSkelOutSomeReferenceScript :: (ReferenceScriptConstrs a) => a -> TxSkelOutReferenceScript

deriving instance Show TxSkelOutReferenceScript

-- | Retrieving, or setting, a typed reference script
txSkelOutTypedRefScriptAT :: (ReferenceScriptConstrs a) => AffineTraversal' TxSkelOutReferenceScript a
txSkelOutTypedRefScriptAT =
  atraversal
    ( \x -> case x of
        TxSkelOutNoReferenceScript -> Left x
        TxSkelOutSomeReferenceScript script -> maybe (Left x) Right (cast script)
    )
    ( flip
        ( \refScript -> \case
            TxSkelOutNoReferenceScript -> TxSkelOutNoReferenceScript
            TxSkelOutSomeReferenceScript _ -> TxSkelOutSomeReferenceScript refScript
        )
    )

-- | Retrieving the versioned reference script
txSkelOutRefScriptVersioned :: TxSkelOutReferenceScript -> Maybe (Script.Versioned Script.Script)
txSkelOutRefScriptVersioned TxSkelOutNoReferenceScript = Nothing
txSkelOutRefScriptVersioned (TxSkelOutSomeReferenceScript content) = Just $ Script.toVersioned content

-- | Retrieving the hash of the reference script
txSkelOutRefScriptHash :: TxSkelOutReferenceScript -> Maybe Api.ScriptHash
txSkelOutRefScriptHash = fmap Script.toScriptHash . txSkelOutRefScriptVersioned
