-- | This module exposes the notion of reference scripts used in our
-- 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.ReferenceScript
  ( ReferenceScriptConstrs,
    TxSkelOutReferenceScript (..),
    txSkelOutReferenceScriptHashAF,
    txSkelOutReferenceScriptTypedP,
    txSkelOutReferenceScriptVersionedP,
  )
where

import Data.Function (on)
import Data.Typeable
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Reference scripts are typeable and can be converted to versioned scripts.
type ReferenceScriptConstrs refScript =
  ( Script.ToVersioned Script.Script refScript,
    Typeable refScript
  )

-- | Reference scripts used in 'Cooked.Skeleton.Ouput.TxSkelOut'
data TxSkelOutReferenceScript where
  NoTxSkelOutReferenceScript :: TxSkelOutReferenceScript
  SomeTxSkelOutReferenceScript :: (ReferenceScriptConstrs a) => a -> TxSkelOutReferenceScript

instance Eq TxSkelOutReferenceScript where
  (==) = (==) `on` preview txSkelOutReferenceScriptHashAF

instance Show TxSkelOutReferenceScript where
  show refScript =
    maybe
      "No reference script"
      (("Reference script: " <>) . show)
      $ preview txSkelOutReferenceScriptHashAF refScript

-- | A prism targeting a certain typed reference script within a 'TxSkelOutReferenceScript'
txSkelOutReferenceScriptTypedP :: (ReferenceScriptConstrs a, ReferenceScriptConstrs b) => Prism TxSkelOutReferenceScript TxSkelOutReferenceScript a b
txSkelOutReferenceScriptTypedP =
  prism
    SomeTxSkelOutReferenceScript
    ( \refScript -> case refScript of
        NoTxSkelOutReferenceScript -> Left refScript
        SomeTxSkelOutReferenceScript script -> maybe (Left refScript) Right (cast script)
    )

-- | A prism targeting the versioned script within a 'TxSkelOutReferenceScript'
txSkelOutReferenceScriptVersionedP :: Prism' TxSkelOutReferenceScript (Script.Versioned Script.Script)
txSkelOutReferenceScriptVersionedP =
  prism
    SomeTxSkelOutReferenceScript
    ( \refScript -> case refScript of
        NoTxSkelOutReferenceScript -> Left refScript
        SomeTxSkelOutReferenceScript script -> Right (Script.toVersioned script)
    )

-- | An affine fold producing an optional script hash from a 'TxSkelOutReferenceScript'
txSkelOutReferenceScriptHashAF :: AffineFold TxSkelOutReferenceScript Api.ScriptHash
txSkelOutReferenceScriptHashAF = txSkelOutReferenceScriptVersionedP % to Script.toScriptHash
