-- | This module exposes the notion of redeemers used whenever a script in
-- invoked in a 'Cooked.Skeleton.TxSkel'.
module Cooked.Skeleton.Redeemer
  ( -- * Type constraints
    RedeemerConstrs,

    -- * Data types
    TxSkelRedeemer (..),

    -- * Optics
    txSkelRedeemerMReferenceInputL,
    txSkelRedeemerReferenceInputAT,
    txSkelRedeemerAutoFillL,
    txSkelRedeemerTypedAT,
    txSkelRedeemerBuiltinDataL,

    -- * Smart constructors
    someTxSkelRedeemer,
    someTxSkelRedeemerNoAutoFill,
    emptyTxSkelRedeemer,
    emptyTxSkelRedeemerNoAutoFill,

    -- * Utilities
    autoFillReferenceInput,
  )
where

import Cooked.Pretty.Class
import Cooked.Pretty.Plutus ()
import Data.Typeable (Typeable, cast)
import Optics.Core
import Optics.TH
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx

-- * Types and constraints for redeemers used in skeletons

-- | These are the constraints that must be satisfied by the inner content of a
-- redeemer, that is the actual data that will be passed to the script as its
-- redeemer during during validation
type RedeemerConstrs redeemer =
  ( Api.ToData redeemer,
    Api.FromData redeemer,
    Show redeemer,
    PrettyCooked redeemer,
    PlutusTx.Eq redeemer,
    Typeable redeemer
  )

-- | A bundle around a redeemer which allows to provide a reference input in
-- which the script associated with the redeemer can be found
data TxSkelRedeemer where
  TxSkelRedeemer ::
    (RedeemerConstrs redeemer) =>
    { -- | The redeemer data with which the script will be executed
      txSkelRedeemerContent :: redeemer,
      -- | An optional reference input containing the script to execute. During
      -- transaction generation, this reference input will only be translated
      -- into a Cardano reference input if it does not appear in regular inputs.
      txSkelRedeemerReferenceInput :: Maybe Api.TxOutRef,
      -- | Whether the reference input can be automatically assigned. This will
      -- only trigger if 'txSkelRedeemerReferenceInput' is 'Nothing'
      txSkelRedeemerAutoFill :: Bool
    } ->
    TxSkelRedeemer

deriving instance (Show TxSkelRedeemer)

instance Eq TxSkelRedeemer where
  (TxSkelRedeemer red mRefIn af) == TxSkelRedeemer red' mRefIn' af' =
    (Api.toBuiltinData red, mRefIn, af) == (Api.toBuiltinData red', mRefIn', af')

instance Ord TxSkelRedeemer where
  compare (TxSkelRedeemer red mRefIn af) (TxSkelRedeemer red' mRefIn' af') =
    compare (Api.toBuiltinData red, mRefIn, af) (Api.toBuiltinData red', mRefIn', af')

-- * Navigating within a 'TxSkelRedeemer'

-- | Focuses on the possible reference input from a redeemer
makeLensesFor [("txSkelRedeemerReferenceInput", "txSkelRedeemerMReferenceInputL")] ''TxSkelRedeemer

-- | Focuses on the reference input form a redeemer
txSkelRedeemerReferenceInputAT :: AffineTraversal' TxSkelRedeemer Api.TxOutRef
txSkelRedeemerReferenceInputAT = txSkelRedeemerMReferenceInputL % _Just

-- | Sets or gets the autofill property from a redeemer
makeLensesFor [("txSkelRedeemerAutoFill", "txSkelRedeemerAutoFillL")] ''TxSkelRedeemer

-- | Extracts, or sets, the typed redeemer of a 'TxSkelRedeemer'. This is
-- attempted in two ways: first, we try to simply cast the content, and then, if
-- it fails, we serialise the content and then attempt to deserialise it to the
-- right type. This second case is specifically useful when the current content
-- is an 'Api.BuiltinData' itself directly, but it can also be used in the
-- cornercase when both types have compatible serialized representation.
txSkelRedeemerTypedAT :: (RedeemerConstrs a, RedeemerConstrs b) => AffineTraversal TxSkelRedeemer TxSkelRedeemer a b
txSkelRedeemerTypedAT =
  atraversal
    ( \case
        (TxSkelRedeemer content _ _) | Just content' <- cast content -> Right content'
        (TxSkelRedeemer content _ _) | Just content' <- Api.fromBuiltinData $ Api.toBuiltinData content -> Right content'
        txSkelRed -> Left txSkelRed
    )
    (\red content -> red {txSkelRedeemerContent = content})

-- | Extracts, or sets, the redeemer content as an `Api.BuiltinData`
txSkelRedeemerBuiltinDataL :: Lens' TxSkelRedeemer Api.BuiltinData
txSkelRedeemerBuiltinDataL =
  lens
    (\(TxSkelRedeemer content _ _) -> Api.toBuiltinData content)
    (\txSkelRed bData -> txSkelRed {txSkelRedeemerContent = bData})

-- * Building 'TxSkelRedeemer's

-- | Creates a 'TxSkelRedeemer' from an inner content with no reference input
someTxSkelRedeemer :: (RedeemerConstrs redeemer) => redeemer -> TxSkelRedeemer
someTxSkelRedeemer red = TxSkelRedeemer red Nothing True

-- | Creates a 'TxSkelRedeemer' from an inner content with no reference input,
-- while not allowing it to be automatically assigned
someTxSkelRedeemerNoAutoFill :: (RedeemerConstrs redeemer) => redeemer -> TxSkelRedeemer
someTxSkelRedeemerNoAutoFill red = TxSkelRedeemer red Nothing False

-- | Creates a 'TxSkelRedeemer' without an inner content nor a reference input
emptyTxSkelRedeemer :: TxSkelRedeemer
emptyTxSkelRedeemer = someTxSkelRedeemer ()

-- | Creates a 'TxSkelRedeemer' with no inner content and no reference input,
-- while dissallowing it to be automatically assinged
emptyTxSkelRedeemerNoAutoFill :: TxSkelRedeemer
emptyTxSkelRedeemerNoAutoFill = someTxSkelRedeemerNoAutoFill ()

-- * Additional helpers

-- | Attaches a reference input to this 'TxSkelRedeemer' when none is already
-- attached. Is meant to be used by the automated attachment process during
-- transaction generation.
autoFillReferenceInput :: Api.TxOutRef -> TxSkelRedeemer -> TxSkelRedeemer
autoFillReferenceInput refInput = over txSkelRedeemerMReferenceInputL (maybe (Just refInput) Just)
