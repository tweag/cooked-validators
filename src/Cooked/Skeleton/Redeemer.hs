-- | This module exposes the notion of redeemers used whenever a script in
-- invoked in a 'Cooked.Skeleton.TxSkel'.
module Cooked.Skeleton.Redeemer
  ( TxSkelRedeemer (..),
    RedeemerConstrs,
    withReferenceInput,
    someTxSkelRedeemer,
    emptyTxSkelRedeemer,
    txSkelRedeemerReferenceInputL,
    txSkelRedeemerAutoFillL,
    txSkelTypedRedeemerAT,
    someTxSkelRedeemerNoAutoFill,
    emptyTxSkelRedeemerNoAutoFill,
    txSkelBuiltinDataRedeemerL,
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
    cast red PlutusTx.== Just red' PlutusTx.&& mRefIn PlutusTx.== mRefIn' PlutusTx.&& af PlutusTx.== af'

-- * Navigating within a 'TxSkelRedeemer'

-- | Sets or gets the reference input from a redeemer
makeLensesFor [("txSkelRedeemerReferenceInput", "txSkelRedeemerReferenceInputL")] ''TxSkelRedeemer

-- | Sets or gets the autofill property from a redeemer
makeLensesFor [("txSkelRedeemerAutoFill", "txSkelRedeemerAutoFillL")] ''TxSkelRedeemer

-- | Attaches a reference input to a given 'TxSkelRedeemer'. This should usually
-- be of no use if option 'Cooked.Skeleton.Option.txOptAutoReferenceScripts' is
-- turned on, which is the case by default.
withReferenceInput :: TxSkelRedeemer -> Api.TxOutRef -> TxSkelRedeemer
withReferenceInput red ref = red & txSkelRedeemerReferenceInputL ?~ ref

-- | Extracts, or sets, the redeemer content of a redeemer of a given type. This
-- is attempted in two ways: first, we try to simply cast the content, and then,
-- if it fails, we serialise the content and then attempt to deserialise it to
-- the right type. This second case is specifically useful when the current
-- content is an `Api.BuiltinData` itself directly, but it can also be used in
-- the cornercase when both types have compatible serialized representation.
txSkelTypedRedeemerAT :: (RedeemerConstrs a) => AffineTraversal' TxSkelRedeemer a
txSkelTypedRedeemerAT =
  atraversal
    ( \case
        (TxSkelRedeemer content _ _) | Just content' <- cast content -> Right content'
        (TxSkelRedeemer content _ _) | Just content' <- Api.fromBuiltinData $ Api.toBuiltinData content -> Right content'
        txSkelRed -> Left txSkelRed
    )
    (\red content -> red {txSkelRedeemerContent = content})

-- | Extracts, or sets, the redeemer content as an `Api.BuiltinData`
txSkelBuiltinDataRedeemerL :: Lens' TxSkelRedeemer Api.BuiltinData
txSkelBuiltinDataRedeemerL =
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
