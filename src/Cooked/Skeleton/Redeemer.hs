-- | This module exposes the notion of redeemers used whenever a script in
-- invoked in a 'Cooked.Skeleton.TxSkel'.
module Cooked.Skeleton.Redeemer
  ( TxSkelRedeemer (..),
    RedeemerConstrs,
    withReferenceInput,
    someTxSkelRedeemer,
    emptyTxSkelRedeemer,
    getTypedRedeemer,
    setTypedRedeemer,
    someTxSkelRedeemerNoAutoFill,
    emptyTxSkelRedeemerNoAutoFill,
  )
where

import Cooked.Pretty.Common
import Data.Typeable (Typeable, cast)
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx

-- | These are the constraints that must be satisfied by the inner content of a
-- redeemer, that is the actual data that will be passed to the script as its
-- redeemer during during validation
type RedeemerConstrs redeemer =
  ( Api.ToData redeemer,
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
      -- | An optional reference input containing the script to execute
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

-- | Attempts to retrieve the content of a 'TxSkelRedeemer' and cast it to a
-- given type
getTypedRedeemer :: (Typeable a) => TxSkelRedeemer -> Maybe a
getTypedRedeemer (TxSkelRedeemer red _ _) = cast red

-- | Changes the inner content of this 'TxSkelRedeemer', leaving the reference
-- input unchanged. This operation is type-changing.
setTypedRedeemer :: (RedeemerConstrs redeemer) => redeemer -> TxSkelRedeemer -> TxSkelRedeemer
setTypedRedeemer red txSkelRed = txSkelRed {txSkelRedeemerContent = red}

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

-- | Attaches a reference input to a given 'TxSkelRedeemer'. This should usually
-- be of no use if option 'Cooked.Skeleton.Option.txOptAutoReferenceScripts' is
-- turned on, which is the case by default.
withReferenceInput :: TxSkelRedeemer -> Api.TxOutRef -> TxSkelRedeemer
withReferenceInput red ref = red {txSkelRedeemerReferenceInput = Just ref}
