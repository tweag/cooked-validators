module Cooked.Skeleton.Redeemer
  ( TxSkelRedeemer (..),
    Redeemer (..),
    RedeemerConstrs,
    withReferenceInput,
    someTxSkelRedeemer,
    emptyTxSkelRedeemer,
    toTypedRedeemer,
  )
where

import Cooked.Pretty.Class
import Data.Typeable (cast)
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx
import Type.Reflection

type RedeemerConstrs redeemer =
  ( Api.ToData redeemer,
    Show redeemer,
    PrettyCooked redeemer,
    PlutusTx.Eq redeemer,
    Typeable redeemer
  )

data Redeemer where
  EmptyRedeemer :: Redeemer
  SomeRedeemer :: (RedeemerConstrs redeemer) => redeemer -> Redeemer

deriving instance (Show Redeemer)

instance Eq Redeemer where
  EmptyRedeemer == EmptyRedeemer = True
  (SomeRedeemer r1) == (SomeRedeemer r2) =
    case typeOf r1 `eqTypeRep` typeOf r2 of
      Just HRefl -> r1 PlutusTx.== r2
      Nothing -> False
  _ == _ = False

data TxSkelRedeemer = TxSkelRedeemer
  { txSkelRedeemer :: Redeemer,
    -- An optional input containing a reference script
    txSkelReferenceInput :: Maybe Api.TxOutRef
  }
  deriving (Show, Eq)

-- Attempts to cast a redeemer to a certain type
toTypedRedeemer :: (Typeable a) => Redeemer -> Maybe a
toTypedRedeemer (SomeRedeemer red) = cast red
toTypedRedeemer EmptyRedeemer = Nothing

-- Two helpers to create skeleton redeemers
someTxSkelRedeemer :: (RedeemerConstrs redeemer) => redeemer -> TxSkelRedeemer
someTxSkelRedeemer a = TxSkelRedeemer (SomeRedeemer a) Nothing

emptyTxSkelRedeemer :: TxSkelRedeemer
emptyTxSkelRedeemer = TxSkelRedeemer EmptyRedeemer Nothing

-- Additional helper to specify a given reference input. As reference inputs are
-- automatically attached during transaction generation when they contain the
-- right scripts by default, there are only 3 cases where this can be useful:
-- - The reliance on a reference script needs to be made explicit
-- - A wrong reference script somehow needs to be attached
-- - The automated attachement of reference inputs has been disabled using the
-- `txOptAutoReferenceScripts` option

withReferenceInput :: TxSkelRedeemer -> Api.TxOutRef -> TxSkelRedeemer
withReferenceInput red ref = red {txSkelReferenceInput = Just ref}
