-- | This module defines an attack tampering on the validity interval of
-- transactions.
module Cooked.Attack.ValidityTampering
  ( -- * Validity tampering params
    ValidityTamperingParams (..),
    lowerExtendedValidityTamperingParams,
    lowerStrictValidityTamperingParams,
    upperExtendedValidityTamperingParams,
    upperStrictValidityTamperingParams,
    bothExtendedValidityTamperingParams,
    bothStrictValidityTamperingParams,
    intervalValidityTamperingParams,

    -- * Validity tampering label
    ValidityTamperingLabel (..),

    -- * Validity tampering attack
    validityTamperingAttack,
  )
where

import Control.Applicative
import Control.Monad
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak
import Ledger.Slot qualified as Ledger
import Optics.Core
import Polysemy
import Polysemy.NonDet
import Prettyprinter qualified as PP

-- | A label added to a 'TxSkel' on which a tweak tampering the validity
-- interval has been applied. The label contains the interval as it was before
-- being tampered with.
newtype ValidityTamperingLabel = ValidityTamperingLabel Ledger.SlotRange
  deriving (Show, Eq)

instance Ord ValidityTamperingLabel where
  compare (ValidityTamperingLabel s1) (ValidityTamperingLabel s2) =
    compare
      (view (at Lower) s1, view (at Upper) s1)
      (view (at Lower) s2, view (at Upper) s2)

instance PrettyCooked ValidityTamperingLabel where
  prettyCooked (ValidityTamperingLabel s) =
    "Validity tampering:" PP.<+> PP.pretty s

-- | Parameters of the validity tampering attack.
data ValidityTamperingParams k is f b
  = ValidityTamperingParams
  { -- | What part of the validity range to modify
    vtpOptic :: Optic' k is Ledger.SlotRange b,
    -- | How to modify the targeted part
    vtpChange :: b -> f b
  }

-- | Modifies the extended (possible infinite) lower bound of the validity
-- interval with a given tampering function.
lowerExtendedValidityTamperingParams ::
  (Maybe Ledger.Slot -> f (Maybe Ledger.Slot)) ->
  ValidityTamperingParams A_Lens NoIx f (Maybe Ledger.Slot)
lowerExtendedValidityTamperingParams =
  ValidityTamperingParams $ at Lower

-- | Modifies the strict lower bound of the validity interval with a given
-- tampering function, failing if it is infinite.
lowerStrictValidityTamperingParams ::
  (Ledger.Slot -> f Ledger.Slot) ->
  ValidityTamperingParams An_AffineTraversal NoIx f Ledger.Slot
lowerStrictValidityTamperingParams =
  ValidityTamperingParams $ ix Lower

-- | Modifies the extended (possible infinite) upper bound of the validity
-- interval with a given tampering function.
upperExtendedValidityTamperingParams ::
  (Maybe Ledger.Slot -> f (Maybe Ledger.Slot)) ->
  ValidityTamperingParams A_Lens NoIx f (Maybe Ledger.Slot)
upperExtendedValidityTamperingParams =
  ValidityTamperingParams $ at Upper

-- | Modifies the strict upper bound of the validity interval with a given
-- tampering function, failing if it is infinite.
upperStrictValidityTamperingParams ::
  (Ledger.Slot -> f Ledger.Slot) ->
  ValidityTamperingParams An_AffineTraversal NoIx f Ledger.Slot
upperStrictValidityTamperingParams =
  ValidityTamperingParams $ ix Upper

-- | Modifies both the extended (possible infinite) lower and upper bounds of
-- the validity interval with a given tampering function.
bothExtendedValidityTamperingParams ::
  (Maybe Ledger.Slot -> f (Maybe Ledger.Slot)) ->
  ValidityTamperingParams A_Traversal NoIx f (Maybe Ledger.Slot)
bothExtendedValidityTamperingParams =
  ValidityTamperingParams $ at Lower `adjoin` at Upper

-- | Modifies both the strict lower and upper bounds of the validity interval
-- with a given tampering function, failing if both are infinite.
bothStrictValidityTamperingParams ::
  (Ledger.Slot -> f Ledger.Slot) ->
  ValidityTamperingParams A_Traversal NoIx f Ledger.Slot
bothStrictValidityTamperingParams =
  ValidityTamperingParams $ ix Lower `adjoin` ix Upper

-- | Modifies the full validity interval directly
intervalValidityTamperingParams ::
  (Ledger.SlotRange -> f Ledger.SlotRange) ->
  ValidityTamperingParams An_Iso NoIx f Ledger.SlotRange
intervalValidityTamperingParams =
  ValidityTamperingParams simple

-- | The validity tampering attack attempts to tamper with the validity interval
-- of a transaction following a given set of parameters. This returns the
-- validity interval of the transaction before modification.
validityTamperingAttack ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Traversal,
    Foldable f,
    Alternative f
  ) =>
  ValidityTamperingParams k is f a ->
  Sem effs Ledger.SlotRange
validityTamperingAttack (ValidityTamperingParams optics change) = do
  currentValidityRange <- viewTweak txSkelValidityRangeL
  void $
    modifyTweakFromParams $
      modifyTweakParamsNoTypeChange
        OneBranchForAllFoci
        (txSkelValidityRangeL % castOptic @A_Traversal optics)
        change
  insertInTweak txSkelLabelsL $ TxSkelLabel $ ValidityTamperingLabel currentValidityRange
  return currentValidityRange
