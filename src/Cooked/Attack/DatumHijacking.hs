{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module provides an automated attack to try and redirect outputs to a
-- certain target with a similar datum type.
module Cooked.Attack.DatumHijacking
  ( redirectOutputTweakAll,
    DatumHijackingParams (..),
    DatumHijackingLabel (..),
    redirectOutputTweakAny,
    datumHijackingAttack,
    ownedByDatumHijackingParams,
    scriptsDatumHijackingParams,
    defaultDatumHijackingParams,
    datumOfDatumHijackingParams,
    outPredDatumHijackingParams,
  )
where

import Control.Monad
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak
import Cooked.Tweak.Common
import Data.Bifunctor
import Data.Kind (Type)
import Data.Maybe
import Data.Typeable
import Optics.Core
import Polysemy
import Polysemy.NonDet

-- | Parameters of the datum hijacking attacks. They state precisely which
-- outputs should have their owner changed, wich owner should be assigned, to
-- each of these outputs, and whether several modified outputs should be
-- combined in a single transaction, or instead spread out multiple branches.
data DatumHijackingParams where
  DatumHijackingParams ::
    (IsTxSkelOutAllowedOwner owner) =>
    { -- | Return 'Just' the new owner, or 'Nothing' if you want to leave this
      -- output unchanged.
      dhpOutputPred :: TxSkelOut -> Maybe owner,
      -- | The redirection described by the previous argument might apply to more
      -- than one of the outputs of the transaction. Use this predicate to select
      -- which of the redirectable outputs to actually redirect. We count the
      -- redirectable outputs from the left to the right, starting with zero.
      dhpIndexPred :: Integer -> Bool,
      -- | Whether all the outputs targetted by the predicates should be
      -- redirected in the same transaction, or one at a time, each in a
      -- distinct transaction.
      dhpAllOutputs :: Bool
    } ->
    DatumHijackingParams

-- | Targets all the outputs for which the focus of a given optic exists, and
-- redirects each of them in a separate transaction.
defaultDatumHijackingParams ::
  ( IsTxSkelOutAllowedOwner owner,
    Is k An_AffineFold
  ) =>
  Optic' k is TxSkelOut x ->
  owner ->
  DatumHijackingParams
defaultDatumHijackingParams optic thief =
  DatumHijackingParams
    ((thief <$) . preview optic)
    (const True)
    False

-- | Targets all the outputs satisfying a given predicate, and redirects each of
-- them in a separate transaction.
outPredDatumHijackingParams ::
  (IsTxSkelOutAllowedOwner owner) =>
  (TxSkelOut -> Bool) ->
  owner ->
  DatumHijackingParams
outPredDatumHijackingParams = defaultDatumHijackingParams . filtered

-- | Datum hijacking parameters targetting all the outputs owned by a certain
-- type of owner, and redirecting each of them in a separate transaction.
ownedByDatumHijackingParams ::
  forall (oldOwner :: Type) owner.
  ( IsTxSkelOutAllowedOwner owner,
    Typeable oldOwner
  ) =>
  owner ->
  DatumHijackingParams
ownedByDatumHijackingParams = defaultDatumHijackingParams (txSkelOutOwnerL % userTypedAF @oldOwner)

-- | Datum hijacking parameters targetting all the outputs owned by a script,
-- and redirecting each of them in a separate transaction.
scriptsDatumHijackingParams ::
  (IsTxSkelOutAllowedOwner owner) =>
  owner ->
  DatumHijackingParams
scriptsDatumHijackingParams = defaultDatumHijackingParams (txSkelOutOwnerL % userScriptHashAF)

-- | Datum hijacking parameters targetting all the outputs with a certain type
-- of datum, and redirecting each of them in a separate transaction.
datumOfDatumHijackingParams ::
  forall dat owner.
  ( IsTxSkelOutAllowedOwner owner,
    DatumConstrs dat
  ) =>
  owner ->
  DatumHijackingParams
datumOfDatumHijackingParams = defaultDatumHijackingParams (txSkelOutDatumL % txSkelOutDatumTypedAT @dat)

-- | Redirects, in the same transaction, all the outputs targetted by an output
-- and an index predicates. See 'DatumHijackingParams' for more information on
-- those predicates. Returns the list of outputs that were successfully
-- modified, before the modification is applied.
redirectOutputTweakAll ::
  ( Member Tweak effs,
    IsTxSkelOutAllowedOwner owner
  ) =>
  (TxSkelOut -> Maybe owner) ->
  (Integer -> Bool) ->
  Sem effs [TxSkelOut]
redirectOutputTweakAll outputPred indexPred = do
  outputs <- viewTweak txSkelOutsL
  let (redirected, newOutputs) = go outputs 0
  setTweak txSkelOutsL newOutputs
  return redirected
  where
    go [] _ = ([], [])
    go (out : l) n =
      case outputPred out of
        Nothing -> second (out :) $ go l n
        Just newOwner | indexPred n -> bimap (out :) ((out & txSkelOutOwnerL .~ toPKHOrVScript newOwner) :) $ go l (n + 1)
        _ -> second (out :) $ go l (n + 1)

-- | Redirects, each in their own transaction, all the outputs targetted by an
-- output and an index predicates. See 'DatumHijackingParams' for more
-- information on those predicates.
redirectOutputTweakAny ::
  ( Members '[Tweak, NonDet] effs,
    IsTxSkelOutAllowedOwner owner
  ) =>
  (TxSkelOut -> Maybe owner) ->
  (Integer -> Bool) ->
  Sem effs [TxSkelOut]
redirectOutputTweakAny outputPred indexPred = do
  outputs <- viewTweak txSkelOutsL
  (redirected, newOutputs) <- go [] 0 outputs
  setTweak txSkelOutsL newOutputs
  return redirected
  where
    go _ _ [] = mzero
    go l' n (out : l)
      | indexPred n =
          fromMaybe
            (go (l' ++ [out]) (n + 1) l)
            ( do
                newOwner <- outputPred out
                return $
                  mplus
                    (return ([out], l' ++ (out & txSkelOutOwnerL .~ toPKHOrVScript newOwner) : l))
                    (go (l' ++ [out]) (n + 1) l)
            )
    go l' n (out : l) = go (l' ++ [out]) n l

-- | The 'DatumHijackingLabel' stores the outputs that have been redirected,
-- before their destination were changed.
newtype DatumHijackingLabel = DatumHijackingLabel [TxSkelOut]
  deriving (Show, Eq, Ord)

instance PrettyCooked DatumHijackingLabel where
  prettyCookedOpt opts (DatumHijackingLabel txSkelOuts) = prettyItemize opts "Redirected outputs" "-" txSkelOuts

-- | The datum hijacking tries to substitute a different recipient on certain
-- outputs based on a 'DatumHijackingParams'.
--
-- A 'DatumHijackingLabel' is added to the labels of the 'TxSkel'. It contains
-- the outputs that have been redirected, which also corresponds to the returned
-- value of this tweak. The tweak fails if no such outputs have been redirected.
datumHijackingAttack ::
  (Members '[Tweak, NonDet] effs) =>
  DatumHijackingParams ->
  Sem effs [TxSkelOut]
datumHijackingAttack (DatumHijackingParams outputPred indexPred mode) = do
  redirected <- (if mode then redirectOutputTweakAll else redirectOutputTweakAny) outputPred indexPred
  guard $ not $ null redirected
  addLabelTweak $ DatumHijackingLabel redirected
  return redirected
