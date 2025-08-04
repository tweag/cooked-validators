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
    txSkelOutPredDatumHijackingParams,
  )
where

import Control.Monad
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak
import Data.Bifunctor
import Data.Kind (Type)
import Data.Maybe
import Data.Typeable
import Optics.Core

-- | The 'DatumHijackingLabel' stores the outputs that have been redirected,
-- before their destination were changed.
newtype DatumHijackingLabel = DatumHijackingLabel [TxSkelOut]
  deriving (Show, Eq, Ord)

instance PrettyCooked DatumHijackingLabel where
  prettyCookedOpt opts (DatumHijackingLabel txSkelOuts) = prettyItemize opts "Redirected outputs" "-" txSkelOuts

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

-- | 'defaultDatumHijackingParams' targets all the outputs for which the focus
-- of a given optic exists, and redirects each of them in a separate
-- transaction.
defaultDatumHijackingParams :: (IsTxSkelOutAllowedOwner owner, Is k An_AffineFold) => Optic' k is TxSkelOut x -> owner -> DatumHijackingParams
defaultDatumHijackingParams optic thief =
  DatumHijackingParams
    ((thief <$) . preview optic)
    (const True)
    False

-- | 'txSkelOutPredDatumHijackingParams' targets all the outputs satisfying a
-- given predicate, and redirects each of them in a separate transaction.
txSkelOutPredDatumHijackingParams :: (IsTxSkelOutAllowedOwner owner) => (TxSkelOut -> Bool) -> owner -> DatumHijackingParams
txSkelOutPredDatumHijackingParams predicate = defaultDatumHijackingParams (selectP predicate)

-- | Datum hijacking parameters targetting all the outputs owned by a certain
-- type of owner, and redirecting each of them in a separate transaction.
ownedByDatumHijackingParams :: forall (oldOwner :: Type) owner. (IsTxSkelOutAllowedOwner owner, Typeable oldOwner) => owner -> DatumHijackingParams
ownedByDatumHijackingParams = defaultDatumHijackingParams (txSkelOutOwnerL % userTypedAF @oldOwner)

-- | Datum hijacking parameters targetting all the outputs owned by a script,
-- and redirecting each of them in a separate transaction.
scriptsDatumHijackingParams :: (IsTxSkelOutAllowedOwner owner) => owner -> DatumHijackingParams
scriptsDatumHijackingParams = defaultDatumHijackingParams (txSkelOutOwnerL % userScriptHashAF)

-- | Datum hijacking parameters targetting all the outputs with a certain type
-- of datum, and redirecting each of them in a separate transaction.
datumOfDatumHijackingParams :: forall dat owner. (IsTxSkelOutAllowedOwner owner, DatumConstrs dat) => owner -> DatumHijackingParams
datumOfDatumHijackingParams = defaultDatumHijackingParams (txSkelOutDatumL % txSkelOutDatumTypedAT @dat)

-- | 'ofDatumDatumHijackingParams' targets all the outputs w

-- | Redirects, in the same transaction, all the outputs targetted by an output
-- and an index predicates. See 'DatumHijackingParams' for more information on
-- those predicates. Returns a pair of the old outputs before they were
-- redirected, and the new updated list of outputs.
redirectOutputTweakAll ::
  (MonadTweak m, IsTxSkelOutAllowedOwner owner) =>
  (TxSkelOut -> Maybe owner) ->
  (Integer -> Bool) ->
  m ([TxSkelOut], [TxSkelOut])
redirectOutputTweakAll outputPred indexPred = do
  outputs <- viewTweak txSkelOutsL
  return $ go outputs 0
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
  (MonadTweak m, IsTxSkelOutAllowedOwner owner) =>
  (TxSkelOut -> Maybe owner) ->
  (Integer -> Bool) ->
  m ([TxSkelOut], [TxSkelOut])
redirectOutputTweakAny outputPred indexPred = viewTweak txSkelOutsL >>= go [] 0
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

-- | A datum hijacking attack, simplified: This attack tries to substitute a
-- different recipient on certain outputs based on a 'DatumHijackingParams'.
--
-- A 'DatumHijackingLabel' is added to the labels of the 'TxSkel' using
-- 'addLabelTweak'. It contains the outputs that have been redirected, which
-- also corresponds to the returned value of this tweak. The tweak fails if no
-- such outputs have been redirected.
datumHijackingAttack :: (MonadTweak m) => DatumHijackingParams -> m [TxSkelOut]
datumHijackingAttack (DatumHijackingParams outputPred indexPred mode) = do
  (redirected, newOutputs) <- (if mode then redirectOutputTweakAll else redirectOutputTweakAny) outputPred indexPred
  guard $ not $ null redirected
  setTweak txSkelOutsL newOutputs
  addLabelTweak $ DatumHijackingLabel redirected
  return redirected
