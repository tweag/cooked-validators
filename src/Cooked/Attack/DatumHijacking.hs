-- | This module provides an automated attack to try and redirect outputs to a
-- certain target with a similar datum type.
module Cooked.Attack.DatumHijacking
  ( redirectOutputTweakAny,
    datumHijackingAttackAny,
    datumHijackingAttack,
    redirectOutputTweakAll,
    datumHijackingAttackAll,
    DatumHijackingLbl (..),
  )
where

import Control.Monad
import Cooked.Output
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak
import Data.Maybe
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter ((<+>))

-- | Redirects some outputs from one owner to another owner, which can be of
-- different types. Returns the list of outputs it redirected (as they were
-- before the modification), in the order in which they occurred on the original
-- transaction.
redirectOutputTweakAll ::
  forall owner owner' m.
  (MonadTweak m, OwnerConstraints owner, OwnerConstraints owner') =>
  -- | Return 'Just' the new owner, or 'Nothing' if you want to leave this
  -- output unchanged.
  (ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script) -> Maybe owner') ->
  -- | The redirection described by the previous argument might apply to more
  -- than one of the outputs of the transaction. Use this predicate to select
  -- which of the redirectable outputs to actually redirect. We count the
  -- redirectable outputs from the left to the right, starting with zero.
  (Integer -> Bool) ->
  m [ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script)]
redirectOutputTweakAll outputPred indexPred = do
  outputs <- viewTweak txSkelOutsL
  let (changed, newOutputs) = unzip $ go outputs 0
  setTweak txSkelOutsL newOutputs
  return $ catMaybes changed
  where
    modifyOutputOwner (Pays out) = Pays . setOwner (fromAbstractOutput out)
    go [] _ = []
    go (out : l) n =
      case ( do
               out' <- preview txSkelOutOwnerTypeP out
               newOwner <- outputPred out'
               return (out', newOwner)
           ) of
        Nothing -> (Nothing, out) : go l n
        Just (out', newOwner) | indexPred n -> (Just out', modifyOutputOwner out newOwner) : go l (n + 1)
        _ -> (Nothing, out) : go l (n + 1)

-- | A version of 'redirectOutputTweakAll' where, instead of modifying all the
-- outputs targeted by the input predicates in the same transaction, we modify
-- one of them at a time, relying on the 'MonadPlus' instance of @m@.
redirectOutputTweakAny ::
  forall owner owner' m.
  (MonadTweak m, OwnerConstraints owner, OwnerConstraints owner') =>
  (ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script) -> Maybe owner') ->
  (Integer -> Bool) ->
  m (ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script))
redirectOutputTweakAny outputPred indexPred = viewTweak txSkelOutsL >>= go [] 0
  where
    go _ _ [] = mzero
    go l' n (out : l)
      | indexPred n =
          fromMaybe
            (go (l' ++ [out]) (n + 1) l)
            ( do
                out' <- preview txSkelOutOwnerTypeP out
                newOwner <- outputPred out'
                return $
                  mplus
                    (setTweak txSkelOutsL (l' ++ Pays (setOwner out' newOwner) : l) >> return out')
                    (go (l' ++ [out]) (n + 1) l)
            )
    go l' n (out : l) = go (l' ++ [out]) n l

-- | A datum hijacking attack, simplified: This attack tries to substitute a
-- different recipient on outputs belonging to scripts, but leaves the datum as
-- it is. That is, it tests for careless uses of something like
-- 'Api.txInfoOutputs' in places where something like 'Api.getContinuingOutputs'
-- should be used. If this attack goes through, however, a "proper" datum
-- hijacking attack that modifies the datum in a way that (the relevant part of)
-- the 'Api.toBuiltinData'-translation stays the same will also work.
--
-- A 'DatumHijackingLbl' with the hash of the "thief" validator is added to the
-- labels of the 'TxSkel' using 'addLabelTweak'.
--
-- This attack returns the list of outputs it redirected, in the order in which
-- they occurred on the original transaction. If no output is redirected, this
-- attack fails.
datumHijackingAttackAll ::
  forall owner owner' m.
  (MonadTweak m, OwnerConstraints owner, OwnerConstraints owner') =>
  -- | Predicate to select outputs to steal, depending on the intended
  -- recipient, the datum, and the value.
  (ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script) -> Bool) ->
  -- | The selection predicate may match more than one output. Use this
  -- predicate to restrict to the i-th of the outputs (counting from the left,
  -- starting at zero) chosen by the selection predicate with this predicate.
  (Integer -> Bool) ->
  -- | The thief
  owner' ->
  m [ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script)]
datumHijackingAttackAll change select thief = do
  redirected <- redirectOutputTweakAll (\output -> if change output then Just thief else Nothing) select
  guard . not $ null redirected
  addLabelTweak $ DatumHijackingLbl $ Script.toCredential thief
  return redirected

-- | A version of datumHijackingAttackAll relying on the rules of
-- 'redirectOutputTweakAny'.
datumHijackingAttackAny ::
  forall owner owner' m.
  (MonadTweak m, OwnerConstraints owner, OwnerConstraints owner') =>
  -- | Predicate to select outputs to steal, depending on the intended
  -- recipient, the datum, and the value.
  (ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script) -> Bool) ->
  -- | The selection predicate may match more than one output. Use this
  -- predicate to restrict to the i-th of the outputs (counting from the left,
  -- starting at zero) chosen by the selection predicate with this predicate.
  (Integer -> Bool) ->
  -- | The thief
  owner' ->
  m (ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script))
datumHijackingAttackAny change select thief = do
  redirected <- redirectOutputTweakAny (\output -> if change output then Just thief else Nothing) select
  addLabelTweak $ DatumHijackingLbl $ Script.toCredential thief
  return redirected

-- | The default datum hijacking attack. It tries to redirect any output for
-- which the owner is of type @owner@ and branches at each attempt.
datumHijackingAttack ::
  forall owner owner' m.
  (MonadTweak m, OwnerConstraints owner, OwnerConstraints owner') =>
  owner' ->
  m (ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script))
datumHijackingAttack = datumHijackingAttackAny (const True) (const True)

-- | A label that is added to a 'TxSkel' that has successfully been modified by
-- any of the datum hijacking attacks
newtype DatumHijackingLbl = DatumHijackingLbl Api.Credential
  deriving (Show, Eq, Ord)

instance PrettyCooked DatumHijackingLbl where
  prettyCookedOpt opts (DatumHijackingLbl address) = "DatumHijacking" <+> prettyCookedOpt opts address
