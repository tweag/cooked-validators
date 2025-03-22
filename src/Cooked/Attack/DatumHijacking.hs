-- | This module provides an automated attack to try and redirect outputs to a
-- certain target with a similar datum type.
module Cooked.Attack.DatumHijacking
  ( redirectOutputTweak,
    datumHijackingAttack,
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

redirectOutputTweak ::
  forall owner owner' m.
  (MonadTweak m, OwnerConstraints owner, OwnerConstraints owner') =>
  (ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script) -> Maybe owner') ->
  (Integer -> Bool) ->
  m [ConcreteOutput owner TxSkelOutDatum TxSkelOutValue (Script.Versioned Script.Script)]
redirectOutputTweak outputPred indexPred = do
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

-- | A datum hijacking attack, simplified: This attack tries to substitute a
-- different recipient on 'PaysScript' constraints, but leaves the datum as it
-- is. That is, it tests for careless uses of something like 'txInfoOutputs' in
-- places where something like 'getContinuingOutputs' should be used. If this
-- attack goes through, however, a "proper" datum hijacking attack that modifies
-- the datum in a way that (the relevant part of) the
-- 'toBuiltinData'-translation stays the same will also work.
--
-- A 'DatumHijackingLbl' with the hash of the "thief" validator is added to the
-- labels of the 'TxSkel' using 'addLabel'.
--
-- This attack returns the list of outputs it redirected, in the order in which
-- they occurred on the original transaction. If no output is redirected, this
-- attack fails.
datumHijackingAttack ::
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
datumHijackingAttack change select thief = do
  redirected <- redirectOutputTweak (\output -> if change output then Just thief else Nothing) select
  guard . not $ null redirected
  addLabelTweak $ DatumHijackingLbl $ Script.toCredential thief
  return redirected

newtype DatumHijackingLbl = DatumHijackingLbl Api.Credential
  deriving (Show, Eq, Ord)

instance PrettyCooked DatumHijackingLbl where
  prettyCookedOpt opts (DatumHijackingLbl address) = "DatumHijacking" <+> prettyCookedOpt opts address
