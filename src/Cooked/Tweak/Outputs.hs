-- | 'Tweak's working on the outputs of a 'TxSkel'
module Cooked.Tweak.Outputs
  ( ensureOutputTweak,
    addOutputTweak,
    removeOutputsTweak,
    tamperDatumTweak,
    TamperDatumLbl (..),
    malformDatumTweak,
    MalformDatumLbl (..),
  )
where

import Control.Monad
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
import Data.List (partition)
import Data.Maybe
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.NonDet

-- | Ensures that a certain output is produced by a transaction. The return
-- value will be @Just@ the added output, when applicable.
ensureOutputTweak ::
  (Member Tweak effs) =>
  TxSkelOut ->
  Sem effs (Maybe TxSkelOut)
ensureOutputTweak txSkelOut = do
  presentOutputs <- viewTweak txSkelOutsL
  if txSkelOut `elem` presentOutputs
    then return Nothing
    else do
      addOutputTweak txSkelOut
      return $ Just txSkelOut

-- | Adds a transaction output, at the end of the current list of outputs, thus
-- retaining the initial outputs order.
addOutputTweak ::
  (Member Tweak effs) =>
  TxSkelOut ->
  Sem effs ()
addOutputTweak txSkelOut = overTweak txSkelOutsL (++ [txSkelOut])

-- | Removes transaction outputs according to some predicate. The returned list
-- contains all the removed outputs.
removeOutputsTweak ::
  (Member Tweak effs) =>
  (TxSkelOut -> Bool) ->
  Sem effs [TxSkelOut]
removeOutputsTweak removePred = do
  presentOutputs <- viewTweak txSkelOutsL
  let (removed, kept) = partition removePred presentOutputs
  setTweak txSkelOutsL kept
  return removed

-- | A label added to a 'TxSkel' on which the 'tamperDatumTweak' has been
-- successfully applied
data TamperDatumLbl = TamperDatumLbl deriving (Show, Eq, Ord)

instance PrettyCooked TamperDatumLbl where
  prettyCooked _ = "TamperDatum"

-- | A tweak that tries to change the datum on outputs carrying datums of a
-- certain type with a prescribed tampering function. The tampering function
-- ignores datums of other types and those for which it returns @Nothing@.
--
-- The tweak returns a list of the modified datums, as they were *before* the
-- modification was applied to them.
tamperDatumTweak ::
  forall a effs.
  ( Members '[Tweak, NonDet] effs,
    DatumConstrs a
  ) =>
  (a -> Maybe a) ->
  Sem effs [a]
tamperDatumTweak change = do
  beforeModification <- overMaybeTweak (txSkelOutsL % traversed % txSkelOutDatumL % txSkelOutDatumTypedAT) change
  guard . not . null $ beforeModification
  addLabelTweak TamperDatumLbl
  return beforeModification

-- | A tweak that tries to change the datum on outputs carrying datums of a
-- certain type with a prescribed tampering function. There are two main
-- differences with 'tamperDatumTweak'. First, the tampering function returns
-- 'Api.BuiltinData', allowing it to do pretty much anything with the
-- datums. Second, for every output datum there are zero or more options for how
-- to modify it, and all combinations of these modifications are tried.
--
-- That is, if there are @n@ output datums, for which there are @k_1,...,k_n@
-- possible modifications, this tweak will try
--
-- >   k_1 + ... + k_n
-- > + k_1 * k_2 + ... + k_{n-1} * k_n
-- > + k_1 * k_2 * k_3 + ... + k_{n-2} * k_{n-1} * k_n
-- > + ...
-- > + k_1 * k_2 * ... * k_{n-1} * k_n
-- > == (k_1 + 1) * ... * (k_n + 1) - 1
--
-- modified transactions.
malformDatumTweak ::
  forall a effs.
  ( Members '[Tweak, NonDet] effs,
    DatumConstrs a
  ) =>
  (a -> [Api.BuiltinData]) ->
  Sem effs ()
malformDatumTweak change = do
  outputs <- viewAllTweak (txSkelOutsL % traversed)
  let modifiedOutputs = map (\output -> output : changeOutput output) outputs
      -- We remove the first combination because it consists of all the heads
      -- and therefore it is the combination consisting of no changes at all.
      modifiedOutputGroups = tail $ sequence modifiedOutputs
  msum $ map (setTweak txSkelOutsL) modifiedOutputGroups
  addLabelTweak MalformDatumLbl
  where
    changeOutput :: TxSkelOut -> [TxSkelOut]
    changeOutput txSkelOut =
      do
        typedDat <- maybeToList $ preview (txSkelOutDatumL % txSkelOutDatumTypedAT) txSkelOut
        modifiedDat <- change typedDat
        return $ set (txSkelOutDatumL % txSkelOutDatumTypedAT @a) modifiedDat txSkelOut

-- | A label added to a 'TxSkel' on which the 'malformDatumTweak' has been
-- successfully applied
data MalformDatumLbl = MalformDatumLbl deriving (Show, Eq, Ord)

instance PrettyCooked MalformDatumLbl where
  prettyCooked _ = "MalformDatum"
