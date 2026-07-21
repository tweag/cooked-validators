module Cooked.Tweak.Insertion
  ( insertTweak,
    insertThereTweak,
    insertFirstTweak,
    insertLastTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Optics.Core
import Polysemy

-- | Appends an element within a semigroup focused in a 'TxSkel'
insertTweak ::
  ( Member Tweak effs,
    Is k A_Setter,
    Semigroup a
  ) =>
  Optic' k is TxSkel a ->
  a ->
  Sem effs ()
insertTweak optic el = overTweak optic (<> el)

-- | Appends an element at the end of a list focused in a 'TxSkel'
insertLastTweak ::
  ( Member Tweak effs,
    Is k A_Setter
  ) =>
  Optic' k is TxSkel [a] ->
  a ->
  Sem effs ()
insertLastTweak optic = insertTweak optic . (: [])

-- | Appends an element at a specific position in a list focused in a 'TxSkel'
insertThereTweak ::
  ( Member Tweak effs,
    Is k A_Setter
  ) =>
  Optic' k is TxSkel [a] ->
  Int ->
  a ->
  Sem effs ()
insertThereTweak optic i a =
  overTweak optic (\(splitAt i -> (before, after)) -> before ++ (a : after))

-- | Appends an element at the end of a list focused in a 'TxSkel'
insertFirstTweak ::
  ( Member Tweak effs,
    Is k A_Setter
  ) =>
  Optic' k is TxSkel [a] ->
  a ->
  Sem effs ()
insertFirstTweak optic = insertThereTweak optic 0
