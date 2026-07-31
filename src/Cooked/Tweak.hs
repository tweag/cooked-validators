-- | This module centralizes Tweaks, that is state-aware skeleton
-- modifications. These tweaks can be used on specific skeletons, or deployed in
-- time using `Cooked.Ltl`
module Cooked.Tweak (module X) where

import Cooked.Tweak.Common as X
import Cooked.Tweak.Inputs as X
import Cooked.Tweak.Insertion as X
import Cooked.Tweak.Modification as X
import Cooked.Tweak.Outputs as X
import Cooked.Tweak.Removal as X
import Cooked.Tweak.Signatories as X
