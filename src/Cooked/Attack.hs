-- | Centralized module with automated attacks
module Cooked.Attack (module X) where

import Cooked.Attack.DatumHijacking as X
import Cooked.Attack.DatumTampering as X
import Cooked.Attack.PeerTampering as X
import Cooked.Attack.RedeemerTampering as X
import Cooked.Attack.TokenDuplication as X
