-- | This module centralizes the node-backend (BlockChain) running code. It is
-- an umbrella re-exporting all the BlockChain submodules, which only provide
-- instances.
module Cooked.BlockChain () where

import Cooked.BlockChain.Instances ()
import Cooked.BlockChain.Runnable ()
