module Example where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Ledger.Ada as Pl

-- * MockChain Example

-- | Start from the initial 'UtxoIndex', where each known 'wallet's have the
-- same amount of Ada, then transfers 4200 lovelace from wallet 1 to wallet 2
example :: Either MockChainError ((), UtxoState)
example = runMockChain $ do
  void $
    validateTxSkel $
      txSkel
        (wallet 1)
        [PaysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200)]
