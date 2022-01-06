module Example where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Ledger.Ada as Pl

-- * MockChain Example

-- | Start from the initial 'UtxoIndex', where each known 'wallet's have the
-- same amount of Ada, then transfers 4200 lovelace from wallet 1 to wallet 2.
-- This transfers from wallet 1 because that's the default signer of transactions
-- if nothing else is specified.
example :: Either MockChainError ((), UtxoState)
example = runMockChain $ do
  void $
    validateTxSkel $
      txSkel
        [PaysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200)]
