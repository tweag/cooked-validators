module Example where

import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Generator
import qualified Ledger.Ada as Pl

-- * MockChain Example

--
-- Start from the initial 'UtxoIndex' and transfer 4200 lovelace from wallet 1 to wallet 2

example :: Either MockChainError ((), UtxoState)
example = runMockChain $ do
  validateTxFromSkeleton $
    TxSkel
      (wallet 1)
      [PaysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200)]

-- (mcWallet 1) mempty (Pl.mustPayToPubKey (mcWalletPKHash $ mcWallet 2) (Pl.lovelaceValueOf 4200))
