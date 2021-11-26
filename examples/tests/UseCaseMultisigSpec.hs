{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UseCaseMultisigSpec where

import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Contexts as Validation
import qualified Ledger.TimeSlot as TimeSlot
import qualified Ledger.Typed.Scripts as TScripts
import qualified Ledger.Value as Value
import qualified Plutus.Contracts.Currency as Currency
import Plutus.Contracts.MultiSig
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude
import Test.Hspec
import qualified Prelude as Haskell

allowedSigners :: [Ledger.PubKeyHash]
allowedSigners = map (walletPKHash . wallet) [1, 4, 5, 7]

param :: MultiSig
param = MultiSig allowedSigners 3

multiVal :: TScripts.TypedValidator MultiSig
multiVal = typedValidator param

run1 :: Either MockChainError ((), UtxoState)
run1 =
  runMockChain $ do
    -- Everyone deposits 1000
    ( validateTxFromSkeleton
        . TxSkel
          (wallet 1)
        . mconcat
      )
      =<< Haskell.sequence
        [ spentByPK (walletPKHash $ wallet 4) (Ada.lovelaceValueOf 1000),
          spentByPK (walletPKHash $ wallet 5) (Ada.lovelaceValueOf 1000),
          spentByPK (walletPKHash $ wallet 7) (Ada.lovelaceValueOf 1000),
          return
            [ PaysScript multiVal [((), Ada.lovelaceValueOf 4000)],
              SignedBy (map wallet [1, 4, 5, 7])
            ]
        ]
    -- We then pay wallet 2 with this money
    [(out, dat)] <- scriptUtxosSuchThat multiVal (\_ _ -> True)
    validateTxFromSkeleton $
      TxSkel
        (wallet 1)
        [ SpendsScript multiVal () (out, dat),
          PaysPK (walletPKHash $ wallet 2) (Ada.lovelaceValueOf 2500),
          PaysScript multiVal [((), Ada.lovelaceValueOf 1500)],
          SignedBy (map wallet [1, 2, 4, 7]) -- Wallet 2 was not allowed to sign, but they did it no matter
        ]

-- Test spec
spec :: Spec
spec = do
  it "succeeds on the example run" $ do
    run1 `shouldSatisfy` isRight
