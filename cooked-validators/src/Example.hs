{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Typed.Scripts as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

-- Toy example: the "AContract" with "ADatum" and "ARedeemer"
--
-- Its validator succeeds when redeeming with "ARedeemer1" and fails with
-- "ARedeemer2".

data ADatum = ADatum deriving (Show)

instance Pl.Eq ADatum where
  ADatum == ADatum = True

Pl.makeLift ''ADatum
Pl.unstableMakeIsData ''ADatum

data ARedeemer = ARedeemer1 | ARedeemer2 deriving (Show)

instance Pl.Eq ARedeemer where
  ARedeemer1 == ARedeemer1 = True
  ARedeemer2 == ARedeemer2 = True
  _ == _ = False

Pl.makeLift ''ARedeemer
Pl.unstableMakeIsData ''ARedeemer

data AContract

instance Pl.ValidatorTypes AContract where
  type DatumType AContract = ADatum
  type RedeemerType AContract = ARedeemer

{-# INLINEABLE mkAValidator #-}
mkAValidator :: ADatum -> ARedeemer -> Pl.ScriptContext -> Bool
mkAValidator _ ARedeemer1 _ = True
mkAValidator _ ARedeemer2 _ = False

aValidator :: Pl.TypedValidator AContract
aValidator =
  Pl.mkTypedValidator @AContract
    $$(Pl.compile [||mkAValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator @ADatum @ARedeemer

-- Example endpoints

payEndpoint :: MonadBlockChain m => Integer -> Integer -> m SpendableOut
payEndpoint amountToPk amountToScript = do
  cardanoTx <-
    validateTxSkel $
      txSkel
        [ paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf amountToPk),
          PaysScript aValidator ADatum (Pl.lovelaceValueOf amountToScript)
        ]
  spOuts <- spOutsFromCardanoTx cardanoTx
  -- We return the second (index 1) utxo from the transaction outputs:
  -- the script output
  return (spOuts !! 1)

spendEndpoint :: MonadBlockChain m => SpendableOut -> ARedeemer -> m ()
spendEndpoint spOut redeemer =
  void $
    validateTxSkel $
      txSkel
        [SpendsScript aValidator redeemer spOut]

-- Example traces, one success, one failure

example1 :: MonadMockChain m => m ()
example1 = do
  spOut <- payEndpoint 42_000_000 5_000_000
  spendEndpoint spOut ARedeemer1

example2 :: MonadMockChain m => m ()
example2 = do
  spOut <- payEndpoint 42_000_000 5_000_000
  spendEndpoint spOut ARedeemer2

-- Test suite including a failing test to showcase pretty printing of
-- constraints

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Order"
    [ Tasty.testCase "Redeem a script output with ARedeemer1" $
        testSucceeds example1,
      Tasty.testCase "Redeem a script output with ARedeemer2" $
        testSucceeds example2
    ]

runTests :: IO ()
runTests = Tasty.defaultMain tests
