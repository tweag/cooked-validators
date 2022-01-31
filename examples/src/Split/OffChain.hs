{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Split.OffChain where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Ledger as Pl
import qualified Ledger.Typed.Scripts as Pl
import Playground.Contract
import qualified Plutus.Contract as C
import qualified Plutus.V1.Ledger.Ada as Pl
import Split
import qualified Wallet.Emulator.Wallet as C

-- * Transaction Skeleton Generators

-- | Transaction to lock some amount into the split contract; note that
-- we receive the split contract as parameter because we use this same function
-- in the @tests/SplitSpec.hs@ and @tests/SplitUPLCSpec.hs@. The later loads
-- the split contract as a raw untyped PlutusCore contract.
txLock :: MonadBlockChain m => Pl.TypedValidator Split -> SplitDatum -> m ()
txLock script datum =
  void $
    validateTxConstrLbl
      (TxLock datum)
      [ PaysScript
          script
          [ ( datum,
              Pl.lovelaceValueOf $ Split.amount datum
            )
          ]
      ]

-- | Label for 'txLock' skeleton, making it immediately recognizable
-- when printing traces.
newtype TxLock = TxLock SplitDatum deriving (Show)

-- | Whether a script output concerns a public key hash
isARecipient :: Pl.PubKeyHash -> SplitDatum -> a -> Bool
isARecipient pkh datum _ = pkh `elem` [Split.recipient1 datum, Split.recipient2 datum]

-- | Unlocks the first 'SplitDatum' where our wallet is a recipient of.
txUnlock :: (MonadBlockChain m) => Pl.TypedValidator Split -> m ()
txUnlock script = do
  pkh <- ownPaymentPubKeyHash
  (output, datum@(Split.SplitDatum r1 r2 amount)) : _ <-
    scriptUtxosSuchThat script (isARecipient pkh)
  let half = div amount 2
  let share1 = half
  let share2 = amount - half
  void $
    validateTxConstrLbl
      TxUnlock
      [ SpendsScript script () (output, datum),
        PaysPK r1 (Pl.lovelaceValueOf share1),
        PaysPK r2 (Pl.lovelaceValueOf share2)
      ]

-- | Label for 'txUnlock' skeleton
data TxUnlock = TxUnlock deriving (Show)

-- * Contract monad endpoints and schema

data LockArgs = LockArgs
  { recipient1Wallet :: C.Wallet,
    recipient2Wallet :: C.Wallet,
    totalAda :: Pl.Ada
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type SplitSchema = C.Endpoint "lock" LockArgs C..\/ C.Endpoint "unlock" ()

mkSchemaDefinitions ''SplitSchema

mkSplitData :: LockArgs -> SplitDatum
mkSplitData LockArgs {recipient1Wallet, recipient2Wallet, totalAda} =
  let convert :: C.Wallet -> Pl.PubKeyHash
      convert = Pl.pubKeyHash . C.walletPubKey
   in SplitDatum
        { recipient1 = convert recipient1Wallet,
          recipient2 = convert recipient2Wallet,
          amount = fromIntegral totalAda
        }

endpoints :: (C.AsContractError e) => C.Promise w SplitSchema e ()
endpoints =
  C.endpoint @"lock" (txLock splitValidator . mkSplitData)
    `C.select` C.endpoint @"unlock" (const $ txUnlock splitValidator)
