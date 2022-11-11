{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cooked.Tx.Constraints.TypeSpec where

import Control.Applicative
import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import Data.Default (def)
import qualified Ledger.Address as Pl
import qualified Ledger.Generators as LG
import qualified Ledger.Tx as Pl
import qualified Ledger.Typed.Scripts as Pl
import qualified Plutus.V1.Ledger.Api as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Builtins.Internal as Pl
import Test.QuickCheck.Hedgehog
import Test.Tasty
import Test.Tasty.QuickCheck

-- * Generators

-- ** Generating 'TxOpts'

instance Arbitrary RawModTx where
  arbitrary =
    oneof
      [ return $ RawModTxAfterBalancing id,
        return $ RawModTxBeforeBalancing id -- TODO: more options
      ]

instance Arbitrary Collateral where
  arbitrary =
    oneof
      [ return CollateralAuto,
        return $ CollateralUtxos mempty -- TODO: more options
      ]

instance Arbitrary BalanceOutputPolicy where
  arbitrary =
    oneof
      [ return AdjustExistingOutput,
        return DontAdjustExistingOutput
      ]

instance Arbitrary TxLabel where
  arbitrary =
    oneof
      [ TxLabel @String <$> arbitrary,
        TxLabel @Integer <$> arbitrary
      ]

instance Arbitrary TxOpts where
  arbitrary =
    TxOpts <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- ** Generating minting policies

-- At the moment, there is only one policy we generate:

{-# INLINEABLE mkMockPolicy #-}
mkMockPolicy :: Pl.BuiltinData -> Pl.BuiltinData -> ()
mkMockPolicy _ _ = ()

mockPolicy :: Pl.MintingPolicy
mockPolicy = Pl.mkMintingPolicyScript $$(Pl.compile [||mkMockPolicy||])

instance Arbitrary Pl.MintingPolicy where
  arbitrary = return mockPolicy

-- ** Generating 'MintsConstraint's

instance Arbitrary Pl.TokenName where
  arbitrary = hedgehog LG.genTokenName

instance Arbitrary MintsConstraint where
  arbitrary =
    oneof
      [ MintsConstraint @Bool <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
      ]

-- ** Generating 'Value's

instance Arbitrary Pl.Value where
  arbitrary = hedgehog LG.genValue

-- ** Generating 'Address'es

instance Arbitrary Pl.PubKeyHash where
  arbitrary = Pl.PubKeyHash <$> Pl.BuiltinByteString <$> arbitrary

instance Arbitrary Pl.StakePubKeyHash where
  arbitrary = Pl.StakePubKeyHash <$> arbitrary

genPKCredential :: Gen Pl.Credential
genPKCredential = Pl.PubKeyCredential <$> arbitrary

instance Arbitrary Pl.ValidatorHash where
  arbitrary = Pl.ValidatorHash <$> Pl.BuiltinByteString <$> arbitrary

genScriptCredential :: Gen Pl.Credential
genScriptCredential = Pl.ScriptCredential <$> arbitrary

instance Arbitrary Pl.Credential where
  arbitrary = oneof [genPKCredential, genScriptCredential]

instance Arbitrary Pl.StakingCredential where
  arbitrary =
    oneof
      [ Pl.StakingHash <$> arbitrary,
        Pl.StakingPtr <$> arbitrary <*> arbitrary <*> arbitrary
      ]

-- TODO: Do we have to impose some restrictions on the staking credentials in
-- 'genPKAddress' and 'genScriptAddress'?

genPKAddress :: Gen Pl.Address
genPKAddress = Pl.Address <$> genPKCredential <*> arbitrary

genScriptAddress :: Gen Pl.Address
genScriptAddress = Pl.Address <$> genScriptCredential <*> arbitrary

instance Arbitrary Pl.Address where
  arbitrary = oneof [genPKAddress, genScriptAddress]

-- ** Generating (typed) validators

type ADatum = Bool

type ARedeemer = Bool

data AContract

instance Pl.ValidatorTypes AContract where
  type DatumType AContract = ADatum
  type RedeemerType AContract = ARedeemer

{-# INLINEABLE mkAValidator #-}
mkAValidator :: ADatum -> ARedeemer -> Pl.ScriptContext -> Bool
mkAValidator _ _ _ = True

aValidator :: Pl.TypedValidator AContract
aValidator =
  Pl.mkTypedValidator @AContract
    $$(Pl.compile [||mkAValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator @ADatum @ARedeemer

instance Arbitrary (Pl.TypedValidator AContract) where
  arbitrary = return aValidator -- TODO more options

instance Arbitrary Pl.Validator where
  arbitrary = Pl.validatorScript @AContract <$> arbitrary

-- ** Generating datum (hashes)

instance Arbitrary Pl.Datum where
  arbitrary = Pl.Datum <$> Pl.toBuiltinData @Integer <$> arbitrary

instance Arbitrary Pl.DatumHash where
  arbitrary = Pl.DatumHash <$> Pl.BuiltinByteString <$> arbitrary

-- ** Generating 'SpendableOut's

instance Arbitrary Pl.TxId where
  arbitrary = Pl.TxId <$> Pl.BuiltinByteString <$> arbitrary

instance Arbitrary Pl.TxOutRef where
  arbitrary = Pl.TxOutRef <$> arbitrary <*> arbitrary

genPKChainIndexTxOut :: Gen Pl.ChainIndexTxOut
genPKChainIndexTxOut = Pl.PublicKeyChainIndexTxOut <$> arbitrary <*> arbitrary

genScriptChainIndexTxOut :: Gen Pl.ChainIndexTxOut
genScriptChainIndexTxOut =
  Pl.ScriptChainIndexTxOut <$> genScriptAddress
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

genPKSpendableOut :: Gen SpendableOut
genPKSpendableOut = SpendableOut <$> arbitrary <*> genPKChainIndexTxOut

genScriptSpendableOut :: Gen SpendableOut
genScriptSpendableOut = SpendableOut <$> arbitrary <*> genScriptChainIndexTxOut

-- ** Generating 'InConstraint's

instance Arbitrary InConstraint where
  arbitrary =
    oneof
      [ SpendsPK <$> genPKSpendableOut,
        SpendsScript @AContract <$> arbitrary -- TODO more options for the validator
          <*> arbitrary
          <*> genScriptSpendableOut
      ]

-- ** Generating 'OutConstraint's

instance Arbitrary OutConstraint where
  arbitrary =
    oneof
      [ PaysPK @Bool <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
      ]

-- ** Generating 'TxSkel's

instance Arbitrary TxSkel where
  arbitrary =
    TxSkel <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (hedgehog $ LG.genTimeRange def)
      <*> arbitrary
      <*> arbitrary

instance Show TxSkel where
  show = show . prettyTxSkel []

tests :: TestTree
tests =
  testGroup
    "testing TxSkel and friends"
    [ testProperty "== is a congruence for <> on TxSkels" $
        \(a :: TxSkel) b x y -> (a == x && b == y) <= (a <> b == x <> y)
    ]
