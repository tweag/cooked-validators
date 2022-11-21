{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cooked.Tx.Constraints.TypeSpec where

import Control.Applicative
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoState
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import Data.Default (def)
import Data.Either
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void
import Debug.Trace
import qualified Ledger.Ada as Pl (toValue)
import qualified Ledger.Address as Pl
import qualified Ledger.Constraints.OffChain as Pl
import qualified Ledger.Constraints.TxConstraints as Pl
import qualified Ledger.Generators as LG
import qualified Ledger.Tx as Pl
import qualified Ledger.Typed.Scripts as Pl
import Optics.Core
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

-- At the moment, we generate only two different policies:

{-# INLINEABLE mkMockPolicy1 #-}
mkMockPolicy1 :: Pl.BuiltinData -> Pl.BuiltinData -> ()
mkMockPolicy1 _ _ = ()

mockPolicy1 :: Pl.MintingPolicy
mockPolicy1 = Pl.mkMintingPolicyScript $$(Pl.compile [||mkMockPolicy1||])

instance Arbitrary Pl.MintingPolicy where
  arbitrary = return mockPolicy1

-- ** Generating 'MintsConstraint's

instance Arbitrary Pl.TokenName where
  arbitrary = hedgehog LG.genTokenName

instance Arbitrary MintsConstraint where
  arbitrary =
    oneof
      [ MintsWithRedeemer @Bool <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary,
        Mints <$> arbitrary
          <*> arbitrary
          <*> arbitrary
      ]

-- ** Generating 'Value's

instance Arbitrary Pl.Value where
  arbitrary = Pl.toValue <$> hedgehog LG.genAda --hedgehog LG.genValueNonNegative -- LG.genValue

-- ** Generating 'Address'es

instance Arbitrary Pl.PubKeyHash where
  -- Public Key Hashes have length 28
  arbitrary = Pl.PubKeyHash <$> Pl.BuiltinByteString <$> hedgehog (LG.genSizedByteStringExact 28)

instance Arbitrary Pl.PaymentPubKeyHash where
  arbitrary = Pl.PaymentPubKeyHash <$> arbitrary

instance Arbitrary Pl.StakePubKeyHash where
  arbitrary = Pl.StakePubKeyHash <$> arbitrary

genPKCredential :: Gen Pl.Credential
genPKCredential = Pl.PubKeyCredential <$> arbitrary

instance Arbitrary Pl.ValidatorHash where
  arbitrary = Pl.ValidatorHash <$> Pl.BuiltinByteString <$> hedgehog (LG.genSizedByteStringExact 28)

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
  arbitrary = Pl.DatumHash <$> Pl.BuiltinByteString <$> hedgehog (LG.genSizedByteStringExact 28)

-- ** Generating 'SpendableOut's

instance Arbitrary Pl.TxId where
  -- These hashes also have to have length 28
  arbitrary = Pl.TxId <$> Pl.BuiltinByteString <$> hedgehog (LG.genSizedByteStringExact 28)

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
    TxSkel mempty <$> arbitrary
      <*> arbitrary
      <*> (hedgehog $ LG.genTimeRange def)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

genTxSkelDefaultOptionsNoLabel :: Gen TxSkel
genTxSkelDefaultOptionsNoLabel =
  TxSkel mempty def
    <$> arbitrary
    <*> (hedgehog $ LG.genTimeRange def)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

-- generate a 'TxSkel' @x@ such that @runMockChain . generateTx $ x@ is a
-- non-error with high probability
genPassingTxSkel :: Gen TxSkel
genPassingTxSkel = do
  options <- arbitrary
  mints <- arbitrary
  let mintedValue = foldMap mintsValue mints
  TxSkel
    mempty -- labels don't matter here
    options
    mints
    <$> (hedgehog $ LG.genTimeRange def) -- validity Time range
    <*> arbitrary -- required signers
    <*> arbitrary -- inputs
    <*> arbitrary -- outputs

-- * A few instances for QuickCheck

instance Show MintsConstraint where
  show = show . prettyMintsConstraint

instance Show InConstraint where
  show = show . prettyInConstraint

instance Show OutConstraint where
  show = show . prettyOutConstraint

instance Show TxSkel where
  show = show . prettyTxSkel []

deriving instance (Eq i, Eq o) => Eq (Pl.TxConstraints i o)

deriving instance Eq (Pl.TxConstraintFuns)

instance Eq Pl.TxConstraintFun where
  _ == _ = False

deriving instance Eq (Pl.ScriptLookups a)

-- * The tests

tests :: TestTree
tests =
  testGroup
    "testing TxSkel and friends"
    [ -- testGroup "== is a congruence for <>" $
      --   let congruenceOn :: (Eq a, Semigroup a) => a -> a -> a -> a -> Property
      --       congruenceOn a b x y =
      --         -- The usual definition of the congruence property would be
      --         --
      --         -- > (a == x && b == y) ==> (a <> b == x <> y)
      --         --
      --         -- but this forces us to discard so many test cases that
      --         -- QuickCheck gives up. A contrapositive formulation is better,
      --         -- because the premise is easier to satisfy.
      --         (a <> b /= x <> y) ==> (a /= x || b /= y)
      --    in [ testProperty "on sets of TxLabel" $ congruenceOn @(Set TxLabel),
      --         testProperty "on TxOpts" $ congruenceOn @TxOpts,
      --         testProperty "on TxSkel" $ congruenceOn @TxSkel
      --       ],
      -- -- TODO: Tests for other laws about <>?
      -- testGroup "toLedgerConstraints is injective" $
      --   let injectiveOn :: (Eq a, ToLedgerConstraint a) => a -> a -> Property
      --       injectiveOn x y =
      --         -- Again, a contrapositive formulation of the usual property is
      --         -- necessary.
      --         (x /= y)
      --           ==> (toLedgerConstraint @_ @Void x /= toLedgerConstraint y)
      --    in [ testProperty "on MintsConstraint" $ injectiveOn @MintsConstraint,
      --         testProperty "on InConstraint" $ injectiveOn @InConstraint,
      --         testProperty "on OutConstraint" $ injectiveOn @OutConstraint,
      --         testProperty "on TxSkel" $
      --           forAll genTxSkelDefaultOptionsNoLabel $ \a ->
      --             forAll genTxSkelDefaultOptionsNoLabel $ \b ->
      --               injectiveOn a b
      --       ],
      testProperty "always valid if only MintsConstraints" $
        \a -> isRight $ runMockChain $ generateTx' $ mempty & txSkelMints .~ a,
      testGroup
        "generateTx' is injective"
        $ [ testGroup "if there are only MintsConstraints" $
              let isEmptyMint :: Set MintsConstraint -> Bool
                  isEmptyMint mcs = foldMap mintsValue mcs == mempty
               in [ testProperty "non-empty difference implies different transaction" $
                      \a b ->
                        let txA = runMockChain $ generateTx' $ mempty & txSkelMints .~ a
                            txB = runMockChain $ generateTx' $ mempty & txSkelMints .~ (a <> b)
                         in not (isEmptyMint $ b S.\\ a) ==> txA /= txB -- ,
                        -- testProperty "empty difference implies same transaction" $
                        --   \a b ->
                        --     let
                        --           txA = runMockChain $ generateTx' $ mempty & txSkelMints .~ a
                        --         txB = runMockChain $ generateTx' $ mempty & txSkelMints .~ (a <> b)
                        --      in (isEmptyMint $ b S.\\ a) ==> txA == txB
                  ]
                  -- HMMM?
                  -- testing TxSkel and friends
                  -- generateTx' is injective
                  --   if there are only MintsConstraints: FAIL (0.04s)
                  --     *** Failed! Falsified (after 4 tests and 1 shrink):
                  --     fromList [Mints
                  --      - Redeemer: True
                  --      - Policy: 363d39
                  --      - Value: (363d394 $ "") : -2]
                  --     fromList [Mints
                  --      - Policy: 363d39
                  --      - Value: (363d394 $ "") : -3,Mints
                  --      - Redeemer: True
                  --      - Policy: 363d39
                  --      - Value: (363d394 $ "") : 1]
                  --     Use --quickcheck-replay=167109 to reproduce.
          ]
    ]
