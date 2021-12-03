{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PMultiSigStatefulSpec where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Writer
import Cooked.MockChain
import Cooked.MockChain.Monad.Staged
import Cooked.Tx.Constraints
import Data.Either (isLeft, isRight)
import Data.List (nub)
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import PMultiSigStateful
import qualified Plutus.V1.Ledger.Value as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Hspec hiding (after)
import qualified Test.QuickCheck as QC
import Test.QuickCheck.GenT
import qualified Test.QuickCheck.Monadic as QC
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.MetadataReporter
import qualified Test.Tasty.Metadata as TW
import Test.Tasty.QuickCheck (testProperty)
import Text.Heredoc

-- * Writing a Test Suite

-- $writingtestsuit
--
-- Start by writing the code that interacts with the system; this is likely a copy
-- of part of the client's off-chain code (everything in the @Contract@ monad).

-- ** Interacting with PMultiSigStateful

-- | Initializes the system and returns the parameters; in this case,
--  initialization is simple. All we have to do is create the thread token
--  that gets passed to the proposal.
mkParams :: MonadMockChain m => m Params
mkParams = do
  out <- mkThreadToken (wallet 1)
  pure $ Params (walletPK <$> knownWallets) 2 $ threadTokenAssetClass out
  where
    mkThreadTokenInputSkel :: Wallet -> TxSkel
    mkThreadTokenInputSkel w = txSkel w [PaysPK (walletPKHash w) mempty]

    mkThreadToken :: MonadMockChain m => Wallet -> m Pl.TxOutRef
    mkThreadToken w = do
      validateTxFromSkeleton $ mkThreadTokenInputSkel w
      emptyUtxos <- pkUtxosSuchThat (walletPKHash w) (== mempty)
      case emptyUtxos of
        [] -> error "No empty UTxOs"
        ((out, _) : _) -> pure out

-- | This is a label that gets attached to the mkProposal transaction; it helps
--  us identify what that transaction is doing without carefully inspecting its
--  constraints.
data ProposalSkel = ProposalSkel Integer Payment
  deriving (Show)

-- | Next, we can create proposals, which are simply an accumulator with no signees
mkProposal :: MonadMockChain m => Integer -> Wallet -> Payment -> m (Params, Pl.TxOutRef)
mkProposal reqSigs w pmt = do
  utxos <- pkUtxos wpkh
  case utxos of
    (spendableOut : _) -> do
      let klass = threadTokenAssetClass $ fst spendableOut
      let params = Params (walletPK <$> knownWallets) reqSigs klass
      let threadToken = paramsToken params
      let skel =
            txSkelLbl
              (ProposalSkel reqSigs pmt)
              w
              [ Mints [threadTokenPolicy (fst spendableOut) threadTokenName] threadToken,
                -- We don't have SpendsPK or PaysPK wrt the wallet `w`
                -- because the balancing mechanism chooses the same (first) output
                -- we're working on.
                PaysScript (pmultisig params) [(Accumulator pmt [], paymentValue pmt <> threadToken)]
              ]
      validateTxSkel skel
      pure (params, fst spendableOut)
    _ -> error "No spendable outputs for the wallet"
  where
    wpkh = walletPKHash w

mkSign :: MonadMockChain m => Params -> Wallet -> Payment -> m ()
mkSign params w pmt =
  validateTxSkel $
    txSkel w [PaysScript (pmultisig params) [(Sign pk sig, mempty)]]
  where
    pk = walletPK w
    sig = Pl.sign (Pl.sha2_256 $ packPayment pmt) (walletSK w)

mkCollect :: MonadMockChain m => Payment -> Params -> m ()
mkCollect thePayment params = do
  [initialProp] <- scriptUtxosSuchThat (pmultisig params) isProposal
  signatures <- scriptUtxosSuchThat (pmultisig params) isSign
  validateTxSkel $
    txSkel (wallet 1) $
      PaysScript
        (pmultisig params)
        [ ( Accumulator thePayment (signPk . snd <$> signatures),
            (paymentValue thePayment) <> paramsToken params
          )
        ] :
      SpendsScript (pmultisig params) () initialProp :
      (SpendsScript (pmultisig params) () <$> signatures)

mkPay :: MonadMockChain m => Payment -> Params -> Pl.TxOutRef -> m ()
mkPay thePayment params tokenOutRef = do
  [accumulated] <- scriptUtxosSuchThat (pmultisig params) isAccumulator
  validateTxSkel $
    txSkel
      (wallet 1)
      [ PaysPK (paymentRecipient thePayment) (paymentValue thePayment),
        SpendsScript (pmultisig params) () accumulated,
        Mints [threadTokenPolicy tokenOutRef threadTokenName] $ Pl.negate $ paramsToken params
      ]

-- *** Auxiliary Functions

isSign :: Datum -> a -> Bool
isSign Sign {} _ = True
isSign _ _ = False

isAccumulator :: Datum -> a -> Bool
isAccumulator Accumulator {} _ = True
isAccumulator _ _ = False

isProposal :: Datum -> a -> Bool
isProposal (Accumulator _ []) _ = True
isProposal _ _ = False

-- ** Test Cases

tests :: TestTree
tests =
  testGroup
    "PMultiSigStateful"
    [ sampleTrace1,
      sampleGroup1
    ]

sampleTrace1 :: TestTree
sampleTrace1 =
  TW.bug
    TW.Critical
    [str|Ensuring that the PMultiSigContract can be used in a trivial scenario where
      |wallet 1 proposes a payment, which receives three signatures, then is
      |\emph{collected} and \emph{payed}.
      |]
    $ testCase "Example Trivial Trace" $ isRight (runMockChain trace) @? "Trace failed"
  where
    trace :: MonadMockChain m => m ()
    trace = do
      (params, tokenOutRef) <- mkProposal 2 (wallet 1) thePayment
      mkSign params (wallet 1) thePayment
      mkSign params (wallet 2) thePayment
      mkSign params (wallet 3) thePayment
      mkCollect thePayment params
      mkPay thePayment params tokenOutRef
      where
        thePayment = Payment 4200 (walletPKHash $ last knownWallets)

sampleGroup1 :: TestTree
sampleGroup1 =
  testGroup
    "Property-based Test Examples"
    [ TW.bug'
        "sec:simple-traces"
        TW.Critical
        [str|Traces that use enough unique signatures should always succeed|]
        $ testProperty "Can execute payment with enough signatures" $
          forAllTrP
            successParams
            (\p -> mkProposalForParams p >>= mkTraceForParams p)
            (const (QC.property . isRight)),
      TW.bug
        TW.Critical
        [str|On the other hand, if we do \emph{not} collect enough unique
          |signatures, the validator should block the payment.
          |]
        $ testProperty "Cannot execute payment without enough signatures" $
          forAllTrP
            failureParams
            (\p -> mkProposalForParams p >>= mkTraceForParams p)
            (const (QC.property . isLeft)),
      TW.bug
        TW.Critical
        [str|On successful traces, it must be impossible to duplicate the
          |authentication token
          |]
        $ testProperty "Cannot duplicate token over \\Cref{sec:simple-traces}" $
          forAllTrP
            successParams
            ( \p ->
                mkProposalForParams p
                  >>= \i -> somewhere (dupTokenAttack i) (mkTraceForParams p i)
            )
            (const (QC.property . isLeft))
    ]

data ThresholdParams = ThresholdParams
  { reqSigs :: Integer,
    sigWallets :: [Int],
    proposerWallet :: Wallet,
    pmt :: Payment
  }
  deriving (Show)

numUniqueSigs :: ThresholdParams -> Integer
numUniqueSigs = fromIntegral . length . nub . sigWallets

anyParams :: QC.Gen ThresholdParams
anyParams =
  ThresholdParams
    <$> choose (1, 5)
    <*> (resize 5 $ listOf (choose (1, 8)))
    <*> (wallet <$> choose (1, 8))
    <*> (Payment <$> choose (1200, 4200) <*> (walletPKHash . wallet <$> choose (1, 8)))

successParams :: QC.Gen ThresholdParams
successParams = anyParams `QC.suchThat` (\p -> numUniqueSigs p >= reqSigs p)

failureParams :: QC.Gen ThresholdParams
failureParams = anyParams `QC.suchThat` (\p -> numUniqueSigs p < reqSigs p)

mkProposalForParams :: MonadMockChain m => ThresholdParams -> GenT m (Params, Pl.TxOutRef)
mkProposalForParams parms = mkProposal (reqSigs parms) (proposerWallet parms) (pmt parms)

mkTraceForParams :: MonadMockChain m => ThresholdParams -> (Params, Pl.TxOutRef) -> GenT m ()
mkTraceForParams tParms (parms, tokenRef) = do
  forM_ (sigWallets tParms) $ \wId -> mkSign parms (wallet wId) (pmt tParms)
  mkCollect (pmt tParms) parms
  mkPay (pmt tParms) parms tokenRef

data DupTokenAttacked where
  DupTokenAttacked :: (Show x) => Maybe x -> DupTokenAttacked

deriving instance Show DupTokenAttacked

dupTokenAttack :: (Params, Pl.TxOutRef) -> TxSkel -> Maybe TxSkel
dupTokenAttack (parms, tokenRef) (TxSkel l s cs) =
  Just $ TxSkel (Just $ (DupTokenAttacked l)) s (attack : cs)
  where
    attack = Mints [threadTokenPolicy tokenRef threadTokenName] (paramsToken parms)
