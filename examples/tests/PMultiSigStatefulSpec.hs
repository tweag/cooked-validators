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

module PMultiSigStatefulSpec where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Either (isLeft, isRight)
import Data.Function (on)
import Data.List (nub, nubBy)
import qualified Ledger as Pl
import PMultiSigStateful
import qualified PlutusTx.Prelude as Pl
import qualified Test.QuickCheck as QC
import Test.QuickCheck.GenT
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.Metadata as TW
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
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
      void $ validateTxSkel $ mkThreadTokenInputSkel w
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
              [ mints [threadTokenPolicy (fst spendableOut) threadTokenName] threadToken,
                -- We don't have SpendsPK or PaysPK wrt the wallet `w`
                -- because the balancing mechanism chooses the same (first) output
                -- we're working on.
                PaysScript (pmultisig params) [(Accumulator pmt [], paymentValue pmt <> threadToken)]
              ]
      void $ validateTxSkel skel
      pure (params, fst spendableOut)
    _ -> error "No spendable outputs for the wallet"
  where
    wpkh = walletPKHash w

mkSign :: MonadMockChain m => Params -> Wallet -> Payment -> m ()
mkSign params w pmt =
  void $
    validateTxSkel $
      txSkel w [PaysScript (pmultisig params) [(Sign pk sig, mempty)]]
  where
    pk = walletPK w
    sig = Pl.sign (Pl.sha2_256 $ packPayment pmt) (walletSK w)

mkCollect :: MonadMockChain m => Payment -> Params -> m ()
mkCollect thePayment params = do
  [initialProp] <- scriptUtxosSuchThat (pmultisig params) isProposal
  signatures <- nubBy ((==) `on` snd) <$> scriptUtxosSuchThat (pmultisig params) isSign
  void $
    validateTxSkel $
      txSkel (wallet 1) $
        PaysScript
          (pmultisig params)
          [ ( Accumulator thePayment (signPk . snd <$> signatures),
              paymentValue thePayment <> paramsToken params
            )
          ] :
        SpendsScript (pmultisig params) () initialProp :
        (SpendsScript (pmultisig params) () <$> signatures)

mkPay :: MonadMockChain m => Payment -> Params -> Pl.TxOutRef -> m ()
mkPay thePayment params tokenOutRef = do
  [accumulated] <- scriptUtxosSuchThat (pmultisig params) isAccumulator
  void $
    validateTxSkel $
      txSkel
        (wallet 1)
        [ PaysPK (paymentRecipient thePayment) (paymentValue thePayment),
          SpendsScript (pmultisig params) () accumulated,
          mints [threadTokenPolicy tokenOutRef threadTokenName] $ Pl.negate $ paramsToken params
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

qcIsRight :: (Show a) => Either a b -> QC.Property
qcIsRight (Left a) = QC.counterexample (show a) False
qcIsRight (Right _) = QC.property True

-- | This is a test tree with quickcheck properties. A word of caution is necessary as the
--  semantics of 'GenT' and 'somewhere'/'everywhere' can be easily confused and lead to
--  tests that take way too long to run.
--
--  TL;DR: if using 'somewhere' in your test, make sure to set
--  @localOption (QuickCheckTests x)@ for some x smaller than the default (100).
--
--  What is happening, though? Well, say we write a trace:
--
--  > tr :: (MonadMockChain m) => GenT m ()
--  > tr = do
--  >   x <- choose (0, 10)
--  >   y <- choose (0, 10)
--  >   validateTxFromSkel (mkTx x)
--  >   validateTxFromSkel (mkTx y)
--
--  If interpreted with @m ~ StagedMockChain@, the @tr@ above denotes a
--  probability distribution over functions that return a list of possible outcomes,
--  that is:
--
--  > interpret (runGenT tr)
--  >   :: Gen (MockChainT (WriterT TraceDesc []) ())
--
--  Now thats a mouthful of monad transformers to look at, if we expand them all we get:
--
--  > Gen (MockChainSt -> [(Either MockChainErr (), TraceDesc)])
--
--  In the particular case of tr, above, we get a probability distribution of
--  traces that return only one possible result. The probability distribution is
--  over the choice of values for @x@ and @y@. Regardless of that, the trace produces
--  a single result which consists of validating transactions @mkTx x@ and @mkTx y@.
--
--  Now say we change @tr@ to:
--
--  > tr2 :: (MonadMockChain m) => GenT m ()
--  > tr2 = do
--  >   x <- choose (0, 10)
--  >   y <- choose (0, 10)
--  >   somewhere mod $ do
--  >     validateTxFromSkel (mkTx x)
--  >     validateTxFromSkel (mkTx y)
--
--  Now, the probability distribution is still over the choice of values for @x@ and @y@,
--  but for any given such choice, we return a function that given an initial state
--  returns /two/ possible results:
--
--  1. One where we validate transactions @mod (mkTx x)@ and @mkTx y@
--  2. One where we validate transactions @mkTx x@ and @mod (mkTx y)@
--
--  When using the 'forAllTr' and 'forAllTrP' combinators, we're actualy
--  testing the 'QC.conjoin' of all possible universes returned by trace.
--  Hence, if we run the following 'testTree' with default options:
--
--  > testProperty "propName" $ forAllTr tr2 somePredicate
--
--  QuickCheck will run the property 100 times, but each of those runs
--  we will be checking two different traces. So all in all, we will look
--  at 200 traces. A more reasonable option is to use:
--
--  > localOption (QuickCheckTests 25) $
--  >   testProperty "propName" $ forAllTr tr2 somePredicate
sampleGroup1 :: TestTree
sampleGroup1 =
  localOption (QuickCheckTests 25) $
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
              (const qcIsRight),
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
    <*> resize 5 (listOf (choose (1, 8)))
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
  Just $ TxSkel (Just $ DupTokenAttacked l) s (attack : cs)
  where
    attack = mints [threadTokenPolicy tokenRef threadTokenName] (paramsToken parms)
