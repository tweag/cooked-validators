{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module PMultiSigStatefulSpec where

import Control.Arrow (second)
import Control.Monad
import Cooked.MockChain
import Cooked.MockChain.HUnit
import Cooked.Tx.Constraints
import Data.Default
import Data.Either (isLeft, isRight)
import Data.Function (on)
import Data.List (nub, nubBy)
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Typed.Scripts as Pl
import PMultiSigStateful
import qualified PMultiSigStateful.DatumHijacking as HJ
import PMultiSigStateful.ToUPLC
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.IsData.Class as Pl
import qualified PlutusTx.Prelude as Pl
import qualified Test.QuickCheck as QC
import Test.QuickCheck.GenT
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
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
mkParams :: MonadBlockChain m => m Params
mkParams =
  Params pkTable 2 . threadTokenAssetClass <$> mkThreadToken
  where
    mkThreadTokenInputSkel :: Pl.PubKeyHash -> TxSkel
    mkThreadTokenInputSkel pkh = txSkel [PaysPK pkh mempty]

    mkThreadToken :: MonadBlockChain m => m Pl.TxOutRef
    mkThreadToken = do
      pkh <- ownPaymentPubKeyHash
      void $ validateTxSkel $ mkThreadTokenInputSkel pkh
      emptyUtxos <- pkUtxosSuchThat pkh (== mempty)
      case emptyUtxos of
        [] -> error "No empty UTxOs"
        ((out, _) : _) -> pure out

pkTable :: AssocMap.Map Pl.PubKeyHash Pl.PubKey
pkTable = AssocMap.fromList $ map (\w -> (walletPKHash w, walletPK w)) knownWallets

-- | This is a label that gets attached to the mkProposal transaction; it helps
--  us identify what that transaction is doing without carefully inspecting its
--  constraints.
data ProposalSkel = ProposalSkel Integer Payment
  deriving (Show)

-- | Next, we can create proposals, which are simply an accumulator with no signees
mkProposal :: MonadBlockChain m => Integer -> Payment -> m (Params, Pl.TxOutRef)
mkProposal reqSigs pmt = do
  wpkh <- ownPaymentPubKeyHash
  utxos <- pkUtxos wpkh
  case utxos of
    (spendableOut : _) -> do
      let klass = threadTokenAssetClass $ fst spendableOut
      let params = Params pkTable reqSigs klass
      let threadToken = paramsToken params
      _ <-
        validateTxConstrLbl
          (ProposalSkel reqSigs pmt)
          [ mints [threadTokenPolicy (fst spendableOut) threadTokenName] threadToken,
            -- We don't have SpendsPK or PaysPK wrt the wallet `w`
            -- because the balancing mechanism chooses the same (first) output
            -- we're working on.
            PaysScript
              (pmultisig params)
              [ ( Accumulator pmt [],
                  minAda <> paymentValue pmt <> threadToken
                )
              ]
          ]
      pure (params, fst spendableOut)
    _ -> error "No spendable outputs for the wallet"

-- | Creates a signature UTxO, signaling that the wallet holder agrees
--   with the payment. Adding a signature locks the minimum amount of Ada
--   in this UTxO; that value should ideally later be returned to the signer, but
--   instead we'll give all of the value to the receiver of the payment, as a tip
--   on top of their payment. This is to avoid complex scenarios where someone signed
--   twice, or even an attack where the attacker would execute the mkPay transaction
--   but keep all the locked ada to themselves.
mkSign :: MonadBlockChain m => Params -> Payment -> Pl.PrivateKey -> m ()
mkSign params pmt sk = do
  pkh <- ownPaymentPubKeyHash
  void $
    validateTxConstrOpts
      (def {adjustUnbalTx = True})
      [PaysScript (pmultisig params) [(Sign pkh sig, mkSignLockedCost)]]
  where
    sig = Pl.sign (Pl.sha2_256 $ packPayment pmt) sk

    -- Whenever a wallet is signing a payment, it must lock away a certain amount of ada
    -- in an UTxO, otherwise, the Sign UTxO can't be created.
    -- Hence, we'll create the script utxo with 1 lovelace but we'll validate with adjustUnbalTx
    -- set to @True@, which will increase this value to however many lovelace are currently needed.
    mkSignLockedCost :: Pl.Value
    mkSignLockedCost = Pl.lovelaceValueOf 1

minAda :: Pl.Value
minAda = Pl.lovelaceValueOf 2000000

mkCollect :: MonadMockChain m => Payment -> Params -> m ()
mkCollect thePayment params = signs (wallet 1) $ do
  [initialProp] <- scriptUtxosSuchThat (pmultisig params) isProposal
  signatures <- nubBy ((==) `on` snd) <$> scriptUtxosSuchThat (pmultisig params) isSign
  let signatureValues = mconcat $ map (sOutValue . fst) signatures
  void $
    validateTxConstr $
      PaysScript
        (pmultisig params)
        [ ( Accumulator thePayment (signPk . snd <$> signatures),
            paymentValue thePayment <> sOutValue (fst initialProp) <> signatureValues
          )
        ] :
      SpendsScript (pmultisig params) () initialProp :
      (SpendsScript (pmultisig params) () <$> signatures)

mkPay :: MonadMockChain m => Payment -> Params -> Pl.TxOutRef -> m ()
mkPay thePayment params tokenOutRef = signs (wallet 1) $ do
  [accumulated] <- scriptUtxosSuchThat (pmultisig params) isAccumulator
  void $
    validateTxConstr
      -- We payout all the gathered funds to the receiver of the payment, including the minimum ada
      -- locked in all the sign UTxOs, which was accumulated in the Accumulate datum.
      [ PaysPK (paymentRecipient thePayment) (sOutValue (fst accumulated) <> Pl.negate (paramsToken params)),
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
      sampleGroup1,
      datumHijackingTrace
    ]

sampleTrace1 :: TestTree
sampleTrace1 =
  testCase "Example Trivial Trace" $ assertSucceeds mtrace
  where
    mtrace :: MonadMockChain m => m ()
    mtrace = do
      (params, tokenOutRef) <- mkProposal 2 thePayment `as` wallet 1
      mkSign params thePayment (walletSK $ wallet 1) `as` wallet 1
      mkSign params thePayment (walletSK $ wallet 2) `as` wallet 2
      mkSign params thePayment (walletSK $ wallet 3) `as` wallet 3
      mkCollect thePayment params
      mkPay thePayment params tokenOutRef
      where
        thePayment = Payment 4200000 (walletPKHash $ last knownWallets)

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
--  > tr :: (MonadBlockChain m) => GenT m ()
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
--  > tr2 :: (MonadBlockChain m) => GenT m ()
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
      [ testProperty "Can execute payment with enough signatures" $
          forAllTrP
            successParams
            (\p -> propose p >>= execute p)
            (const qcIsRight),
        testProperty "Cannot execute payment without enough signatures" $
          forAllTrP
            failureParams
            (\p -> propose p >>= execute p)
            (const (QC.property . isLeft)),
        testProperty "Cannot duplicate token over \\Cref{sec:simple-traces}" $
          forAllTrP
            successParams
            ( \p -> do
                i <- propose p
                w3utxos <- pkUtxos (walletPKHash $ wallet 9)
                somewhere (dupTokenAttack (head w3utxos) i) (execute p i)
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
successParams =
  anyParams
    `QC.suchThat` ( \p ->
                      let u = numUniqueSigs p
                       in u >= reqSigs p
                            && u == fromIntegral (length (sigWallets p))
                  )

failureParams :: QC.Gen ThresholdParams
failureParams = anyParams `QC.suchThat` (\p -> numUniqueSigs p < reqSigs p)

propose :: MonadMockChain m => ThresholdParams -> GenT m (Params, Pl.TxOutRef)
propose parms = mkProposal (reqSigs parms) (pmt parms) `as` proposerWallet parms

execute :: MonadMockChain m => ThresholdParams -> (Params, Pl.TxOutRef) -> GenT m ()
execute tParms (parms, tokenRef) = do
  forM_ (sigWallets tParms) $ \wId -> mkSign parms (pmt tParms) (walletSK $ wallet wId) `as` wallet wId
  mkCollect (pmt tParms) parms
  mkPay (pmt tParms) parms tokenRef

-- ** Auth Token Ducplication Attack

-- Modifies a transaction skeleton by attempting to mint one more provenance token.
dupTokenAttack :: SpendableOut -> (Params, Pl.TxOutRef) -> TxSkel -> Maybe TxSkel
dupTokenAttack sOut (parms, tokenRef) (TxSkel l opts cs) =
  Just $ TxSkel (Just $ DupTokenAttacked l) opts (cs ++ attack)
  where
    attack =
      [ mints [threadTokenPolicy tokenRef threadTokenName] (paramsToken parms),
        SpendsPK sOut,
        signedByWallets [wallet 9],
        PaysPK (walletPKHash $ wallet 9) (paramsToken parms <> sOutValue sOut)
      ]

data DupTokenAttacked where
  DupTokenAttacked :: (Show x) => Maybe x -> DupTokenAttacked

deriving instance Show DupTokenAttacked

-- ** Datum Hijacking Attack

-- Performs a datum-hijacking attack by using a fake validator with an
-- isomorphic datum type. This attack can be used in scripts that forget
-- to check the address of an output. Most of the times, if a script
-- uses 'txInfoOutputs' recklessly, instead of 'getContinuingOutputs',
-- chances are said script will be vulnerable.
datumHijackingTrace :: TestTree
datumHijackingTrace = testCase "not vulnerable to datum hijacking" $ assertFails datumHijacking

attacker :: Wallet
attacker = wallet 9

-- checks are done on the datum of the contract instead of the address of it.
fakeValidator :: Pl.TypedValidator HJ.Stealer
fakeValidator = HJ.stealerValidator $ HJ.StealerParams (walletPKHash $ wallet 9)

datumHijacking :: (MonadMockChain m) => m ()
datumHijacking = do
  (params, tokenOutRef) <- mkProposal 2 thePayment `as` wallet 1
  mkSign params thePayment (walletSK $ wallet 1) `as` wallet 1
  mkSign params thePayment (walletSK $ wallet 2) `as` wallet 2
  mkSign params thePayment (walletSK $ wallet 3) `as` wallet 3
  mkFakeCollect thePayment params
  where
    thePayment = Payment 4200000 (walletPKHash $ last knownWallets)

trPayment :: Payment -> HJ.Payment
trPayment (Payment val dest) = HJ.Payment val dest

mkFakeCollect :: MonadBlockChain m => Payment -> Params -> m ()
mkFakeCollect thePayment params = do
  [initialProp] <- scriptUtxosSuchThat (pmultisig params) isProposal
  signatures <- nubBy ((==) `on` snd) <$> scriptUtxosSuchThat (pmultisig params) isSign
  let signatureValues = mconcat $ map (sOutValue . fst) signatures
  void $
    validateTxConstr $
      PaysScript
        fakeValidator
        [ ( HJ.Accumulator (trPayment thePayment) (signPk . snd <$> signatures),
            paymentValue thePayment <> sOutValue (fst initialProp) <> signatureValues
          )
        ] :
      SpendsScript (pmultisig params) () initialProp :
      (SpendsScript (pmultisig params) () <$> signatures)
