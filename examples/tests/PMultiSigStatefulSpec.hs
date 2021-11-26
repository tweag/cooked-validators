{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module PMultiSigStatefulSpec where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Either (isRight)
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import PMultiSigStateful
import qualified Plutus.V1.Ledger.Value as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Hspec
import Test.QuickCheck

paymentValue :: Payment -> Pl.Value
paymentValue = Pl.lovelaceValueOf . paymentAmount

paramsToken :: Params -> Pl.Value
paramsToken params = Pl.assetClassValue (pmspThreadToken params) 1

-- Proposal is basically an accumulator with no signees
mkProposalSkel :: Integer -> Wallet -> Payment -> MockChain (Params, TxSkel, Pl.TxOutRef)
mkProposalSkel reqSigs w pmt = do
  utxos <- pkUtxos wpkh
  case utxos of
    (spendableOut : _) -> do
      let klass = threadTokenAssetClass $ fst spendableOut
      let params = Params (walletPK <$> knownWallets) reqSigs klass
      let threadToken = paramsToken params
      let skel =
            TxSkel
              w
              [ Mints [threadTokenPolicy (fst spendableOut) threadTokenName] threadToken,
                -- We don't have SpendsPK or PaysPK wrt the wallet `w`
                -- because the balancing mechanism chooses the same (first) output
                -- we're working on.
                PaysScript (pmultisig params) [(Accumulator pmt [], paymentValue pmt <> threadToken)]
              ]
      pure (params, skel, fst spendableOut)
    _ -> error "No spendable outputs for the wallet"
  where
    wpkh = walletPKHash w

mkSignSkel :: Params -> Wallet -> Payment -> TxSkel
mkSignSkel params w pmt = TxSkel w [PaysScript (pmultisig params) [(Sign pk sig, mempty)]]
  where
    pk = walletPK w
    sig = Pl.sign (Pl.sha2_256 $ packPayment pmt) (walletSK w)

isSign :: Datum -> a -> Bool
isSign Sign {} _ = True
isSign _ _ = False

isAccumulator :: Datum -> a -> Bool
isAccumulator Accumulator {} _ = True
isAccumulator _ _ = False

isProposal :: Datum -> a -> Bool
isProposal (Accumulator _ []) _ = True
isProposal _ _ = False

mkThreadTokenInputSkel :: Wallet -> TxSkel
mkThreadTokenInputSkel w = TxSkel w [PaysPK (walletPKHash w) mempty]

mkThreadToken :: Wallet -> MockChain Pl.TxOutRef
mkThreadToken w = do
  validateTxFromSkeleton $ mkThreadTokenInputSkel w
  emptyUtxos <- pkUtxosSuchThat (walletPKHash w) (== mempty)
  case emptyUtxos of
    [] -> error "No empty UTxOs"
    ((out, _) : _) -> pure out

mkParams :: MockChain Params
mkParams = do
  out <- mkThreadToken (wallet 1)
  pure $ Params (walletPK <$> knownWallets) 2 $ threadTokenAssetClass out

mkCollectSkel :: Payment -> Params -> MockChain TxSkel
mkCollectSkel thePayment params = do
  [initialProp] <- scriptUtxosSuchThat (pmultisig params) isProposal
  signatures <- scriptUtxosSuchThat (pmultisig params) isSign
  pure $
    TxSkel (wallet 1) $
      PaysScript (pmultisig params) [(Accumulator thePayment (signPk . snd <$> signatures), (paymentValue thePayment) <> paramsToken params)] :
      SpendsScript (pmultisig params) () initialProp :
      (SpendsScript (pmultisig params) () <$> signatures)


mkPaySkel :: Payment -> Params -> Pl.TxOutRef -> MockChain TxSkel
mkPaySkel thePayment params tokenOutRef = do
  [accumulated] <- scriptUtxosSuchThat (pmultisig params) isAccumulator
  pure $
    TxSkel
      (wallet 1)
      [ PaysPK (paymentRecipient thePayment) (paymentValue thePayment),
        SpendsScript (pmultisig params) () accumulated,
        Mints [threadTokenPolicy tokenOutRef threadTokenName] $ Pl.negate $ paramsToken params
      ]

txs1 :: MockChain ()
txs1 = do
  (params, proposalSkel, tokenOutRef) <- mkProposalSkel 2 (wallet 1) thePayment
  validateTxFromSkeleton proposalSkel
  validateTxFromSkeleton $ mkSignSkel params (wallet 1) thePayment
  validateTxFromSkeleton $ mkSignSkel params (wallet 2) thePayment
  validateTxFromSkeleton $ mkSignSkel params (wallet 3) thePayment
  validateTxFromSkeleton =<< mkCollectSkel thePayment params
  validateTxFromSkeleton =<< mkPaySkel thePayment params tokenOutRef
  where
    thePayment = Payment 4200 (walletPKHash $ last knownWallets)

spec :: Spec
spec = do
  it "allows accumulating all at once" $
    runMockChain txs1 `shouldSatisfy` isRight

spec' :: IO ()
spec' = hspec spec


data Action res where
  Validate :: TxSkel -> Action ()
  WithModifier :: (TxSkel -> [TxSkel]) -> Script a -> Action a
  MCAct    :: MockChain a -> Action a
  GenNat   :: {- upperBound : -} Integer -> Action Integer   -- TODO generalize to arbitrary generatable things

data Script res where
  Pure :: res -> Script res
  Bind :: Action r1 -> (r1 -> Script r2) -> Script r2

liftAction :: Action res -> Script res
liftAction act = act `Bind` Pure

validate :: TxSkel -> Script ()
validate = liftAction . Validate

mcAct :: MockChain a -> Script a
mcAct = liftAction . MCAct

genNat :: Integer -> Script Integer
genNat = liftAction . GenNat

withModifier :: (TxSkel -> [TxSkel]) -> Script a -> Script a
withModifier f = liftAction . WithModifier f

instance Functor Script where
  f `fmap` Pure res = Pure $ f res
  f `fmap` Bind act step = Bind act $ fmap f . step

instance Applicative Script where
  pure = Pure
  Pure f        <*> script = f <$> script
  Bind act step <*> script = Bind act $ \r -> step r <*> script

instance Monad Script where
  Pure v        >>= f = f v
  Bind act step >>= f = Bind act $ step >=> f

runAct :: Action a -> Gen (MockChain a)
runAct (Validate txSkel) = pure $ validateTxFromSkeleton txSkel
runAct (WithModifier f sa) = undefined -- TODO
runAct (MCAct mia) = pure mia
runAct (GenNat upper) = do
  n <- chooseInteger (0, upper)
  pure $ pure n

runScript :: Script a -> Gen (MockChain (), a)
runScript (Pure v) = pure (pure (), v)
runScript (Bind act step) = do
  mc <- runAct act
  _


walletsThreshold :: Script (Integer, Integer)
walletsThreshold = do
  reqSigs <- genNat 5
  withModifier pure $ do
    (params, proposalSkel, tokenOutRef) <- mcAct $ mkProposalSkel reqSigs (wallet 1) thePayment
    validate proposalSkel

    numSigs <- genNat 5
    forM_ [1..numSigs] $ \n -> validate $ mkSignSkel params (wallet $ fromIntegral n) thePayment

    validate =<< mcAct (mkCollectSkel thePayment params)
    validate =<< mcAct (mkPaySkel thePayment params tokenOutRef)

    pure (reqSigs, numSigs)
  where
    thePayment = Payment 4200 (walletPKHash $ last knownWallets)
