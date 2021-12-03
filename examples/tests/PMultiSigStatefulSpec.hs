{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import PMultiSigStateful
import qualified Plutus.V1.Ledger.Value as Pl
import qualified PlutusTx.Prelude as Pl
import QuickCheck.GenT
import Test.Hspec hiding (after)
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

paymentValue :: Payment -> Pl.Value
paymentValue = Pl.lovelaceValueOf . paymentAmount

paramsToken :: Params -> Pl.Value
paramsToken params = Pl.assetClassValue (pmspThreadToken params) 1

data ProposalSkel = ProposalSkel Integer Payment
  deriving (Show)

-- Proposal is basically an accumulator with no signees
mkProposalSkel :: MonadMockChain m => Integer -> Wallet -> Payment -> m (Params, TxSkel, Pl.TxOutRef)
mkProposalSkel reqSigs w pmt = do
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
      pure (params, skel, fst spendableOut)
    _ -> error "No spendable outputs for the wallet"
  where
    wpkh = walletPKHash w

mkSignSkel :: Params -> Wallet -> Payment -> TxSkel
mkSignSkel params w pmt = txSkel w [PaysScript (pmultisig params) [(Sign pk sig, mempty)]]
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
mkThreadTokenInputSkel w = txSkel w [PaysPK (walletPKHash w) mempty]

mkThreadToken :: MonadMockChain m => Wallet -> m Pl.TxOutRef
mkThreadToken w = do
  validateTxFromSkeleton $ mkThreadTokenInputSkel w
  emptyUtxos <- pkUtxosSuchThat (walletPKHash w) (== mempty)
  case emptyUtxos of
    [] -> error "No empty UTxOs"
    ((out, _) : _) -> pure out

mkParams :: MonadMockChain m => m Params
mkParams = do
  out <- mkThreadToken (wallet 1)
  pure $ Params (walletPK <$> knownWallets) 2 $ threadTokenAssetClass out

mkCollectSkel :: MonadMockChain m => Payment -> Params -> m TxSkel
mkCollectSkel thePayment params = do
  [initialProp] <- scriptUtxosSuchThat (pmultisig params) isProposal
  signatures <- scriptUtxosSuchThat (pmultisig params) isSign
  pure $
    txSkel (wallet 1) $
      PaysScript (pmultisig params) [(Accumulator thePayment (signPk . snd <$> signatures), (paymentValue thePayment) <> paramsToken params)] :
      SpendsScript (pmultisig params) () initialProp :
      (SpendsScript (pmultisig params) () <$> signatures)

mkPaySkel :: MonadMockChain m => Payment -> Params -> Pl.TxOutRef -> m TxSkel
mkPaySkel thePayment params tokenOutRef = do
  [accumulated] <- scriptUtxosSuchThat (pmultisig params) isAccumulator
  pure $
    txSkel
      (wallet 1)
      [ PaysPK (paymentRecipient thePayment) (paymentValue thePayment),
        SpendsScript (pmultisig params) () accumulated,
        Mints [threadTokenPolicy tokenOutRef threadTokenName] $ Pl.negate $ paramsToken params
      ]

txs1 :: MonadMockChain m => m ()
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

data ThresholdParams = ThresholdParams
  { reqSigs :: Integer,
    numSigs :: Integer
  }
  deriving (Show)

walletsThreshold :: MonadMockChain m => ThresholdParams -> GenT m ()
walletsThreshold ThresholdParams {reqSigs, numSigs} = do
  (params, proposalSkel, tokenOutRef) <- mkProposalSkel reqSigs (wallet 1) thePayment
  validateTxFromSkeleton proposalSkel

  forM_ [1 .. numSigs] $ \n -> validateTxFromSkeleton $ mkSignSkel params (wallet $ fromIntegral n) thePayment

  validateTxFromSkeleton =<< mkCollectSkel thePayment params
  validateTxFromSkeleton =<< mkPaySkel thePayment params tokenOutRef
  where
    thePayment = Payment 4200 (walletPKHash $ last knownWallets)

test :: QC.Property
test = after' (ThresholdParams <$> choose (1, 5) <*> choose (0, 5)) walletsThreshold prop
  where
    prop ThresholdParams {..} =
      QC.property
        . if reqSigs <= numSigs
          then isRight
          else isLeft

dropOne :: TxSkel -> Maybe TxSkel
dropOne (TxSkel lbl w [c]) = Nothing
dropOne (TxSkel lbl w (c : constr)) = Just $ TxSkel lbl w constr

test2 :: QC.Property
test2 = after (somewhere dropOne (walletsThreshold $ ThresholdParams 2 3)) prop
  where
    prop (Left err) = QC.counterexample (show err) False
    prop (Right _) = QC.property True

r = QC.quickCheck test2
