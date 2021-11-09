module PMultiSigStatefulSpec where

import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Generator
import Data.Either (isRight)
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import PMultiSigStateful
import qualified Plutus.V1.Ledger.Value as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Hspec

paymentValue :: Payment -> Pl.Value
paymentValue = Pl.lovelaceValueOf . paymentAmount

paramsToken :: Params -> Pl.Value
paramsToken params = Pl.assetClassValue (pmspThreadToken params) 1

-- Proposal is basically an accumulator with no signees
mkProposalSkel :: Wallet -> Payment -> MockChain (Params, TxSkel, Pl.TxOutRef)
mkProposalSkel w pmt = do
  utxos <- pkUtxos wpkh
  case utxos of
    (spendableOut : _) -> do
      let klass = threadTokenAssetClass $ fst spendableOut
      let params = Params (walletPK <$> knownWallets) 2 klass
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
    sig = Pl.sign (Pl.sha2_256 $ packPayment pmt) (snd w)

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

txs1 :: MockChain ()
txs1 = do
  (params, proposalSkel, tokenOutRef) <- mkProposalSkel (wallet 1) thePayment
  validateTxFromSkeleton proposalSkel
  validateTxFromSkeleton $ mkSignSkel params (wallet 1) thePayment
  validateTxFromSkeleton $ mkSignSkel params (wallet 2) thePayment
  validateTxFromSkeleton $ mkSignSkel params (wallet 3) thePayment
  validateTxFromSkeleton =<< collect params
  validateTxFromSkeleton =<< pay params tokenOutRef
  where
    collect params = do
      [initialProp] <- scriptUtxosSuchThat (pmultisig params) isProposal
      signatures <- scriptUtxosSuchThat (pmultisig params) isSign
      pure $
        TxSkel (wallet 1) $
          PaysScript (pmultisig params) [(Accumulator thePayment (signPk . snd <$> signatures), paymentAda <> paramsToken params)] :
          SpendsScript (pmultisig params) () initialProp :
          (SpendsScript (pmultisig params) () <$> signatures)

    pay params tokenOutRef = do
      [accumulated] <- scriptUtxosSuchThat (pmultisig params) isAccumulator
      pure $
        TxSkel
          (wallet 1)
          [ PaysPK (paymentRecipient thePayment) paymentAda,
            SpendsScript (pmultisig params) () accumulated,
            Mints [threadTokenPolicy tokenOutRef threadTokenName] $ Pl.negate $ paramsToken params
          ]

    paymentAda = paymentValue thePayment
    thePayment = Payment 4200 (walletPKHash $ last knownWallets)

spec :: Spec
spec = do
  it "allows accumulating all at once" $
    runMockChain txs1 `shouldSatisfy` isRight

spec' :: IO ()
spec' = hspec spec
