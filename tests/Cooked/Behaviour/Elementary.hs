{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Standard transactions that ought to cover a substantial spectrum of the
-- capabilities of Cooked.
module Cooked.Behaviour.Elementary where

import Control.Monad (void)
import Cooked
import qualified Cooked.Validators.Unit as Validators
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Typed as Scripts
import qualified Plutus.Script.Utils.Value as Pl
import qualified Plutus.V1.Ledger.Address as PV1 (pubKeyHashAddress)
import qualified Plutus.V2.Ledger.Api as PV2
import Test.Tasty
import Test.Tasty.HUnit

-- | Return the number of Lovelace of the value.
countLovelace :: PV2.Value -> Integer
countLovelace v = Pl.valueOf v PV2.adaSymbol PV2.adaToken

-- | Get all outputs locked by a validator from a blockchain.
utxosProtectedBy ::
  MonadBlockChainBalancing m =>
  Scripts.TypedValidator Validators.Unit ->
  m [(PV2.TxOutRef, PV2.TxOut)]
utxosProtectedBy =
  runUtxoSearch
    . utxosAtSearch
    . Scripts.validatorAddress

wAddress :: Int -> PV2.Address
wAddress = PV1.pubKeyHashAddress . walletPKHash . wallet

walletToWallet :: Assertion
walletToWallet =
  testSucceedsFrom'
    def
    ( \_ state -> do
        wAddress 2 `holdsInState` state @?= Pl.adaValueOf 7
        countLovelace (wAddress 1 `holdsInState` state)
          <= 3_000_000
          @? "Wallet 1 has too many Lovelace"
    )
    ( InitialDistribution $
        Map.fromList
          [ (wallet 1, [Pl.adaValueOf 5]),
            (wallet 2, [Pl.adaValueOf 5])
          ]
    )
    ( validateTxSkel $
        txSkelTemplate
          { txSkelOuts = [paysPK (walletPKHash $ wallet 2) (Pl.adaValueOf 2)],
            txSkelSigners = [wallet 1]
          }
    )

-- | Create transaction with a UTxO protected by a script.
walletToScript ::
  -- | How to store the datum on the UTxO
  Validators.DatumKind ->
  Assertion
walletToScript datumKind =
  testSucceedsFrom'
    def
    ( \utxos state -> do
        let outsValue = mconcat $ view outputValueL . snd <$> utxos
        countLovelace outsValue @?= 2_000_000
        -- Wallet 1 pays some fees
        countLovelace (wAddress 1 `holdsInState` state)
          < 3_000_000
          @? "Wallet 1 has too many Lovelace (have fees been spent?)"
        ( case ( datumKind,
                 mapMaybe
                   (fmap outputOutputDatum . (isScriptOutputFrom Validators.yes . snd))
                   utxos
               ) of
            (Validators.Inline, [PV2.OutputDatum _]) -> True
            (Validators.OnlyHash, [PV2.OutputDatumHash _]) -> True
            (Validators.ResolvedHash, [PV2.OutputDatumHash _]) -> True
            _ -> False
          )
          @? "The datum kind is not the one expected"
    )
    ( InitialDistribution $
        Map.fromList
          [ (wallet 1, [Pl.adaValueOf 5]),
            (wallet 2, [Pl.adaValueOf 5])
          ]
    )
    ( let pays ::
            Scripts.TypedValidator Validators.Unit ->
            Scripts.DatumType Validators.Unit ->
            PV2.Value ->
            TxSkelOut
          pays = case datumKind of
            Validators.Inline -> paysScriptInlineDatum
            Validators.OnlyHash -> paysScriptDatumHash
            Validators.ResolvedHash -> paysScript
          skel =
            txSkelTemplate
              { txSkelOuts = [pays Validators.yes () (Pl.adaValueOf 2)],
                txSkelSigners = [wallet 1]
              }
       in validateTxSkel skel >> utxosProtectedBy Validators.yes
    )

-- | Create an output reference locked by a wallet containing a reference
-- script.
putRefScriptOnWalletOutput ::
  MonadBlockChain m =>
  -- | Recipient of the output and wallet used to balance the transaction
  Wallet ->
  -- | Referenced script
  Scripts.TypedValidator Validators.Unit ->
  m PV2.TxOutRef
putRefScriptOnWalletOutput recipient referencedScript =
  fst . head . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [ paysPK
                (walletPKHash recipient)
                mempty
                `withReferenceScript` referencedScript
            ],
          txSkelSigners = [recipient]
        }

-- | Two transactions:
-- 1. create a UTxO protected by a script
-- 2. consume that UTxO (which means running the validator).
-- The first transaction puts the minimal amount of Ada (taken from wallet 1)
-- in the UTxO protected by the validator.
triggerValidator ::
  MonadBlockChain m =>
  Scripts.TypedValidator Validators.Unit ->
  -- | Template transaction which runs the validator. The transaction
  -- effectively run has an input with the validator added.
  TxSkel ->
  m ()
triggerValidator validator skel = do
  outCTx <-
    validateTxSkel $
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [paysScript validator () mempty],
          txSkelSigners = [wallet 1]
        }
  let (outRef, _) : _ = utxosFromCardanoTx outCTx
  let skel' = over txSkelInsL (Map.insert outRef (TxSkelRedeemerForScript ())) skel
  void $ validateTxSkel skel'

-- * Continuing outputs

--
-- A transaction involves a continuing output when one of its inputs is locked
-- by a script and one of its outputs is locked by the same script. Continuing
-- outputs can be thought of as "threads" in a contract's life, where each
-- transaction changes the value or the datum of the output. On
-- each contribution, the script is run to ensure its correctness.

-- | Produce traces of the form
-- 1. Post a reference script at address of wallet 1
-- 2. Produce an output with some value protected by the latter script
-- 3. Submit a sequence of @n@ transactions that take the unique UTxO locked by the
--    script, consume it and produce a new UTxO still locked by the script.
-- A predicate can be run after each transaction of the sequence.
txSequence ::
  MonadBlockChain m =>
  Scripts.TypedValidator Validators.Unit ->
  -- | The length of the sequence
  Int ->
  -- | Predicates run after each transaction of the sequence. The integer is
  -- substituted by the index of the transaction (starting at zero).
  -- The string is a debug message when the proposition is false.
  (Integer -> m (Bool, String)) ->
  -- | The value /added/ to the value of the previous UTxO in the chain.
  -- The initial UTxO contains 2 Ada.
  (Integer -> PV2.Value) ->
  m [(Bool, String)]
txSequence validator chainLength (predicates :: Integer -> m (Bool, String)) values = do
  refScriptOutRef <- putRefScriptOnWalletOutput (wallet 1) validator
  initCTx <-
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [paysScript validator () (Pl.adaValueOf 2)],
          txSkelSigners = [wallet 2]
        }
  let previousOut : _ = utxosFromCardanoTx initCTx
  go refScriptOutRef previousOut 0 []
  where
    go ::
      MonadBlockChain m =>
      PV2.TxOutRef ->
      (PV2.TxOutRef, PV2.TxOut) ->
      Int ->
      [(Bool, String)] ->
      m [(Bool, String)]
    go refScriptOutRef (previousOutRef, previousOut) count propositions =
      if count >= chainLength
        then return propositions
        else do
          _ <-
            validateTxSkel $
              txSkelTemplate
                { txSkelOpts = def {txOptEnsureMinAda = True},
                  txSkelIns =
                    Map.singleton
                      previousOutRef
                      (TxSkelRedeemerForReferencedScript ()),
                  txSkelOuts =
                    [ paysScript
                        validator
                        ()
                        (previousOut ^. outputValueL <> values (toInteger count))
                    ],
                  txSkelInsReference = Set.singleton refScriptOutRef,
                  txSkelSigners = [wallet 2]
                }
          proposition <- predicates (toInteger count)
          -- There should be a unique output, the test 'uniqueScriptOutput'
          -- ensures this
          outs <- runUtxoSearch $ utxosAtSearch (Scripts.validatorAddress validator)
          go refScriptOutRef (head outs) (count + 1) (proposition : propositions)

-- | A helper function to test sequences of output chaining.
testSequence ::
  -- | The validator protected the value of the chained outputs
  Scripts.TypedValidator Validators.Unit ->
  -- | Length of the sequence
  Int ->
  -- | Predicates to run after each transaction.
  (Integer -> forall m. MonadBlockChain m => m (Bool, String)) ->
  -- | Value accumulated on each transaction: for each transaction of the
  -- sequence, the value of the UTxO is the value of the input + the value
  -- returned by this function. The initial UTxO contains 2A.
  (Integer -> PV2.Value) ->
  InitialDistribution ->
  Assertion
testSequence validator chainLength predicates values distrib =
  testSucceedsFrom'
    def
    (\propositions _ -> mapM_ (uncurry (@?)) propositions)
    distrib
    (txSequence validator chainLength predicates values)

-- | Check that on each step, there is only one output protected by the
-- validator.
uniqueScriptOutput :: Assertion
uniqueScriptOutput =
  testSequence
    Validators.yes
    6
    ( \_ -> do
        utxos <- runUtxoSearch $ utxosAtSearch (Scripts.validatorAddress Validators.yes)
        return
          (case utxos of [_] -> True; _ -> False, "Not a single UTxO locked by the validator")
    )
    (const $ Pl.adaValueOf 2)
    ( InitialDistribution $
        Map.fromList
          [ (wallet 1, [Pl.adaValueOf 42]),
            (wallet 2, [Pl.adaValueOf 42])
          ]
    )

-- | Check that the money stored in the pot increases on each step (according
-- to an affine function).
increasingPot :: Assertion
increasingPot =
  testSequence
    Validators.yes
    6
    ( \i -> do
        (_, theUtxo) : _ <-
          runUtxoSearch $ utxosAtSearch (Scripts.validatorAddress Validators.yes)
        return
          ( outputValue theUtxo == Pl.lovelaceValueOf (2_000_000 * (2 + i)),
            "The amount in the UTxO doesn't match "
              ++ show (2_000_000 * (2 + i))
              ++ " lovelace"
          )
    )
    (\_ -> Pl.adaValueOf 2)
    def

-- | Create a transaction that runs the validator 'Validators.requireSigner'.
requireSigner :: MonadBlockChain m => Wallet -> [Wallet] -> m ()
requireSigner required signers =
  triggerValidator
    (Validators.requireSigner $ walletPKHash required)
    ( txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts = [paysPK (walletPKHash $ wallet 1) mempty],
          txSkelSigners = signers
        }
    )

tests :: TestTree
tests =
  testGroup
    "Standard-ish transactions"
    [ testGroup
        "Transferring Ada from a wallet to"
        [ testCase "another wallet" walletToWallet,
          testCase
            "a script, using an inline datum"
            (walletToScript Validators.Inline),
          testCase
            "a script, using a hashed datum"
            (walletToScript Validators.OnlyHash),
          testCase
            "a script, using a resolved datum"
            $ walletToScript Validators.ResolvedHash
        ],
      testGroup
        "Signers"
        [ testCase
            "the required signer is there"
            (testSucceeds def $ requireSigner (wallet 3) [wallet 1, wallet 3]),
          testCase
            "the required signer is not there"
            (testFails def (isCekEvaluationFailure def) $ requireSigner (wallet 3) [wallet 1, wallet 2])
        ],
      testGroup
        "Valid ranges"
        [ testCase "always subset of the whole time" $
            testSucceeds
              def
              ( triggerValidator
                  (Validators.validRangeSubsetOf (Nothing, Nothing))
                  (txSkelTemplate {txSkelSigners = [wallet 1]})
              ),
          testCase "never subset of the (almost) empty set" $
            testFails
              def
              (isCekEvaluationFailure def)
              ( triggerValidator
                  (Validators.validRangeSubsetOf (Just 0, Just 0))
                  (txSkelTemplate {txSkelSigners = [wallet 1]})
              )
        ],
      testGroup
        "Fees control"
        [ testCase "standard" $
            testSucceeds
              def
              ( triggerValidator
                  (Validators.checkFeeBetween (350_000, Just 3_000_000))
                  (txSkelTemplate {txSkelSigners = [wallet 1]})
              ),
          testCase "3 Ada is the upper bound" $
            testFails
              def
              (isCekEvaluationFailure def)
              ( triggerValidator
                  (Validators.checkFeeBetween (350_000, Just 2_900_000))
                  (txSkelTemplate {txSkelSigners = [wallet 1]})
              ),
          testCase "350 000 Lovelace is the lower bound" $
            testFails
              def
              (isCekEvaluationFailure def)
              ( triggerValidator
                  (Validators.checkFeeBetween (360_000, Just 3_000_000))
                  (txSkelTemplate {txSkelSigners = [wallet 1]})
              )
        ],
      testGroup
        "Continuing outputs"
        [ testCase "keeping one output" uniqueScriptOutput,
          testCase "pot increases" increasingPot
        ]
    ]
