module Cooked.MockChain.Monad.GenerateTx where

import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Type
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ledger.Address as Pl
import qualified Ledger.Constraints.OffChain as Pl
import qualified Ledger.Interval as Pl
import qualified Ledger.Scripts as Pl hiding (validatorHash)
import qualified Ledger.Tx as Pl
import qualified Ledger.Typed.Scripts as Pl
import qualified Ledger.Value as Pl
import Optics.Core
import qualified PlutusTx as Pl
import Test.QuickCheck.Modifiers (NonZero (..))

data GenerateTxError = GenerateTxError deriving (Show, Eq)

generateUnbalTx :: TxSkel -> Either GenerateTxError Pl.UnbalancedTx
generateUnbalTx
  skel@TxSkel
    { _txSkelOpts = _opts, -- seems we don't need these yet?
      _txSkelMints = mints,
      _txSkelValidityRange = validityRange,
      _txSkelRequiredSigners = reqSigners,
      _txSkelIns = ins,
      _txSkelOuts = outs
    } =
    Right $
      Pl.UnbalancedEmulatorTx
        { Pl.unBalancedEmulatorTx =
            Pl.Tx
              { Pl.txInputs = inConstraintToTxIn <$> Set.toList ins,
                -- We don't yet support reference inputs.
                Pl.txReferenceInputs = [],
                -- This is at the moment set later in the transaction
                -- generation process, but we might alreay include this in
                -- the 'TxSkel', no?
                Pl.txCollateral = [],
                Pl.txOutputs = outConstraintToTxOut <$> outs,
                Pl.txMint = mintedValue,
                -- Will be set later: At the moment, we have
                -- 'setFeeAndValidRange' for that purpose.
                Pl.txFee = mempty,
                -- This is where we need convert between time and slots. TODO!
                Pl.txValidRange = Pl.always,
                -- These are the redeemers for the minting scripts, given as
                -- a Map from 'MintingPolicyHash' to 'Redeemer'.
                Pl.txMintingScripts = mintsRedeemers,
                -- Don't yet know how to use the next two fields:
                Pl.txWithdrawals = [],
                Pl.txCertificates = [],
                -- Don't yet know how to set this correctly. It's a map from
                -- 'Pl.PubKey' to 'Pl.Signature', both of which I find
                -- confusing, also, there are the questions:
                -- - What kind of (monadic) contxet will be necessary to
                --   figure out the 'Signature's?
                -- - When will we need to know them (when generating the
                --   transaction or can we add them later?
                -- - Which signatures are needed (the ones from which we
                --   consume inputs, the ones from the 'reqSigners', both)?
                Pl.txSignatures = Map.empty,
                -- This should record "Scripts for all script credentials
                -- mentioned in this tx", as per the comment. For now, it's
                -- only the minting scripts.
                Pl.txScripts =
                  Map.fromList $
                    ( \(Pl.Versioned (Pl.MintingPolicy mp) version) ->
                        let mpScript = Pl.Versioned mp version
                         in (Pl.scriptHash mpScript, mpScript)
                    )
                      <$> Map.keys mints,
                -- Instead of calling 'txSkelData' here, we might need a
                -- function that's monadic somehow, to
                -- - find data that are on the transation, but only has
                --   hashes in some registry, and
                -- - update that registry.
                Pl.txData = txSkelData skel,
                -- What should go here?
                Pl.txMetadata = Nothing
              },
          -- As with the signatures above, I'm unsure if this is correct:
          Pl.unBalancedTxRequiredSignatories = Set.map Pl.PaymentPubKeyHash reqSigners,
          -- The haddock comment on plutus-apps "defines" this in terms of the
          -- 'ScriptLookups' that were used to generate the transaction... Don't
          -- know at the moment what it's supposed to be.
          Pl.unBalancedTxUtxoIndex = txSkelUtxoIndex skel
        }
    where
      -- The value minted by the transaction described by the TxSkel
      mintedValue :: Pl.Value
      mintedValue =
        foldMap
          ( \(policy, _red, tName, NonZero amount) ->
              Pl.assetClassValue
                ( Pl.assetClass
                    (Pl.mpsSymbol . Pl.mintingPolicyHash $ policy)
                    tName
                )
                amount
          )
          $ txSkelMintsToList mints

      mintsRedeemers :: Map Pl.MintingPolicyHash Pl.Redeemer
      mintsRedeemers =
        Map.fromList $
          map
            ( \(policy, mRedeemer, _tName, _amount) ->
                ( Pl.mintingPolicyHash policy,
                  case mRedeemer of
                    -- Minting with no redeemer means minting with the unit
                    -- redeemer. Plutus-apps does it the same way.
                    NoMintsRedeemer -> Pl.unitRedeemer
                    SomeMintsRedeemer redeemer -> Pl.Redeemer . Pl.toBuiltinData $ redeemer
                )
            )
            (txSkelMintsToList mints)

      inConstraintToTxIn :: InConstraint -> Pl.TxInput
      inConstraintToTxIn inConstr = Pl.TxInput oRef txInputType
        where
          oRef = inConstr ^. input % spOutTxOutRef
          txInputType
            | SpendsScript val red spOut <- inConstr =
              case spOutDatum spOut of
                Just datum ->
                  Pl.TxScriptAddress
                    (Pl.Redeemer . Pl.toBuiltinData $ red)
                    (Left . Pl.validatorHash $ val)
                    (Pl.datumHash datum)
                Nothing -> Pl.TxConsumeSimpleScriptAddress
            | otherwise = Pl.TxConsumePublicKeyAddress
