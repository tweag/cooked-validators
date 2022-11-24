module Cooked.MockChain.Monad.GenerateTx where

import Cooked.Tx.Constraints.Type
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ledger.Address as Pl
import Ledger.Constraints.OffChain as Pl
import qualified Ledger.Tx as Pl
import qualified Ledger.Typed.Scripts as Pl
import qualified Ledger.Value as Pl
import Optics.Core
import qualified Plutus.Script.Utils.V1.Scripts as Pl
import qualified Plutus.V1.Ledger.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
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
      Pl.UnbalancedTx
        { Pl.unBalancedTxTx =
            Right
              ( Pl.Tx
                  { Pl.txInputs = Set.map inConstraintToTxIn ins,
                    Pl.txOutputs = outConstraintToTxOut <$> outs,
                    -- This is at the moment set later in the transaction
                    -- generation process, but we might alreay include this in
                    -- the 'TxSkel', no?
                    Pl.txCollateral = Set.empty,
                    Pl.txMint = txSkelMintedValue,
                    -- The next two will be set later: At the moment, we have
                    -- 'setFeeAndValidRange' for that purpose.
                    Pl.txFee = mempty,
                    Pl.txValidRange = Pl.always,
                    -- This is a set of the minting scripts:
                    Pl.txMintScripts = Set.fromList $ (\(pol, _, _) -> pol) <$> Map.keys mints,
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
                    -- These are the redeemers for the minting scripts, given as
                    -- a Map from 'RedeemerPtr' to 'Redeemer'. This is strange,
                    -- because the 'RedeemerPtr's identify the scripts by the
                    -- order ib which they appear on the transaction, and the
                    -- minting scripts are given as a set...
                    Pl.txRedeemers = mintsRedeemers,
                    -- Instead of calling 'txSkelData' here, we might need a
                    -- function that's monadic somehow, to
                    -- - find data that are on the transation, but only has
                    --   hashes in some registry, and
                    -- - update that registry.
                    Pl.txData = txSkelData skel
                  }
              ),
          -- As with the signatures above, I'm unsure if this is correct:
          Pl.unBalancedTxRequiredSignatories = Set.map Pl.PaymentPubKeyHash reqSigners,
          -- The haddock comment on plutus-apps "defines" this in terms of the
          -- 'ScriptLookups' that were used to generate the transaction... Don't
          -- know at the moment what it's supposed to be.
          Pl.unBalancedTxUtxoIndex = Map.empty,
          Pl.unBalancedTxValidityTimeRange = validityRange
        }
    where
      -- The value minted by the transaction described by the TxSkel
      txSkelMintedValue =
        Map.foldMapWithKey
          ( \(policy, _redeemer, tName) (NonZero amount) ->
              Pl.assetClassValue
                ( Pl.assetClass
                    (Pl.mpsSymbol . Pl.mintingPolicyHash $ policy)
                    tName
                )
                amount
          )
          mints

      -- If one minting policy appears more than once, what to do? I think we
      -- should forbid this by construction of the 'TxSkelMints' type.
      mintsRedeemers =
        Map.fromList $
          zipWith
            ( \i (_policy, mRedeemer, _tName) ->
                ( Pl.RedeemerPtr Pl.Mint i,
                  case mRedeemer of
                    NoMintsRedeemer ->
                      Pl.unitRedeemer -- Minting with no redeemer means minting
                      -- with the unit redeemer. Plutus-apps
                      -- doew it the same way.
                    SomeMintsRedeemer redeemer ->
                      Pl.Redeemer . Pl.toBuiltinData $ redeemer
                )
            )
            [0 ..]
            (Map.keys mints)

      inConstraintToTxIn :: InConstraint -> Pl.TxIn
      inConstraintToTxIn inConstr = Pl.TxIn oRef $ Just txInType
        where
          oRef = inConstr ^. input % spOutTxOutRef
          txInType
            | SpendsScript val red spOut <- inConstr =
              case spOut ^? spOutDatum of
                Just datum ->
                  Pl.ConsumeScriptAddress
                    (Pl.validatorScript val) -- TODO
                    (Pl.Redeemer . Pl.toBuiltinData $ red)
                    datum
                Nothing -> Pl.ConsumeSimpleScriptAddress
            | otherwise = Pl.ConsumePublicKeyAddress

      outConstraintToTxOut :: OutConstraint -> Pl.TxOut
      outConstraintToTxOut outConstr =
        Pl.TxOut
          (recipientAddress outConstr)
          (outConstr ^. outValue)
          (Pl.datumHash <$> outConstr ^? outConstraintDatum)
