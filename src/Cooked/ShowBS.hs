{-# LANGUAGE NoImplicitPrelude #-}

-- | This module exposes on-chain pretty-printing function for all the types
-- that occur on the 'ScriptContext' to 'BuiltinString'. This is useful for
-- debugging of validators. You probably do not want to use this in production
-- code, as many of the functions in this module are wildly inefficient due to
-- limitations of the 'BuiltinString' type.
--
-- If the script execution on your transactions go over budget by using this
-- module, consider using 'txOptEmulatorParamsModification' to temporarily
-- loosen the limits (at the cost of breaking compatibility with mainnet)
module Cooked.ShowBS (ShowBS (..)) where

import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as PlutusTx
import PlutusTx.Builtins qualified as PlutusTx
import PlutusTx.Prelude hiding (toList)
import PlutusTx.Ratio qualified as PlutusTx hiding (negate)

-- | analogue of Haskell's 'Show' class to be use in Plutus scripts.
class ShowBS a where
  -- | analogue of 'show'
  showBS :: a -> BuiltinString

-- | print with a surrounding parenthesis
{-# INLINEABLE showBSParen #-}
showBSParen :: BuiltinString -> BuiltinString
showBSParen s = "(" <> s <> ")"

-- | print an application of a constructor to an argument
{-# INLINEABLE application1 #-}
application1 :: (ShowBS a) => BuiltinString -> a -> BuiltinString
application1 bs x = showBSParen $ bs <> " " <> showBS x

-- | like 'application1' with two arguments
{-# INLINEABLE application2 #-}
application2 :: (ShowBS a, ShowBS b) => BuiltinString -> a -> b -> BuiltinString
application2 bs x y = showBSParen $ bs <> " " <> showBS x <> " " <> showBS y

-- | like 'application1' with three arguments
{-# INLINEABLE application3 #-}
application3 :: (ShowBS a, ShowBS b, ShowBS c) => BuiltinString -> a -> b -> c -> BuiltinString
application3 bs x y z = showBSParen $ bs <> " " <> showBS x <> " " <> showBS y <> " " <> showBS z

-- | like 'application1' with four arguments
{-# INLINEABLE application4 #-}
application4 :: (ShowBS a, ShowBS b, ShowBS c, ShowBS d) => BuiltinString -> a -> b -> c -> d -> BuiltinString
application4 bs x y z w = showBSParen $ bs <> " " <> showBS x <> " " <> showBS y <> " " <> showBS z <> " " <> showBS w

instance ShowBS Integer where
  {-# INLINEABLE showBS #-}
  showBS i = mconcat (integerToDigits i)

{-# INLINEABLE integerToDigits #-}
integerToDigits :: Integer -> [BuiltinString]
integerToDigits n
  | n < 0 = "-" : go (negate n) []
  | n == 0 = ["0"]
  | otherwise = go n []
  where
    go i acc
      | i == 0 = acc
      | otherwise = let (q, r) = quotRem i 10 in go q (digitToBS r : acc)

{-# INLINEABLE digitToBS #-}
digitToBS :: Integer -> BuiltinString
digitToBS x
  | x == 0 = "0"
  | x == 1 = "1"
  | x == 2 = "2"
  | x == 3 = "3"
  | x == 4 = "4"
  | x == 5 = "5"
  | x == 6 = "6"
  | x == 7 = "7"
  | x == 8 = "8"
  | x == 9 = "9"
  | otherwise = "?"

instance (ShowBS a) => ShowBS [a] where
  {-# INLINEABLE showBS #-}
  showBS = catList "[" "," "]" showBS

{-# INLINEABLE catList #-}
catList :: BuiltinString -> BuiltinString -> BuiltinString -> (a -> BuiltinString) -> [a] -> BuiltinString
catList open _ close _ [] = open <> close
catList open sep close print (x : xs) = open <> print x <> printSeparated xs <> close
  where
    printSeparated [] = ""
    printSeparated (y : ys) = sep <> print y <> printSeparated ys

instance (ShowBS a, ShowBS b) => ShowBS (a, b) where
  {-# INLINEABLE showBS #-}
  showBS (x, y) = "(" <> showBS x <> "," <> showBS y <> ")"

instance ShowBS Bool where
  {-# INLINEABLE showBS #-}
  showBS True = "True"
  showBS False = "False"

instance (ShowBS a) => ShowBS (Maybe a) where
  {-# INLINEABLE showBS #-}
  showBS Nothing = "Nothing"
  showBS (Just x) = application1 "Just" x

instance (ShowBS k, ShowBS v) => ShowBS (PlutusTx.Map k v) where
  {-# INLINEABLE showBS #-}
  showBS m = application1 "fromList" (PlutusTx.toList m)

instance ShowBS BuiltinByteString where
  -- base16 representation
  {-# INLINEABLE showBS #-}
  showBS bs = "\"" <> mconcat (builtinByteStringCharacters bs) <> "\""

{-# INLINEABLE builtinByteStringCharacters #-}
builtinByteStringCharacters :: BuiltinByteString -> [BuiltinString]
builtinByteStringCharacters s = go (len - 1) []
  where
    len = lengthOfByteString s

    go :: Integer -> [BuiltinString] -> [BuiltinString]
    go i acc
      | i >= 0 =
          let (highNibble, lowNibble) = quotRem (indexByteString s i) 16
           in go (i - 1) (toHex highNibble : toHex lowNibble : acc)
      | otherwise = acc

    toHex :: Integer -> BuiltinString
    toHex x
      | x <= 9 = digitToBS x
      | x == 10 = "a"
      | x == 11 = "b"
      | x == 12 = "c"
      | x == 13 = "d"
      | x == 14 = "e"
      | x == 15 = "f"
      | otherwise = "?"

instance ShowBS Api.TokenName where
  {-# INLINEABLE showBS #-}
  showBS (Api.TokenName x) = application1 "TokenName" x

instance ShowBS Api.CurrencySymbol where
  {-# INLINEABLE showBS #-}
  showBS (Api.CurrencySymbol x) = application1 "CurrencySymbol" x

instance ShowBS Api.Value where
  {-# INLINEABLE showBS #-}
  showBS (Api.Value m) = application1 "Value" m

instance ShowBS Api.TxId where
  {-# INLINEABLE showBS #-}
  showBS (Api.TxId x) = application1 "TxId" x

instance ShowBS Api.TxOutRef where
  {-# INLINEABLE showBS #-}
  showBS (Api.TxOutRef txid i) = application2 "TxOutRef" txid i

instance ShowBS Api.PubKeyHash where
  {-# INLINEABLE showBS #-}
  showBS (Api.PubKeyHash h) = application1 "PubKeyHash" h

instance ShowBS Api.Credential where
  {-# INLINEABLE showBS #-}
  showBS (Api.ScriptCredential hash) = application1 "ScriptCredential" hash
  showBS (Api.PubKeyCredential pkh) = application1 "PubKeyCredential" pkh

instance ShowBS Api.StakingCredential where
  {-# INLINEABLE showBS #-}
  showBS (Api.StakingHash cred) = application1 "StakingCredential" cred
  showBS (Api.StakingPtr i j k) = application3 "StakingPointer" i j k

instance ShowBS Api.Address where
  {-# INLINEABLE showBS #-}
  showBS (Api.Address cred mStCred) = application2 "Address" cred mStCred

instance ShowBS Api.DatumHash where
  {-# INLINEABLE showBS #-}
  showBS (Api.DatumHash h) = application1 "DatumHash" h

instance ShowBS BuiltinData where
  {-# INLINEABLE showBS #-}
  showBS d = showBSParen $ "BuiltinData " <> showData d

{-# INLINEABLE showData #-}
showData :: BuiltinData -> BuiltinString
showData d =
  PlutusTx.matchData
    d
    (\i ds -> showBSParen $ "Constr " <> showBS i <> " " <> catList "[" "," "]" showData ds)
    (\alist -> showBSParen $ "Map " <> catList "[" "," "]" (\(a, b) -> "(" <> showData a <> "," <> showData b <> ")") alist)
    (\list -> showBSParen $ "List " <> catList "[" "," "]" showData list)
    (\i -> showBSParen $ "I " <> showBS i)
    (\bs -> showBSParen $ "B " <> showBS bs)

instance ShowBS Api.Datum where
  {-# INLINEABLE showBS #-}
  showBS (Api.Datum d) = application1 "Datum" d

instance ShowBS Api.OutputDatum where
  {-# INLINEABLE showBS #-}
  showBS Api.NoOutputDatum = "NoOutputDatum"
  showBS (Api.OutputDatumHash h) = application1 "OutputDatumHash" h
  showBS (Api.OutputDatum d) = application1 "OutputDatum" d

instance ShowBS Api.ScriptHash where
  {-# INLINEABLE showBS #-}
  showBS (Api.ScriptHash h) = application1 "ScriptHash" h

instance ShowBS Api.TxOut where
  {-# INLINEABLE showBS #-}
  showBS (Api.TxOut address value datum mRefScriptHash) = application4 "TxOut" address value datum mRefScriptHash

instance ShowBS Api.TxInInfo where
  {-# INLINEABLE showBS #-}
  showBS (Api.TxInInfo oref out) = application2 "TxInInfo" oref out

instance ShowBS Api.POSIXTime where
  {-# INLINEABLE showBS #-}
  showBS (Api.POSIXTime t) = application1 "POSIXTime" t

instance (ShowBS a) => ShowBS (Api.Extended a) where
  {-# INLINEABLE showBS #-}
  showBS Api.NegInf = "NegInf"
  showBS Api.PosInf = "PosInf"
  showBS (Api.Finite x) = application1 "Finite" x

instance (ShowBS a) => ShowBS (Api.LowerBound a) where
  {-# INLINEABLE showBS #-}
  showBS (Api.LowerBound x closure) = application2 "LowerBound" x closure

instance (ShowBS a) => ShowBS (Api.UpperBound a) where
  {-# INLINEABLE showBS #-}
  showBS (Api.UpperBound x closure) = application2 "UpperBound" x closure

instance (ShowBS a) => ShowBS (Api.Interval a) where
  {-# INLINEABLE showBS #-}
  showBS (Api.Interval lb ub) = application2 "Interval" lb ub

instance ShowBS Api.DRepCredential where
  {-# INLINEABLE showBS #-}
  showBS (Api.DRepCredential cred) = application1 "DRep credential" cred

instance ShowBS Api.DRep where
  {-# INLINEABLE showBS #-}
  showBS (Api.DRep dRepCred) = application1 "DRep" dRepCred
  showBS Api.DRepAlwaysAbstain = "DRep always abstain"
  showBS Api.DRepAlwaysNoConfidence = "DRep always no confidence"

instance ShowBS Api.Delegatee where
  {-# INLINEABLE showBS #-}
  showBS (Api.DelegStake pkh) = application1 "Delegate stake" pkh
  showBS (Api.DelegVote dRep) = application1 "Delegate vote" dRep
  showBS (Api.DelegStakeVote pkh dRep) = application2 "Delegate stake vote" pkh dRep

instance ShowBS Api.TxCert where
  {-# INLINEABLE showBS #-}
  showBS (Api.TxCertRegStaking cred maybeDepositAmount) = application2 "Register staking" cred maybeDepositAmount
  showBS (Api.TxCertUnRegStaking cred maybeDepositAmount) = application2 "Unregister staking" cred maybeDepositAmount
  showBS (Api.TxCertDelegStaking cred delegatee) = application2 "Delegate staking" cred delegatee
  showBS (Api.TxCertRegDeleg cred delegatee depositAmount) = application3 "Register and delegate staking" cred delegatee depositAmount
  showBS (Api.TxCertRegDRep dRepCred amount) = application2 "Register DRep" dRepCred amount
  showBS (Api.TxCertUpdateDRep dRepCred) = application1 "Update DRep" dRepCred
  showBS (Api.TxCertUnRegDRep dRepCred amount) = application2 "Unregister DRep" dRepCred amount
  showBS (Api.TxCertPoolRegister poolId poolVFR) = application2 "Register to pool" poolId poolVFR
  showBS (Api.TxCertPoolRetire pkh epoch) = application2 "Retire from pool" pkh epoch
  showBS (Api.TxCertAuthHotCommittee coldCommitteeCred hotCommitteeCred) = application2 "Authorize hot committee " coldCommitteeCred hotCommitteeCred
  showBS (Api.TxCertResignColdCommittee coldCommitteeCred) = application1 "Resign cold committee" coldCommitteeCred

instance ShowBS Api.Voter where
  {-# INLINEABLE showBS #-}
  showBS (Api.CommitteeVoter hotCommitteeCred) = application1 "Committee Voter" hotCommitteeCred
  showBS (Api.DRepVoter dRepCred) = application1 "DRep Voter" dRepCred
  showBS (Api.StakePoolVoter pkh) = application1 "Stake Pool Voter" pkh

instance ShowBS Api.GovernanceActionId where
  {-# INLINEABLE showBS #-}
  showBS Api.GovernanceActionId {..} =
    showBSParen
      $ "Tx Id:"
      <> showBS gaidTxId
      <> "Action Id:"
      <> showBS gaidGovActionIx

instance ShowBS Api.ScriptPurpose where
  {-# INLINEABLE showBS #-}
  showBS (Api.Minting cs) = application1 "Minting" cs
  showBS (Api.Spending oref) = application1 "Spending" oref
  showBS (Api.Rewarding stCred) = application1 "Rewarding" stCred
  showBS (Api.Certifying nb txCert) = application2 "Certifying" nb txCert
  showBS (Api.Voting voter) = application1 "Voting" voter
  showBS (Api.Proposing nb proposal) = application2 "Proposing" nb proposal

instance ShowBS Api.Redeemer where
  {-# INLINEABLE showBS #-}
  showBS (Api.Redeemer builtinData) = application1 "Redeemer" builtinData

instance ShowBS Api.Vote where
  {-# INLINEABLE showBS #-}
  showBS Api.VoteNo = "No"
  showBS Api.VoteYes = "Yes"
  showBS Api.Abstain = "Abstain"

instance ShowBS Api.ChangedParameters where
  {-# INLINEABLE showBS #-}
  showBS (Api.ChangedParameters builtinData) = application1 "Changed parameters" builtinData

instance ShowBS Api.ProtocolVersion where
  {-# INLINEABLE showBS #-}
  showBS (Api.ProtocolVersion major minor) = application2 "Protocol version" major minor

instance ShowBS Api.Constitution where
  {-# INLINEABLE showBS #-}
  showBS (Api.Constitution constitutionScript) = application1 "Constitution" constitutionScript

instance ShowBS Api.ColdCommitteeCredential where
  {-# INLINEABLE showBS #-}
  showBS (Api.ColdCommitteeCredential cred) = application1 "Cold committee credential" cred

instance ShowBS Api.HotCommitteeCredential where
  {-# INLINEABLE showBS #-}
  showBS (Api.HotCommitteeCredential cred) = application1 "Hot committee credential" cred

instance ShowBS Api.Committee where
  {-# INLINEABLE showBS #-}
  showBS (Api.Committee committeeMembers committeeQuorum) = application2 "Committee" committeeMembers committeeQuorum

instance ShowBS Api.Lovelace where
  {-# INLINEABLE showBS #-}
  showBS (Api.Lovelace amount) = application1 "Lovelace" amount

instance ShowBS PlutusTx.Rational where
  {-# INLINEABLE showBS #-}
  showBS rat = application2 "Rational" (PlutusTx.numerator rat) (PlutusTx.denominator rat)

instance ShowBS Api.GovernanceAction where
  {-# INLINEABLE showBS #-}
  showBS (Api.ParameterChange maybeActionId changeParams mScriptHash) = application3 "Parameter change" maybeActionId changeParams mScriptHash
  showBS (Api.HardForkInitiation maybeActionId protocolVersion) = application2 "HardForkInitiation" maybeActionId protocolVersion
  showBS (Api.TreasuryWithdrawals mapCredLovelace mScriptHash) = application2 "TreasuryWithdrawals" mapCredLovelace mScriptHash
  showBS (Api.NoConfidence maybeActionId) = application1 "NoConfidence" maybeActionId
  showBS (Api.UpdateCommittee maybeActionId toRemoveCreds toAddCreds quorum) = application4 "Info action" maybeActionId toRemoveCreds toAddCreds quorum
  showBS (Api.NewConstitution maybeActionId constitution) = application2 "NewConstitution" maybeActionId constitution
  showBS Api.InfoAction = "InfoAction"

instance ShowBS Api.ProposalProcedure where
  {-# INLINEABLE showBS #-}
  showBS Api.ProposalProcedure {..} =
    showBSParen
      $ "deposit:"
      <> showBS ppDeposit
      <> "return address"
      <> showBS ppReturnAddr
      <> "governance action"
      <> showBS ppGovernanceAction

instance ShowBS Api.TxInfo where
  {-# INLINEABLE showBS #-}
  showBS Api.TxInfo {..} =
    showBSParen
      $ "inputs:"
      <> showBS txInfoInputs
      <> "reference inputs:"
      <> showBS txInfoReferenceInputs
      <> "outputs:"
      <> showBS txInfoOutputs
      <> "fees:"
      <> showBS txInfoFee
      <> "minted value:"
      <> showBS txInfoMint
      <> "certificates:"
      <> showBS txInfoTxCerts
      <> "wdrl:"
      <> showBS txInfoWdrl
      <> "valid range:"
      <> showBS txInfoValidRange
      <> "signatories:"
      <> showBS txInfoSignatories
      <> "redeemers:"
      <> showBS txInfoRedeemers
      <> "datums:"
      <> showBS txInfoData
      <> "transaction id:"
      <> showBS txInfoId
      <> "votes:"
      <> showBS txInfoVotes
      <> "proposals:"
      <> showBS txInfoProposalProcedures
      <> "treasury amount:"
      <> showBS txInfoCurrentTreasuryAmount
      <> "treasury donation:"
      <> showBS txInfoTreasuryDonation

instance ShowBS Api.ScriptInfo where
  {-# INLINEABLE showBS #-}
  showBS (Api.MintingScript cs) = application1 "MintingScript" cs
  showBS (Api.SpendingScript oref mDat) = application2 "SpendingScript" oref mDat
  showBS (Api.RewardingScript stCred) = application1 "RewardingScript" stCred
  showBS (Api.CertifyingScript nb txCert) = application2 "CertifyingScript" nb txCert
  showBS (Api.VotingScript voter) = application1 "VotingScript" voter
  showBS (Api.ProposingScript nb proposal) = application2 "ProposingScript" nb proposal

instance ShowBS Api.ScriptContext where
  {-# INLINEABLE showBS #-}
  showBS Api.ScriptContext {..} =
    showBSParen
      $ "Script context:"
      <> "Script Tx info:"
      <> showBS scriptContextTxInfo
      <> "Script redeemer:"
      <> showBS scriptContextRedeemer
      <> "Script purpose:"
      <> showBS scriptContextScriptInfo
