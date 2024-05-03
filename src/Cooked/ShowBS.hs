{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use camelCase" #-}

-- | Print all the types that occur on the 'TxInfo' to 'BuiltinString'. This is
-- useful for debugging of validators. You probably do not want to use this in
-- production code, as many of the functions in this module are wildly
-- inefficient due to limitations of the 'BuiltinString' type.
--
-- If the functions from this module make script execution on your transactions
-- go over budget, consider using 'txOptEmulatorParamsModification' to
-- temporarily loosen the limits (at the cost of breaking compatibility with
-- mainnet)
module Cooked.ShowBS (ShowBS (..)) where

import PlutusLedgerApi.V3
import PlutusTx.AssocMap
import PlutusTx.Builtins
import PlutusTx.Prelude hiding (toList)
import PlutusTx.Ratio hiding (negate)

-- | analogue of Haskell's 'Show' class for use in Plutus scripts.
class ShowBS a where
  -- | analogue of 'show'
  showBS :: a -> BuiltinString

-- | print with a surrounding parenthesis, if the boolean argument is true
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

instance (ShowBS k, ShowBS v) => ShowBS (Map k v) where
  {-# INLINEABLE showBS #-}
  showBS m = application1 "fromList" (toList m)

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

instance ShowBS TokenName where
  {-# INLINEABLE showBS #-}
  showBS (TokenName x) = application1 "TokenName" x

instance ShowBS CurrencySymbol where
  {-# INLINEABLE showBS #-}
  showBS (CurrencySymbol x) = application1 "CurrencySymbol" x

instance ShowBS Value where
  {-# INLINEABLE showBS #-}
  showBS (Value m) = application1 "Value" m

instance ShowBS TxId where
  {-# INLINEABLE showBS #-}
  showBS (TxId x) = application1 "TxId" x

instance ShowBS TxOutRef where
  {-# INLINEABLE showBS #-}
  showBS (TxOutRef txid i) = application2 "TxOutRef" txid i

instance ShowBS PubKeyHash where
  {-# INLINEABLE showBS #-}
  showBS (PubKeyHash h) = application1 "PubKeyHash" h

instance ShowBS Credential where
  {-# INLINEABLE showBS #-}
  showBS (ScriptCredential hash) = application1 "ScriptCredential" hash
  showBS (PubKeyCredential pkh) = application1 "PubKeyCredential" pkh

instance ShowBS StakingCredential where
  {-# INLINEABLE showBS #-}
  showBS (StakingHash cred) = application1 "StakingCredential" cred
  showBS (StakingPtr i j k) = application3 "StakingPointer" i j k

instance ShowBS Address where
  {-# INLINEABLE showBS #-}
  showBS (Address cred mStCred) = application2 "Address" cred mStCred

instance ShowBS DatumHash where
  {-# INLINEABLE showBS #-}
  showBS (DatumHash h) = application1 "DatumHash" h

instance ShowBS BuiltinData where
  {-# INLINEABLE showBS #-}
  showBS d = showBSParen $ "BuiltinData " <> showData d

{-# INLINEABLE showData #-}
showData :: BuiltinData -> BuiltinString
showData d =
  matchData
    d
    (\i ds -> showBSParen $ "Constr " <> showBS i <> " " <> catList "[" "," "]" showData ds)
    (\alist -> showBSParen $ "Map " <> catList "[" "," "]" (\(a, b) -> "(" <> showData a <> "," <> showData b <> ")") alist)
    (\list -> showBSParen $ "List " <> catList "[" "," "]" showData list)
    (\i -> showBSParen $ "I " <> showBS i)
    (\bs -> showBSParen $ "B " <> showBS bs)

instance ShowBS Datum where
  {-# INLINEABLE showBS #-}
  showBS (Datum d) = application1 "Datum" d

instance ShowBS OutputDatum where
  {-# INLINEABLE showBS #-}
  showBS NoOutputDatum = "NoOutputDatum"
  showBS (OutputDatumHash h) = application1 "OutputDatumHash" h
  showBS (OutputDatum d) = application1 "OutputDatum" d

instance ShowBS ScriptHash where
  {-# INLINEABLE showBS #-}
  showBS (ScriptHash h) = application1 "ScriptHash" h

instance ShowBS TxOut where
  {-# INLINEABLE showBS #-}
  showBS (TxOut address value datum mRefScriptHash) = application4 "TxOut" address value datum mRefScriptHash

instance ShowBS TxInInfo where
  {-# INLINEABLE showBS #-}
  showBS (TxInInfo oref out) = application2 "TxInInfo" oref out

instance ShowBS POSIXTime where
  {-# INLINEABLE showBS #-}
  showBS (POSIXTime t) = application1 "POSIXTime" t

instance (ShowBS a) => ShowBS (Extended a) where
  {-# INLINEABLE showBS #-}
  showBS NegInf = "NegInf"
  showBS PosInf = "PosInf"
  showBS (Finite x) = application1 "Finite" x

instance (ShowBS a) => ShowBS (LowerBound a) where
  {-# INLINEABLE showBS #-}
  showBS (LowerBound x closure) = application2 "LowerBound" x closure

instance (ShowBS a) => ShowBS (UpperBound a) where
  {-# INLINEABLE showBS #-}
  showBS (UpperBound x closure) = application2 "UpperBound" x closure

instance (ShowBS a) => ShowBS (Interval a) where
  {-# INLINEABLE showBS #-}
  showBS (Interval lb ub) = application2 "Interval" lb ub

instance ShowBS DRepCredential where
  {-# INLINEABLE showBS #-}
  showBS (DRepCredential cred) = application1 "DRep credential" cred

instance ShowBS DRep where
  {-# INLINEABLE showBS #-}
  showBS (DRep dRepCred) = application1 "DRep" dRepCred
  showBS DRepAlwaysAbstain = "DRep always abstain"
  showBS DRepAlwaysNoConfidence = "DRep always no confidence"

instance ShowBS Delegatee where
  {-# INLINEABLE showBS #-}
  showBS (DelegStake pkh) = application1 "Delegate stake" pkh
  showBS (DelegVote dRep) = application1 "Delegate vote" dRep
  showBS (DelegStakeVote pkh dRep) = application2 "Delegate stake vote" pkh dRep

-- Comments are copied for the definition of TxCert
instance ShowBS TxCert where
  {-# INLINEABLE showBS #-}
  -- \| Register staking credential with an optional deposit amount
  showBS (TxCertRegStaking cred maybeDepositAmount) = application2 "Register staking" cred maybeDepositAmount
  -- \| Un-Register staking credential with an optional refund amount
  showBS (TxCertUnRegStaking cred maybeDepositAmount) = application2 "Unregister staking" cred maybeDepositAmount
  -- \| Delegate staking credential to a Delegatee
  showBS (TxCertDelegStaking cred delegatee) = application2 "Delegate staking" cred delegatee
  -- \| Register and delegate staking credential to a Delegatee in one
  -- certificate. Noter that deposit is mandatory.
  showBS (TxCertRegDeleg cred delegatee depositAmount) = application3 "Register and delegate staking" cred delegatee depositAmount
  -- \| Register a DRep with a deposit value. The optional anchor is omitted.
  showBS (TxCertRegDRep dRepCred amount) = application2 "Register DRep" dRepCred amount
  -- \| Update a DRep. The optional anchor is omitted.
  showBS (TxCertUpdateDRep dRepCred) = application1 "Update DRep" dRepCred
  -- \| UnRegister a DRep with mandatory refund value
  showBS (TxCertUnRegDRep dRepCred amount) = application2 "Unregister DRep" dRepCred amount
  -- -- \| A digest of the PoolParams (with poolId and pool VFR)
  showBS (TxCertPoolRegister poolId poolVFR) = application2 "Register to pool" poolId poolVFR
  -- -- \| The retirement certificate and the Epoch in which the
  -- -- retirement will take place
  showBS (TxCertPoolRetire pkh epoch) = application2 "Retire from pool" pkh epoch
  -- \| Authorize a Hot credential for a specific Committee member's cold credential
  showBS (TxCertAuthHotCommittee coldCommitteeCred hotCommitteeCred) = application2 "Authorize hot committee " coldCommitteeCred hotCommitteeCred
  showBS (TxCertResignColdCommittee coldCommitteeCred) = application1 "Resign cold committee" coldCommitteeCred

instance ShowBS Voter where
  {-# INLINEABLE showBS #-}
  showBS (CommitteeVoter hotCommitteeCred) = application1 "Committee Voter" hotCommitteeCred
  showBS (DRepVoter dRepCred) = application1 "DRep Voter" dRepCred
  showBS (StakePoolVoter pkh) = application1 "Stake Pool Voter" pkh

instance ShowBS GovernanceActionId where
  {-# INLINEABLE showBS #-}
  showBS GovernanceActionId {..} =
    showBSParen
      $ "Tx Id:"
      <> showBS gaidTxId
      <> "Action Id:"
      <> showBS gaidGovActionIx

instance ShowBS ScriptPurpose where
  {-# INLINEABLE showBS #-}
  showBS (Minting cs) = application1 "Minting" cs
  showBS (Spending oref) = application1 "Spending" oref
  showBS (Rewarding stCred) = application1 "Rewarding" stCred
  showBS (Certifying nb txCert) = application2 "Certifying" nb txCert
  showBS (Voting voter) = application1 "Voting" voter
  showBS (Proposing nb proposal) = application2 "Proposing" nb proposal

instance ShowBS Redeemer where
  {-# INLINEABLE showBS #-}
  showBS (Redeemer builtinData) = application1 "Redeemer" builtinData

instance ShowBS Vote where
  {-# INLINEABLE showBS #-}
  showBS VoteNo = "No"
  showBS VoteYes = "Yes"
  showBS Abstain = "Abstain"

instance ShowBS ChangedParameters where
  {-# INLINEABLE showBS #-}
  showBS (ChangedParameters builtinData) = application1 "Changed parameters" builtinData

instance ShowBS ProtocolVersion where
  {-# INLINEABLE showBS #-}
  showBS (ProtocolVersion major minor) = application2 "Protocol version" major minor

instance ShowBS Constitution where
  {-# INLINEABLE showBS #-}
  showBS (Constitution constitutionScript) = application1 "Constitution" constitutionScript

instance ShowBS ColdCommitteeCredential where
  {-# INLINEABLE showBS #-}
  showBS (ColdCommitteeCredential cred) = application1 "Cold committee credential" cred

instance ShowBS HotCommitteeCredential where
  {-# INLINEABLE showBS #-}
  showBS (HotCommitteeCredential cred) = application1 "Hot committee credential" cred

instance ShowBS Committee where
  {-# INLINEABLE showBS #-}
  showBS (Committee committeeMembers committeeQuorum) = application2 "Committee" committeeMembers committeeQuorum

instance ShowBS Lovelace where
  {-# INLINEABLE showBS #-}
  showBS (Lovelace amount) = application1 "Lovelace" amount

instance ShowBS Rational where
  {-# INLINEABLE showBS #-}
  showBS rat = application2 "Rational" (numerator rat) (denominator rat)

instance ShowBS GovernanceAction where
  {-# INLINEABLE showBS #-}
  showBS (ParameterChange maybeActionId changeParams mScriptHash) = application3 "Parameter change" maybeActionId changeParams mScriptHash
  showBS (HardForkInitiation maybeActionId protocolVersion) = application2 "HardForkInitiation" maybeActionId protocolVersion
  showBS (TreasuryWithdrawals mapCredLovelace mScriptHash) = application2 "TreasuryWithdrawals" mapCredLovelace mScriptHash
  showBS (NoConfidence maybeActionId) = application1 "NoConfidence" maybeActionId
  showBS (UpdateCommittee maybeActionId toRemoveCreds toAddCreds quorum) = application4 "Info action" maybeActionId toRemoveCreds toAddCreds quorum
  showBS (NewConstitution maybeActionId constitution) = application2 "NewConstitution" maybeActionId constitution
  showBS InfoAction = "InfoAction" --

instance ShowBS ProposalProcedure where
  {-# INLINEABLE showBS #-}
  showBS ProposalProcedure {..} =
    showBSParen
      $ "deposit:"
      <> showBS ppDeposit
      <> "return address"
      <> showBS ppReturnAddr
      <> "governance action"
      <> showBS ppGovernanceAction

instance ShowBS TxInfo where
  {-# INLINEABLE showBS #-}
  showBS TxInfo {..} =
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
      <> "wdrl:" -- TODO: what is wdrl? Explain better here
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

instance ShowBS ScriptContext where
  {-# INLINEABLE showBS #-}
  showBS ScriptContext {..} =
    showBSParen
      $ "Script context:"
      <> "Script Tx info:"
      <> showBS scriptContextTxInfo
      <> "Script purpose:"
      <> showBS scriptContextPurpose
