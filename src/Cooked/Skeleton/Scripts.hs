-- | This module exposes aliases to conveniently use versioned script and tools
-- to manipulate a script alongside its redeemer.
module Cooked.Skeleton.Scripts where

import Cooked.Skeleton.Redeemer
import Data.Kind
import GHC.TypeLits
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.Data.V3 qualified as Api

-- * Handy aliases around versioned scripts

-- | 'VScript' is a convenient alias as we have versioned scripts everywhere.
type VScript = Script.Versioned Script.Script

-- | The 'ToVScript' alias will come in handy when dealing with constrains.
type ToVScript = Script.ToVersioned Script.Script

-- | The 'toVScript' alias will come in handy to default the type parameter of
-- 'Script.toVersioned' to 'Script.Script'.
toVScript :: (ToVScript script) => script -> VScript
toVScript = Script.toVersioned

-- * A depiction of user kinds and modes

-- | The 'UserMode' corresponds to the way the user will be used in our
-- 'Cooked.Skeleton.TxSkel' which can either be for allocation (allocation a
-- certain entity to a user) or for redemption (using this user as a witness in
-- a transaction).
data UserMode = Allocation | Redemption
  deriving (Eq, Show)

-- | The 'UserKind' corresponds to the requirement on the type of users. Some
-- elements will require specifically a script and some others a pubkey.
data UserKind = IsScript | IsPubKey | IsNone
  deriving (Eq, Show)

-- * A depiction of requirements over users

-- | Requirements to be satisfied by users. They can either be required to be
-- scripts, pubkeys, either of those, or nothing at all.
data UserReq = ReqScript | ReqPubKey | ReqEither | ReqNone
  deriving (Eq, Show)

-- | Which user kind satsifies which user requirements
type family (⊨) (kind :: UserKind) (req :: UserReq) :: Constraint where
  IsScript ⊨ ReqScript = ()
  IsScript ⊨ ReqEither = ()
  IsPubKey ⊨ ReqPubKey = ()
  IsPubKey ⊨ ReqEither = ()
  IsScript ⊨ ReqPubKey = TypeError ('Text "Only public key users allowed")
  IsPubKey ⊨ ReqScript = TypeError ('Text "Only script users allowed")
  IsNone ⊨ ReqNone = ()
  IsNone ⊨ a = TypeError ('Text "A user is needed")
  a ⊨ ReqNone = TypeError ('Text "No user is allowed")

-- | Building users. The type exposes the mode for which the user has been
-- built, and the requirements on the kind of the user.
data User :: UserKind -> UserMode -> Type where
  -- | A pubkey user. This can be used whenever a pubkey is needed, and for
  -- either of the possible modes.
  UserPubKeyHash :: (Script.ToPubKeyHash pkh) => pkh -> User IsPubKey mode
  -- | A script user. This can be used whenever a script is needed, but only for
  -- the allocation mode.
  UserScript :: (ToVScript script) => script -> User IsScript Allocation
  -- | A script user with an associated redeemer. This can be used whenever a
  -- script is needed for redemption mode.
  UserRedeemedScript :: (ToVScript script) => script -> TxSkelRedeemer -> User IsScript Redemption

instance Show (User req mode) where
  show (UserPubKeyHash (Script.toPubKeyHash -> pkh)) = "UserPubKeyHash " <> show pkh
  show (UserScript (toVScript -> vScript)) = "UserScript " <> show (Script.toScriptHash vScript)
  show (UserRedeemedScript (toVScript -> vScript) red) = "UserRedeemedScript " <> show (Script.toScriptHash vScript) <> " " <> show red

instance Eq (User req mode) where
  (UserPubKeyHash (Script.toPubKeyHash -> pkh))
    == (UserPubKeyHash (Script.toPubKeyHash -> pkh')) = pkh == pkh'
  (UserScript (Script.toScriptHash . toVScript -> sHash))
    == (UserScript (Script.toScriptHash . toVScript -> sHash')) = sHash == sHash'
  (UserRedeemedScript (Script.toScriptHash . toVScript -> sHash) red)
    == (UserRedeemedScript (Script.toScriptHash . toVScript -> sHash') red') = sHash == sHash' && red == red'

instance Script.ToCredential (User req mode) where
  toCredential (UserPubKeyHash (Script.toPubKeyHash -> pkh)) = Script.toCredential pkh
  toCredential (UserScript (toVScript -> vScript)) = Script.toCredential vScript
  toCredential (UserRedeemedScript (toVScript -> vScript) _) = Script.toCredential vScript

-- | There are many cases where some sub families of users will be used as keys
-- in a map. In those cases, we never want to compare the redeemer, thus this
-- instance only compares the hashes.
instance Ord (User req mode) where
  compare
    (UserPubKeyHash (Script.toPubKeyHash -> pkh))
    (UserPubKeyHash (Script.toPubKeyHash -> pkh')) = compare pkh pkh'
  compare
    (UserScript (Script.toScriptHash . toVScript -> sHash))
    (UserScript (Script.toScriptHash . toVScript -> sHash')) = compare sHash sHash'
  compare
    (UserRedeemedScript (Script.toScriptHash . toVScript -> sHash) _)
    (UserRedeemedScript (Script.toScriptHash . toVScript -> sHash') _) = compare sHash sHash'

-- * Optics on various possible families of users

-- | Focusing on the possible 'TxSkelRedeemer' of this 'User'
userTxSkelRedeemerAT :: AffineTraversal' (User req mode) TxSkelRedeemer
userTxSkelRedeemerAT =
  atraversal
    ( \case
        UserRedeemedScript _ red -> Right red
        user -> Left user
    )
    ( \case
        UserRedeemedScript script _ -> UserRedeemedScript script
        user -> const user
    )

-- | Focusing on the possible 'VScript' of this 'User'
userVScriptAT :: AffineTraversal' (User req mode) VScript
userVScriptAT =
  atraversal
    ( \case
        UserScript (toVScript -> vScript) -> Right vScript
        UserRedeemedScript (toVScript -> vScript) _ -> Right vScript
        user -> Left user
    )
    ( \case
        UserScript _ -> UserScript
        UserRedeemedScript _ red -> (`UserRedeemedScript` red)
        user -> const user
    )

-- | Focusing on the possible 'Api.ScriptHash' of this 'User'
userScriptHashAT :: AffineFold (User req mode) Api.ScriptHash
userScriptHashAT = userVScriptAT % to Script.toScriptHash

-- | Focusing on the possible 'Api.PubKeyHash' of this 'User'
userPubKeyHashAT :: AffineTraversal' (User req mode) Api.PubKeyHash
userPubKeyHashAT =
  atraversal
    ( \case
        UserPubKeyHash (Script.toPubKeyHash -> pkh) -> Right pkh
        user -> Left user
    )
    ( \case
        UserPubKeyHash _ -> UserPubKeyHash
        user -> const user
    )

-- | An isomorphism between users required to be pubkeys and 'Api.PubKeyHash'
userPubKeyHashI :: Iso' (User IsPubKey a) Api.PubKeyHash
userPubKeyHashI =
  iso
    (\(UserPubKeyHash (Script.toPubKeyHash -> pkh)) -> pkh)
    UserPubKeyHash

-- | Focusing on the 'VScript' from a script
userVScriptL :: Lens' (User IsScript mode) VScript
userVScriptL =
  lens
    ( \case
        UserScript (toVScript -> vScript) -> vScript
        UserRedeemedScript (toVScript -> vScript) _ -> vScript
    )
    ( \case
        UserScript _ -> UserScript
        UserRedeemedScript _ red -> (`UserRedeemedScript` red)
    )

-- | Focusing on the 'Api.ScriptHash' from a script
userScriptHashG :: Getter (User IsScript mode) Api.ScriptHash
userScriptHashG = userVScriptL % to Script.toScriptHash

-- | Focus on the 'TxSkelRedeemer' from a script being redeemed
userTxSkelRedeemerL :: Lens' (User IsScript Redemption) TxSkelRedeemer
userTxSkelRedeemerL =
  lens
    (\(UserRedeemedScript _ red) -> red)
    (\(UserRedeemedScript script _) -> UserRedeemedScript script)
