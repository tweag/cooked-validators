{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module exposes aliases to conveniently use versioned script and tools
-- to manipulate a script alongside its redeemer.
module Cooked.Skeleton.User
  ( VScript,
    ToVScript,
    toVScript,
    UserMode (..),
    UserKind (..),
    User (..),
    type (∈),
    userCredentialG,
    userTxSkelRedeemerAT,
    userVScriptAT,
    userScriptHashAF,
    userPubKeyHashAT,
    userPubKeyHashI,
    userVScriptL,
    userScriptHashG,
    userTxSkelRedeemerL,
    userEitherScriptP,
    userEitherPubKeyP,
    userTypedAF,
    userTypedScriptAT,
    userTypedPubKeyAT,
  )
where

import Cooked.Skeleton.Redeemer
import Data.Kind
import Data.Typeable
import GHC.TypeLits
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

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
data UserKind = IsScript | IsPubKey | IsEither | IsNone
  deriving (Eq, Show)

-- * Membership type family

-- | A type family representing membership. This requires @UndecidableInstances@
-- because the type checker is not smart enough to understand that this type
-- family decreases in @els@, due to the presence of @extras@. @extras@ is used
-- to keep track of the original list and output a relevant message in the empty
-- case, which could otherwise be omitted altogther at no loss of type safety.
type family Member (el :: a) (els :: [a]) (extras :: [a]) :: Constraint where
  Member x (x ': xs) _ = ()
  Member x (y ': xs) l = Member x xs (y ': l)
  Member x '[] l = TypeError ('ShowType x ':<>: 'Text " is not a member of " ':<>: 'ShowType l)

-- | A specific instance of @Member@ where the already browsed elements is @[]@
type (∈) el els = Member el els '[]

-- | Building users. The type exposes the mode for which the user has been
-- built, and the requirements on the kind of the user.
data User :: UserKind -> UserMode -> Type where
  -- | A pubkey user. This can be used whenever a pubkey is needed, and for
  -- either of the possible modes.
  UserPubKey :: forall pkh kind mode. (kind ∈ '[IsPubKey, IsEither], Script.ToPubKeyHash pkh, Typeable pkh) => pkh -> User kind mode
  -- | A script user. This can be used whenever a script is needed, but only for
  -- the allocation mode.
  UserScript :: forall script kind. (kind ∈ '[IsScript, IsEither], ToVScript script, Typeable script) => script -> User kind Allocation
  -- | A script user with an associated redeemer. This can be used whenever a
  -- script is needed for redemption mode.
  UserRedeemedScript :: forall script kind. (kind ∈ [IsScript, IsEither], ToVScript script, Typeable script) => script -> TxSkelRedeemer -> User kind Redemption

instance Show (User kind mode) where
  show (UserPubKey (Script.toPubKeyHash -> pkh)) = "UserPubKey " <> show pkh
  show (UserScript (toVScript -> vScript)) = "UserScript " <> show (Script.toScriptHash vScript)
  show (UserRedeemedScript (toVScript -> vScript) red) = "UserRedeemedScript " <> show (Script.toScriptHash vScript) <> " " <> show red

instance Eq (User kind mode) where
  (UserPubKey (Script.toPubKeyHash -> pkh)) == (UserPubKey (Script.toPubKeyHash -> pkh')) =
    pkh == pkh'
  (UserScript (Script.toScriptHash . toVScript -> sHash)) == (UserScript (Script.toScriptHash . toVScript -> sHash')) =
    sHash == sHash'
  (UserRedeemedScript (Script.toScriptHash . toVScript -> sHash) red) == (UserRedeemedScript (Script.toScriptHash . toVScript -> sHash') red') =
    sHash == sHash' && red == red'
  _ == _ = False

instance Ord (User kind mode) where
  compare (UserPubKey {}) (UserScript {}) = LT
  compare (UserPubKey {}) (UserRedeemedScript {}) = LT
  compare (UserScript {}) (UserPubKey {}) = GT
  compare (UserRedeemedScript {}) (UserPubKey {}) = GT
  compare (UserPubKey (Script.toPubKeyHash -> pkh)) (UserPubKey (Script.toPubKeyHash -> pkh')) = compare pkh pkh'
  compare (UserScript (Script.toScriptHash . toVScript -> sh)) (UserScript (Script.toScriptHash . toVScript -> sh')) = compare sh sh'
  compare (UserRedeemedScript (Script.toScriptHash . toVScript -> sh) red) (UserRedeemedScript (Script.toScriptHash . toVScript -> sh') red') =
    compare (sh, red) (sh', red')

instance Script.ToCredential (User kind mode) where
  toCredential (UserPubKey (Script.toPubKeyHash -> pkh)) = Script.toCredential pkh
  toCredential (UserScript (toVScript -> vScript)) = Script.toCredential vScript
  toCredential (UserRedeemedScript (toVScript -> vScript) _) = Script.toCredential vScript

-- * Optics on various possible families of users

-- | Retrieves a possible typed user from a 'User'
userTypedAF :: forall user kind mode. (Typeable user) => AffineFold (User kind mode) user
userTypedAF =
  afolding
    ( \case
        UserPubKey @user' pkh | Just Refl <- eqT @user @user' -> Just pkh
        UserScript @user' script | Just Refl <- eqT @user @user' -> Just script
        UserRedeemedScript @user' script _ | Just Refl <- eqT @user @user' -> Just script
        _ -> Nothing
    )

-- | Focuses on a possible typed script in this 'User'
userTypedScriptAT :: forall userScript mode. (ToVScript userScript, Typeable userScript) => AffineTraversal' (User IsScript mode) userScript
userTypedScriptAT =
  atraversal
    ( \user -> case user of
        UserScript @userScript' script | Just Refl <- eqT @userScript @userScript' -> Right script
        UserRedeemedScript @userScript' script _ | Just Refl <- eqT @userScript @userScript' -> Right script
        _ -> Left user
    )
    ( \case
        UserScript _ -> UserScript
        UserRedeemedScript _ red -> (`UserRedeemedScript` red)
    )

-- | Focuses on a possible typed pubkey in this 'User'
userTypedPubKeyAT :: forall userPK mode. (Script.ToPubKeyHash userPK, Typeable userPK) => AffineTraversal' (User IsPubKey mode) userPK
userTypedPubKeyAT =
  atraversal
    ( \user -> case user of
        UserPubKey @userPK' pkh | Just Refl <- eqT @userPK @userPK' -> Right pkh
        _ -> Left user
    )
    (\(UserPubKey _) -> UserPubKey)

-- | Builds a @User IsEither@ from a @User IsScript@
userEitherScriptP :: Prism' (User IsEither mode) (User IsScript mode)
userEitherScriptP =
  prism
    ( \case
        UserScript script -> UserScript script
        UserRedeemedScript script red -> UserRedeemedScript script red
    )
    ( \case
        UserScript script -> Right (UserScript script)
        UserRedeemedScript script red -> Right (UserRedeemedScript script red)
        user -> Left user
    )

-- | Builds a @User IsEither@ from a @User IsPubKey@
userEitherPubKeyP :: Prism' (User IsEither mode) (User IsPubKey mode)
userEitherPubKeyP =
  prism
    (\(UserPubKey pkh) -> UserPubKey pkh)
    ( \case
        UserPubKey pkh -> Right (UserPubKey pkh)
        user -> Left user
    )

-- | Extracts the 'Api.Credential' from a 'User'
userCredentialG :: Getter (User kind mode) Api.Credential
userCredentialG = to Script.toCredential

-- | Focusing on the possible 'TxSkelRedeemer' of this 'User'
userTxSkelRedeemerAT :: AffineTraversal' (User kind mode) TxSkelRedeemer
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
userVScriptAT :: AffineTraversal' (User kind mode) VScript
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
userScriptHashAF :: AffineFold (User kind mode) Api.ScriptHash
userScriptHashAF = userVScriptAT % to Script.toScriptHash

-- | Focusing on the possible 'Api.PubKeyHash' of this 'User'
userPubKeyHashAT :: AffineTraversal' (User kind mode) Api.PubKeyHash
userPubKeyHashAT =
  atraversal
    ( \case
        UserPubKey (Script.toPubKeyHash -> pkh) -> Right pkh
        user -> Left user
    )
    ( \case
        UserPubKey _ -> UserPubKey
        user -> const user
    )

-- | An isomorphism between users required to be pubkeys and 'Api.PubKeyHash'
userPubKeyHashI :: Iso' (User IsPubKey mode) Api.PubKeyHash
userPubKeyHashI =
  iso
    (\(UserPubKey (Script.toPubKeyHash -> pkh)) -> pkh)
    UserPubKey

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
