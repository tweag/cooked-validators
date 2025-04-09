-- | This module defines 'Cooked.Tweak.Common.Tweak's revolving around the
-- signers of a transaction. They assume but do not ensure that the list of
-- signers is free of duplicates.
module Cooked.Tweak.Signers
  ( getSignersTweak,
    modifySignersTweak,
    setSignersTweak,
    signersSatisfyTweak,
    isSignerTweak,
    hasSignersTweak,
    addFirstSignerTweak,
    addSignersTweak,
    addLastSignerTweak,
    removeSignersTweak,
    removeSignerTweak,
    replaceFirstSignerTweak,
  )
where

import Cooked.Skeleton (txSkelSignersL)
import Cooked.Tweak.Common (MonadTweak, setTweak, viewTweak)
import Cooked.Wallet (Wallet)
import Data.List (delete, (\\))

-- | Returns the current list of signers
getSignersTweak :: (MonadTweak m) => m [Wallet]
getSignersTweak = viewTweak txSkelSignersL

-- | Apply a function to the list of signers and return the old ones
modifySignersTweak :: (MonadTweak m) => ([Wallet] -> [Wallet]) -> m [Wallet]
modifySignersTweak f = do
  oldSigners <- getSignersTweak
  setTweak txSkelSignersL (f oldSigners)
  return oldSigners

-- | Change the current signers and return the old ones
setSignersTweak :: (MonadTweak m) => [Wallet] -> m [Wallet]
setSignersTweak = modifySignersTweak . const

-- | Check if the signers satisfy a certain predicate
signersSatisfyTweak :: (MonadTweak m) => ([Wallet] -> Bool) -> m Bool
signersSatisfyTweak = (<$> getSignersTweak)

-- | Check if a wallet signs a transaction
isSignerTweak :: (MonadTweak m) => Wallet -> m Bool
isSignerTweak = signersSatisfyTweak . elem

-- | Check if the transaction has at least a signer
hasSignersTweak :: (MonadTweak m) => m Bool
hasSignersTweak = signersSatisfyTweak (not . null)

-- | Add a signer to the transaction, at the head of the list of signers, and
-- return the old list of signers
addFirstSignerTweak :: (MonadTweak m) => Wallet -> m [Wallet]
addFirstSignerTweak = modifySignersTweak . (:)

-- | Add signers at the end of the list of signers, and return the old list of
-- signers
addSignersTweak :: (MonadTweak m) => [Wallet] -> m [Wallet]
addSignersTweak = modifySignersTweak . (<>)

-- | Add a signer to the transaction, at the end of the list of signers, and
-- return the old list of signers
addLastSignerTweak :: (MonadTweak m) => Wallet -> m [Wallet]
addLastSignerTweak = addSignersTweak . (: [])

-- | Remove signers from the transaction and return the old list of signers
removeSignersTweak :: (MonadTweak m) => [Wallet] -> m [Wallet]
removeSignersTweak = modifySignersTweak . (\\)

-- | Remove a signer from the transaction and return the old list of signers
removeSignerTweak :: (MonadTweak m) => Wallet -> m [Wallet]
removeSignerTweak = modifySignersTweak . delete

-- | Changes the first signer (adds it if there are no signers) and return the
-- old list of signers.
replaceFirstSignerTweak :: (MonadTweak m) => Wallet -> m [Wallet]
replaceFirstSignerTweak =
  modifySignersTweak
    . ( \newSigner -> \case
          [] -> [newSigner]
          (_ : ss) -> newSigner : ss
      )
