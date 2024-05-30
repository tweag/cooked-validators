-- | This module provides an automated attack to mint and give extra tokens to a
-- certain wallet.
module Cooked.Attack.AddToken (addTokenAttack, AddTokenLbl (..)) where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak
import Cooked.Wallet
import Data.Map qualified as Map
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Numeric qualified as ScriptutusTx

-- | This attack adds extra tokens, depending on the minting policy. It is
-- different from the 'dupTokenAttack' in that it does not merely try to
-- increase the amount of tokens minted: It tries to mint tokens of asset
-- classes that were not necessarily present on the unmodified transaction.
--
-- This attack adds an 'AddTokenLbl' with the token name of the additional
-- minted token(s). It returns additional value minted.
addTokenAttack ::
  (MonadTweak m) =>
  -- | For each policy that occurs in some 'Mints' constraint, return a list of
  -- token names together with how many tokens with that name should be minted.
  --
  -- For each of the elements of the returned list, one modified transaction
  -- with the additional tokens will be generated. (This means for example that,
  -- if there were three minting policies on the original transaction, and the
  -- lists returned for each of them have n,m, and o elements, respectively,
  -- there'll be n*m*o modified transactions.)
  --
  -- The redeemer will be unchanged
  (Script.Versioned Script.MintingPolicy -> [(Script.TokenName, Integer)]) ->
  -- | The wallet of the attacker where extra tokens will be paid to
  Wallet ->
  m Api.Value
addTokenAttack extraTokens attacker = do
  oldMints <- viewTweak txSkelMintsL
  msum $
    map
      ( \(policy, (redeemer, _)) ->
          msum $
            map
              ( \(tName, amount) ->
                  let newMints = addToTxSkelMints (policy, redeemer, tName, amount) oldMints
                      increment = txSkelMintsValue newMints <> ScriptutusTx.negate (txSkelMintsValue oldMints)
                   in if increment `Script.geq` mempty
                        then do
                          setTweak txSkelMintsL newMints
                          addOutputTweak $ paysPK attacker increment
                          return increment
                        else failingTweak
              )
              (extraTokens policy)
      )
      (Map.toList oldMints)

newtype AddTokenLbl = AddTokenLbl Script.TokenName deriving (Show, Eq)
