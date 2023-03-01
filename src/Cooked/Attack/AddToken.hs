module Cooked.Attack.AddToken (addTokenAttack, AddTokenLbl (..)) where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak
import Cooked.Wallet
import qualified Data.Map as Map
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.V1.Ledger.Value as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx.Numeric as Pl

-- | This attack adds extra tokens, depending on the minting policy. It is
-- different from the 'dupTokenAttack' in that it does not merely try to
-- increase the amount of tokens minted: It tries to mint tokens of asset
-- classes that were not necessarily present on the unmodified transaction.
--
-- This attack adds an 'AddTokenLbl' with the token name of the additional
-- minted token(s). It returns additional value minted.
addTokenAttack ::
  MonadTweak m =>
  -- | For each policy that occurs in some 'Mints' constraint, return a list of
  -- token names together with how many tokens with that name should be
  -- minted.
  --
  -- For each of the elements of the returned list, one modified transaction
  -- with the additional tokens will be generated. (This means for example that,
  -- if there were three minting policies on the original transaction, and the
  -- lists returned for each of them have n,m, and o elements, respectively,
  -- there'll be n*m*o modified transactions.)
  --
  -- The redeemer will be the one that's already being used on the transaction.
  (Pl.Versioned Pl.MintingPolicy -> [(Pl.TokenName, Integer)]) ->
  -- | The wallet of the attacker. Any extra tokens will be paid to this wallet.
  Wallet ->
  m Pl.Value
addTokenAttack extraTokens attacker = do
  oldMints <- viewTweak txSkelMintsL
  msum $
    map
      ( \(policy, (redeemer, _)) ->
          msum $
            map
              ( \(tName, amount) ->
                  let newMints = addToTxSkelMints (policy, redeemer, tName, amount) oldMints
                      increment =
                        txSkelMintsValue newMints
                          <> Pl.negate (txSkelMintsValue oldMints)
                   in if increment `Pl.geq` mempty
                        then do
                          setTweak txSkelMintsL newMints
                          addOutputTweak $ paysPK (walletPKHash attacker) increment
                          return increment
                        else failingTweak
              )
              (extraTokens policy)
      )
      (Map.toList oldMints)

newtype AddTokenLbl = AddTokenLbl Pl.TokenName deriving (Show, Eq)
