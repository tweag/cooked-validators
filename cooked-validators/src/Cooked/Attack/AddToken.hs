module Cooked.Attack.AddToken where

import Control.Monad
import Cooked.Attack.Tweak
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import qualified Ledger as Pl
import qualified Ledger.Typed.Scripts as Pl
import qualified Ledger.Value as Pl
import Optics.Core
import qualified Plutus.Script.Utils.V1.Scripts as Pl

-- | This attack adds extra tokens, depending on the minting policy. It is
-- different from the 'dupTokenAttack' in that it does not merely try to
-- increase the amount of tokens minted: It tries to mint tokens of asset
-- classes that were not necessarily present on the unmodified transaction.
--
-- This attack adds an 'AddTokenLbl' with the token name of the additional
-- minted token(s). It returns additional value minted.
addTokenAttack ::
  -- | For each policy that occurs in some 'Mints' constraint, return a list of
  -- token names together with how many tokens with that name be minted, using
  -- the same redeemer as on the original transaction. For each of the elements
  -- of the returned list, one modified transaction will be tried.
  (Pl.MintingPolicy -> [(Pl.TokenName, Integer)]) ->
  -- | The wallet of the attacker. Any extra tokens will be paid to this wallet.
  Wallet ->
  Tweak Pl.Value
addTokenAttack extraTokens attacker = do
  mintsCs <- viewTweak $ partsOf mintsConstraintsT
  msum $
    map
      ( \(MintsConstraint redeemer pols _value) ->
          msum $
            map
              ( \pol ->
                  msum $
                    map
                      ( \(extraTn, amount) ->
                          let extraValue =
                                Pl.assetClassValue
                                  (Pl.assetClass (Pl.scriptCurrencySymbol pol) extraTn)
                                  amount
                           in do
                                _ <- addMiscConstraintTweak $ Mints redeemer [pol] extraValue
                                addLabelTweak $ AddTokenLbl extraTn
                                addOutConstraintTweak $ paysPK (walletPKHash attacker) extraValue
                                return extraValue
                      )
                      $ extraTokens pol
              )
              pols
      )
      mintsCs

newtype AddTokenLbl = AddTokenLbl Pl.TokenName deriving (Show, Eq)
