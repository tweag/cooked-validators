-- | This module provides an automated attack to mint and give extra tokens to a
-- certain wallet.
module Cooked.Attack.AddToken (addTokenAttack, AddTokenLbl (..)) where

import Control.Monad
import Cooked.Pretty
import Cooked.Skeleton
import Cooked.Tweak
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as PMap
import Prettyprinter qualified as PP

-- | This attack adds extra tokens, depending on the minting policy. It is
-- different from the 'Cooked.Attack.DupToken.dupTokenAttack' in that it does
-- not merely try to increase the amount of tokens minted: It tries to mint
-- tokens of asset classes that were not necessarily present on the unmodified
-- transaction.
--
-- This attack adds an 'AddTokenLbl' label.
addTokenAttack ::
  (MonadTweak m, OwnerConstrs o) =>
  -- | For each policy that occurs in some 'Mint' constraint, return a list of
  -- token names together with how many tokens with that name should be minted.
  (Script.Versioned Script.MintingPolicy -> [(Api.TokenName, Integer)]) ->
  -- | The wallet of the attacker where extra tokens will be paid to
  o ->
  m Api.Value
addTokenAttack extraTokens attacker = do
  oldMintsList <- viewTweak $ txSkelMintsL % to txSkelMintsToList
  let (newMintsList, totalIncrement) =
        foldl
          ( \(newMs, addVal) (Mint mp@(Script.toVersioned @Script.MintingPolicy -> mp') red tks) ->
              let change = extraTokens mp'
               in ( Mint mp red (tks ++ change) : newMs,
                    Api.Value (PMap.singleton (Script.toCurrencySymbol mp') (PMap.unsafeFromList change)) <> addVal
                  )
          )
          ([], mempty)
          oldMintsList
  guard (totalIncrement /= mempty)
  setTweak txSkelMintsL $ txSkelMintsFromList newMintsList
  addOutputTweak $ attacker `receives` Value totalIncrement
  addLabelTweak AddTokenLbl
  return totalIncrement

-- | A label that is added to a 'TxSkel' that has successfully been modified by
-- 'addTokenAttack'
data AddTokenLbl = AddTokenLbl deriving (Show, Eq, Ord)

instance PrettyCooked AddTokenLbl where
  prettyCooked = PP.viaShow
