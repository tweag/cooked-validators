{-# LANGUAGE OverloadedStrings #-}

module Cooked.MockChain.UtxoState where

import Control.Arrow (second)
import Cooked.Currencies
import Cooked.MockChain.Wallet
import Data.Function (on)
import qualified Data.List as L
import qualified Data.List as List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Ledger as Pl
import qualified Ledger.Ada as Ada
import qualified Ledger.Credential as Pl
import qualified Ledger.Value as Pl
import qualified PlutusTx.AssocMap as Pl
import qualified PlutusTx.Numeric as Pl
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP

-- | A 'UtxoState' provides us with the mental picture of the state of the UTxO graph:
-- Each address has a set of UTxOs that consist in a value and some potential datum.
newtype UtxoState = UtxoState {utxoState :: M.Map Pl.Address UtxoValueSet}
  deriving (Eq)

instance Semigroup UtxoState where
  (UtxoState a) <> (UtxoState b) = UtxoState $ M.unionWith (<>) a b

-- | Represents a /set/ of values, yet, we use a list instead of a set because 'Pl.Value'
-- doesn't implement 'Ord' and because it is possible that we want to distinguish between utxo states
-- that have additional utxos, even if these could have been merged together.
newtype UtxoValueSet = UtxoValueSet {utxoValueSet :: [(Pl.Value, Maybe UtxoDatum)]}
  deriving (Show)

instance Eq UtxoValueSet where
  (UtxoValueSet xs) == (UtxoValueSet ys) = xs' == ys'
    where
      k (val, m) = (Pl.flattenValue val, m)
      xs' = L.sortBy (compare `on` k) xs
      ys' = L.sortBy (compare `on` k) ys

instance Semigroup UtxoValueSet where
  UtxoValueSet a <> UtxoValueSet b = UtxoValueSet $ a ++ b

instance Monoid UtxoValueSet where
  mempty = UtxoValueSet []

-- | Computes the total value in a set
utxoValueSetTotal :: UtxoValueSet -> Pl.Value
utxoValueSetTotal = mconcat . map fst . utxoValueSet

-- | Computes the total value in the entire state
utxoStateTotal :: UtxoState -> Pl.Value
utxoStateTotal = mconcat . map utxoValueSetTotal . M.elems . utxoState

-- | A 'UtxoDatum' contains a datum which is @Datum $ Pl.toBuiltinData x@ for some @x :: X@,
-- but we also include @show x@ to be able to print this value in a more user friendly fashion.
data UtxoDatum = UtxoDatum {utxoDatum :: Pl.Datum, utxoShow :: String}
  deriving (Eq, Ord)

instance Show UtxoDatum where
  show = utxoShow

instance Show UtxoState where
  show = show . prettyUtxoState

-- * Differences between two 'UtxoState'

-- | The differences between two states @a@ and @b@ is a map whose keys
-- are @M.keys a `union` M.keys b@ and values dictate what happened to that
-- particular value set with a 'UtxoValueSetDiff'
type UtxoStateDiff = M.Map Pl.Address UtxoValueSetDiff

-- | A 'UtxoValueSet' can have been inserted, deleted or modified.
data UtxoValueSetDiff
  = Inserted UtxoValueSet
  | Deleted UtxoValueSet
  | -- | In case it has been modified, the order is irrelevant; hence we just need to keep the deltas around.
    Modified
      { modDeleted :: UtxoValueSet,
        modInserted :: UtxoValueSet,
        modUnchanged :: UtxoValueSet
      }
  deriving (Show)

-- | Computes the numeric difference between the new total and the old total
-- of the source and destinations of this diff of set of values
utxoValueSetDiffTotal :: UtxoValueSetDiff -> Pl.Value
utxoValueSetDiffTotal (Inserted ins) = utxoValueSetTotal ins
utxoValueSetDiffTotal (Deleted del) = Pl.negate $ utxoValueSetTotal del
utxoValueSetDiffTotal (Modified del ins _) = utxoValueSetTotal ins <> Pl.negate (utxoValueSetTotal del)

-- | Computes the total difference in value between a new state and an old state:
-- @utxoStateDiff new old@. If this difference is positive for some token @t@,
-- it means some @t@ it was burnt. If it is negative, some @t@ was minted.
utxoStateDiffTotal :: UtxoStateDiff -> Pl.Value
utxoStateDiffTotal = mconcat . map utxoValueSetDiffTotal . M.elems

-- | Computes the difference between two states.
utxoStateDiff :: UtxoState -> UtxoState -> UtxoStateDiff
utxoStateDiff (UtxoState stA) (UtxoState stB) =
  M.union (M.mapWithKey deleteOrModify stA) (M.map Inserted $ M.difference stB stA)
  where
    deleteOrModify :: Pl.Address -> UtxoValueSet -> UtxoValueSetDiff
    deleteOrModify addr valA@(UtxoValueSet a) =
      case M.lookup addr stB of
        Nothing -> Deleted valA
        Just (UtxoValueSet b) ->
          Modified
            { modDeleted = UtxoValueSet $ a L.\\ b,
              modInserted = UtxoValueSet $ b L.\\ a,
              modUnchanged = UtxoValueSet $ a `L.intersect` b
            }

-- | Returns the source of a difference. Satisfies: @utxoStateDiffSrc (utxoStateDiff a b) == a@
utxoStateDiffSrc :: UtxoStateDiff -> UtxoState
utxoStateDiffSrc = UtxoState . M.foldlWithKey keepIfModOrDel M.empty
  where
    keepIfModOrDel :: M.Map Pl.Address UtxoValueSet -> Pl.Address -> UtxoValueSetDiff -> M.Map Pl.Address UtxoValueSet
    keepIfModOrDel m addr (Deleted v) = M.insert addr v m
    keepIfModOrDel m addr (Modified del _ins unch) = M.insert addr (del <> unch) m
    keepIfModOrDel m _ _ = m

-- | Returns the target of a difference. Satisfies: @utxoStateDiffTgt (utxoStateDiff a b) == b@
utxoStateDiffTgt :: UtxoStateDiff -> UtxoState
utxoStateDiffTgt = UtxoState . M.foldlWithKey keepIfModOrIns M.empty
  where
    keepIfModOrIns :: M.Map Pl.Address UtxoValueSet -> Pl.Address -> UtxoValueSetDiff -> M.Map Pl.Address UtxoValueSet
    keepIfModOrIns m addr (Inserted v) = M.insert addr v m
    keepIfModOrIns m addr (Modified _del ins unch) = M.insert addr (ins <> unch) m
    keepIfModOrIns m _ _ = m

-- * Pretty-printing

-- | Pretty prints a 'UtxoState'.
-- The entire point of producing a 'UtxoState' instead of a 'Pl.UtxoIndex' is to
-- provide the user a picture that is closer to their mental model of what is going
-- on. Hence, we must be able to display our 'UtxoState's in a human readable fashion.
prettyUtxoState :: UtxoState -> Doc ann
prettyUtxoState =
  PP.vsep
    . List.intersperse PP.emptyDoc
    . map (uncurry prettyAddress . second utxoValueSet)
    . M.toList
    . utxoState

prettyAddress :: Pl.Address -> [(Pl.Value, Maybe UtxoDatum)] -> Doc ann
prettyAddress address payloads =
  PP.vsep
    [ prettyAddressTypeAndHash address,
      PP.indent 2
        . PP.vsep
        . map (("-" <>) . PP.indent 1)
        . mapMaybe prettyPayloadGroup
        . L.group
        . L.sortBy (compare `on` (Ada.fromValue . fst))
        $ payloads
    ]

prettyPayloadGroup :: [(Pl.Value, Maybe UtxoDatum)] -> Maybe (Doc ann)
prettyPayloadGroup [] = Nothing
prettyPayloadGroup ((vl, mutxo) : rest) =
  let lenRest = length rest
      lenInfo x
        | lenRest == 0 = x
        | otherwise = x <+> PP.parens ("x" <+> PP.pretty (lenRest + 1))
   in lenInfo <$> prettyPayload vl mutxo

-- Returns `Nothing` if the value is empty to avoid having an empty document
-- whose height is 1 in the `prettyprinter` library and would generate empty
-- lines.
prettyPayload :: Pl.Value -> Maybe UtxoDatum -> Maybe (Doc ann)
prettyPayload value mDatum =
  (\vs -> if null vs then Nothing else Just $ PP.vsep vs)
    . catMaybes
    $ [ mPrettyValue value,
        (":" <>) . PP.indent 1 . PP.pretty . utxoShow <$> mDatum
      ]

-- Returns `Nothing` if the value is empty to avoid having an empty document
-- whose height is 1, which would cause `prettyprinter` to generate empty
-- lines.
mPrettyValue :: Pl.Value -> Maybe (Doc ann)
mPrettyValue =
  ( \vs ->
      case vs of
        [] -> Nothing
        [v] -> Just v
        _ -> Just $ PP.lbrace <> PP.indent 1 (PP.vsep vs) <> PP.space <> PP.rbrace
  )
    . map (uncurry prettyCurrencyAndAmount)
    . Pl.toList
    . Pl.getValue

prettyCurrencyAndAmount :: Pl.CurrencySymbol -> Pl.Map Pl.TokenName Integer -> Doc ann
prettyCurrencyAndAmount symbol =
  PP.vsep . map (uncurry prettyToken) . Pl.toList
  where
    prettySymbol :: Pl.CurrencySymbol -> Doc ann
    prettySymbol = PP.pretty . take 7 . show

    prettySpacedNumber :: Integer -> Doc ann
    prettySpacedNumber = psnTerm "" 0
      where
        psnTerm :: Doc ann -> Integer -> Integer -> Doc ann
        psnTerm acc _ 0 = acc
        psnTerm acc 3 nb = psnTerm (PP.pretty (nb `mod` 10) <> "_" <> acc) 1 (nb `div` 10)
        psnTerm acc n nb = psnTerm (PP.pretty (nb `mod` 10) <> acc) (n + 1) (nb `div` 10)

    prettyToken :: Pl.TokenName -> Integer -> Doc ann
    prettyToken name n =
      let prettyAmount = ":" <+> prettySpacedNumber n
          prettyCurrency
            | symbol == Pl.CurrencySymbol "" = "Lovelace"
            | symbol == quickCurrencySymbol = withTok "Quick"
            | symbol == permanentCurrencySymbol = withTok "Perm"
            | otherwise = withTok (prettySymbol symbol)

          withTok :: Doc ann -> Doc ann
          withTok s = PP.parens (s <+> "$" <+> PP.pretty name)
       in prettyCurrency <+> prettyAmount

prettyAddressTypeAndHash :: Pl.Address -> Doc ann
prettyAddressTypeAndHash (Pl.Address addrCr _) =
  case addrCr of
    (Pl.ScriptCredential vh) -> prettyAux "script" vh
    (Pl.PubKeyCredential pkh) ->
      prettyAux "pubkey" pkh
        <> maybe
          PP.emptyDoc
          ((PP.space <>) . PP.parens . ("wallet #" <>) . PP.pretty)
          (walletPKHashToId pkh)
  where
    prettyAux :: Show hash => String -> hash -> Doc ann
    prettyAux addressType hash =
      mconcat
        [ PP.pretty addressType,
          PP.space,
          PP.pretty . take 7 . show $ hash
        ]
        <> PP.colon
