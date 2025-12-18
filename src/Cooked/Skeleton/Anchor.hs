-- | This module exposes the notion of Anchor used in proposals and
-- certificates.
module Cooked.Skeleton.Anchor
  ( -- * Data types
    TxSkelAnchor,

    -- * Optics
    txSkelAnchorMResolvedPageAT,
    txSkelAnchorResolvedPageAT,
    txSkelAnchorURLAT,

    -- * Smart constructors
    simpleURLAnchor,
  )
where

import Data.ByteString
import Optics.Core

-- | A 'TxSkelAnchor' optionally bundles an URL as a String alongside an
-- optional resolved page as a ByteString.
type TxSkelAnchor =
  Maybe
    ( String, -- The anchor URL
      Maybe ByteString -- The optional anchor resolved page
    )

-- | Focusing on the URL of a 'TxSkelAnchor'
txSkelAnchorURLAT :: AffineTraversal' TxSkelAnchor String
txSkelAnchorURLAT = _Just % _1

-- | Focusing on the optional resolved page of a 'TxSkelAnchor'
txSkelAnchorMResolvedPageAT :: AffineTraversal' TxSkelAnchor (Maybe ByteString)
txSkelAnchorMResolvedPageAT = _Just % _2

-- | Focusing on the existing resolved page of a 'TxSkelAnchor'
txSkelAnchorResolvedPageAT :: AffineTraversal' TxSkelAnchor ByteString
txSkelAnchorResolvedPageAT = txSkelAnchorMResolvedPageAT % _Just

-- | Builds a simple Anchor with an URL
simpleURLAnchor :: String -> TxSkelAnchor
simpleURLAnchor = Just . (,Nothing)
