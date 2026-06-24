-- | Transforming 'TxSkelAnchor' into its Cardano counterpart
module Cooked.MockChain.GenerateTx.Anchor (toCardanoAnchor) where

import Cardano.Ledger.BaseTypes qualified as C.Ledger
import Cardano.Ledger.Conway.Core qualified as Conway
import Control.Monad.Catch
import Cooked.Skeleton.Anchor
import Data.Default
import Data.Functor
import Data.Maybe
import Data.Text qualified as Text
import GHC.IO.Unsafe
import Network.HTTP.Simple qualified as Network

-- | This function transforms a 'TxSkelAnchor' into its Cardano counterpart. If
-- the provided anchor does not provde a resolved page, it will be unsafely
-- fetched online, so use at your own discretion.
toCardanoAnchor :: TxSkelAnchor -> C.Ledger.Anchor
toCardanoAnchor txSkelAnchor =
  fromMaybe def $
    do
      (url, page) <- txSkelAnchor
      anchorUrl <- C.Ledger.textToUrl (length url) (Text.pack url)
      fmap (C.Ledger.Anchor anchorUrl . Conway.hashAnnotated . C.Ledger.AnchorData) $ case page of
        Just resolvedPage -> return resolvedPage
        Nothing ->
          -- WARNING: very unsafe and unreproducible
          unsafePerformIO
            ( handle
                (return . fail . (("Error when parsing anchor " ++ show url ++ " with error: ") ++) . (show @Network.HttpException))
                ((Network.parseRequest url >>= Network.httpBS) <&> return . Network.getResponseBody)
            )
