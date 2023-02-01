module Main where

import qualified AuctionSpec
import Test.Tasty

main :: IO ()
main = do
  defaultMain $
    testGroup
      "main"
      [ AuctionSpec.tests
      ]
