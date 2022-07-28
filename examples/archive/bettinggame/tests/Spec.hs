module Main where

import qualified BettingGameSpec
import Test.Tasty

main :: IO ()
main = do
  defaultMain $
    testGroup
      "main"
      [ BettingGameSpec.tests
      ]
