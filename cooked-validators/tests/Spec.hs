import qualified Cooked.InlineDatumsSpec as InlineDatums
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "cooked-validators"
    [ testGroup "inline datums" InlineDatums.tests
    ]
