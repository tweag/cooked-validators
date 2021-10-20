import qualified SplitSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "'Split' contract" SplitSpec.spec
