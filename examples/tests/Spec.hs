import qualified SplitSpec
import qualified PMultiSigSpec
import qualified ForgeSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "'Split' contract" SplitSpec.spec
  describe "'PMultiSig' contract" PMultiSigSpec.spec
  describe "'Forge' contract" ForgeSpec.spec
