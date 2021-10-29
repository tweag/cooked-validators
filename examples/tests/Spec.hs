import qualified SplitSpec
import qualified PMultiSigSpec
import qualified ForgeSpec
import qualified UseCasesAuctionSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "'Split' contract" SplitSpec.spec
  describe "'PMultiSig' contract" PMultiSigSpec.spec
  describe "'Forge' contract" ForgeSpec.spec
  describe "'plutus-use-cases/Auction' contract" UseCasesAuctionSpec.spec
