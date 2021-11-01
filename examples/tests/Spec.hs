import qualified ForgeSpec
import qualified MarketMakerSpec
import qualified PMultiSigSpec
import qualified SplitSpec
import Test.Hspec
import qualified UseCaseMultisigSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "'Split' contract" SplitSpec.spec
  describe "'PMultiSig' contract" PMultiSigSpec.spec
  describe "'Forge' contract" ForgeSpec.spec
  describe "'UseCaseMultisig' contract" UseCaseMultisigSpec.spec
  describe "'MarketMaker' contract" MarketMakerSpec.spec
