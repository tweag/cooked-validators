import qualified Cooked.BalanceSpec as Ba
import qualified Cooked.QuickValueSpec as QuickValueSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Balancing transactions" Ba.spec
  describe "Quick values" QuickValueSpec.spec
