import qualified Cooked.BalanceSpec as Ba
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Balancing transactions" Ba.spec
