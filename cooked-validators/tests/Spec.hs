import Test.Hspec

import qualified Cooked.BalanceSpec as Ba

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Balancing transactions" Ba.spec
