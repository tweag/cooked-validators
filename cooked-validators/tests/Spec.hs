import qualified Cooked.BalanceSpec as Ba
import qualified Cooked.MockChain.Monad.StagedSpec as StagedSpec
import qualified Cooked.MockChain.UtxoStateSpec as UtxoStateSpec
import qualified Cooked.MockChain.WalletSpec as WalletSpec
import qualified Cooked.OutputReorderingSpec as OutputReorderingSpec
import qualified Cooked.QuickValueSpec as QuickValueSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Reordering outputs" OutputReorderingSpec.spec
  describe "Balancing transactions" Ba.spec
  describe "Quick values" QuickValueSpec.spec
  describe "Staged monad" StagedSpec.spec
  describe "UtxoState" UtxoStateSpec.spec
  describe "Wallet" WalletSpec.spec
