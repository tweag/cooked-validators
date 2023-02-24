import qualified Cooked.AttackSpec as AttackSpec
import qualified Cooked.InlineDatumsSpec as InlineDatumsSpec
-- import qualified Cooked.BalanceSpec as Ba
import qualified Cooked.LtlSpec as LtlSpec
import qualified Cooked.MinAdaSpec as MinAdaSpec
import qualified Cooked.MockChainSpec as MockChainSpec
import qualified Cooked.ReferenceInputsSpec as ReferenceInputsSpec
import qualified Cooked.ReferenceScriptsSpec as ReferenceScriptsSpec
import qualified Cooked.TweakSpec as TweakSpec
-- import qualified Cooked.MockChain.Monad.StagedSpec as StagedSpec
-- import qualified Cooked.MockChain.UtxoStateSpec as UtxoStateSpec
-- import qualified Cooked.WalletSpec as WalletSpec
-- import qualified Cooked.OutputReorderingSpec as OutputReorderingSpec
-- import qualified Cooked.QuickValueSpec as QuickValueSpec
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "cooked-validators"
    [ -- testGroup "Reordering outputs" OutputReorderingSpec.tests,
      -- testGroup "Balancing transactions" Ba.tests,
      -- testGroup "Quick values" QuickValueSpec.tests,
      -- testGroup "Staged monad" StagedSpec.tests,
      -- testGroup "UtxoState" UtxoStateSpec.tests,
      -- testGroup "Wallet" WalletSpec.tests,
      MinAdaSpec.tests,
      InlineDatumsSpec.tests,
      ReferenceInputsSpec.tests,
      ReferenceScriptsSpec.tests,
      AttackSpec.tests,
      TweakSpec.tests,
      LtlSpec.tests,
      MockChainSpec.tests
    ]
