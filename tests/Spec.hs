import Cooked.AttackSpec qualified as AttackSpec
import Cooked.BalancingSpec qualified as BalancingSpec
import Cooked.BasicUsageSpec qualified as BasicUsageSpec
import Cooked.InitialDistributionSpec qualified as InitDistribSpec
import Cooked.InlineDatumsSpec qualified as InlineDatumsSpec
import Cooked.LtlSpec qualified as LtlSpec
import Cooked.MinAdaSpec qualified as MinAdaSpec
import Cooked.MockChainSpec qualified as MockChainSpec
import Cooked.ProposingScriptSpec qualified as ProposingSpec
import Cooked.ReferenceInputsSpec qualified as ReferenceInputsSpec
import Cooked.ReferenceScriptsSpec qualified as ReferenceScriptsSpec
import Cooked.TweakSpec qualified as TweakSpec
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "cooked-validators"
      [ BasicUsageSpec.tests,
        MinAdaSpec.tests,
        BalancingSpec.tests,
        InlineDatumsSpec.tests,
        ReferenceInputsSpec.tests,
        ReferenceScriptsSpec.tests,
        AttackSpec.tests,
        TweakSpec.tests,
        LtlSpec.tests,
        MockChainSpec.tests,
        InitDistribSpec.tests,
        ProposingSpec.tests
      ]
