import Cooked.AttackSpec qualified as AttackSpec
import Cooked.BalancingSpec qualified as BalancingSpec
import Cooked.BasicUsageSpec qualified as BasicUsageSpec
import Cooked.InitialDistributionSpec qualified as InititalDistributionSpec
import Cooked.InlineDatumsSpec qualified as InlineDatumsSpec
import Cooked.LtlSpec qualified as LtlSpec
import Cooked.MinAdaSpec qualified as MinAdaSpec
import Cooked.MockChainSpec qualified as MockChainSpec
import Cooked.MultiPurposeSpec qualified as MultiPurposeSpec
import Cooked.ProposingScriptSpec qualified as ProposingScriptSpec
import Cooked.ReferenceInputsSpec qualified as ReferenceInputsSpec
import Cooked.ReferenceScriptsSpec qualified as ReferenceScriptsSpec
import Cooked.TweakSpec qualified as TweakSpec
import Cooked.WithdrawalsSpec qualified as WithdrawalsSpec
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "cooked-validators"
      [ AttackSpec.tests,
        BalancingSpec.tests,
        BasicUsageSpec.tests,
        InititalDistributionSpec.tests,
        InlineDatumsSpec.tests,
        LtlSpec.tests,
        MinAdaSpec.tests,
        MockChainSpec.tests,
        MultiPurposeSpec.tests,
        ProposingScriptSpec.tests,
        ReferenceInputsSpec.tests,
        ReferenceScriptsSpec.tests,
        TweakSpec.tests,
        WithdrawalsSpec.tests
      ]
