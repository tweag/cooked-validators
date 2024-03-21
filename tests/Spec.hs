import qualified Cooked.AttackSpec as AttackSpec
import qualified Cooked.InitialDistributionSpec as InitDistrib
import qualified Cooked.InlineDatumsSpec as InlineDatumsSpec
import qualified Cooked.LtlSpec as LtlSpec
import qualified Cooked.MinAdaSpec as MinAdaSpec
import qualified Cooked.MockChainSpec as MockChainSpec
import qualified Cooked.ReferenceInputsSpec as ReferenceInputsSpec
import qualified Cooked.ReferenceScriptsSpec as ReferenceScriptsSpec
import qualified Cooked.ShowBSSpec as ShowBSSpec
import qualified Cooked.TweakSpec as TweakSpec
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "cooked-validators"
      [ MinAdaSpec.tests,
        InlineDatumsSpec.tests,
        ReferenceInputsSpec.tests,
        ReferenceScriptsSpec.tests,
        AttackSpec.tests,
        TweakSpec.tests,
        LtlSpec.tests,
        MockChainSpec.tests,
        ShowBSSpec.tests,
        InitDistrib.tests
      ]
