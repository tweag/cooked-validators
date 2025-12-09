import Spec.Attack qualified as Attack
import Spec.Balancing qualified as Balancing
import Spec.BasicUsage qualified as BasicUsage
import Spec.Certificates qualified as Certificates
import Spec.InitialDistribution qualified as InititalDistribution
import Spec.InlineDatums qualified as InlineDatums
import Spec.Ltl qualified as Ltl
import Spec.Ltl.Combinators qualified as LtlComb
import Spec.MinAda qualified as MinAda
import Spec.MultiPurpose qualified as MultiPurpose
import Spec.ProposingScript qualified as ProposingScript
import Spec.ReferenceInputs qualified as ReferenceInputs
import Spec.ReferenceScripts qualified as ReferenceScripts
import Spec.Slot qualified as Slot
import Spec.Tweak qualified as Tweak
import Spec.Withdrawals qualified as Withdrawals
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "cooked-validators"
      [ Attack.tests,
        Balancing.tests,
        BasicUsage.tests,
        Certificates.tests,
        InititalDistribution.tests,
        InlineDatums.tests,
        Ltl.tests,
        LtlComb.tests,
        MinAda.tests,
        MultiPurpose.tests,
        -- ProposingScript.tests,
        ReferenceInputs.tests,
        ReferenceScripts.tests,
        Slot.tests,
        Tweak.tests,
        Withdrawals.tests
      ]
