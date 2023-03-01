-- import qualified Cooked.BalanceSpec as Ba

import qualified Cooked.Behaviour.Elementary as Elementary
import qualified Cooked.Behaviour.InlineDatumsSpec as InlineDatumsSpec
import qualified Cooked.Behaviour.ReferenceInputsSpec as ReferenceInputsSpec
import qualified Cooked.Behaviour.ReferenceScriptsSpec as ReferenceScriptsSpec
import qualified Cooked.Unit.AttackSpec as AttackSpec
import qualified Cooked.Unit.LtlSpec as LtlSpec
import qualified Cooked.Unit.MinAdaSpec as MinAdaSpec
import qualified Cooked.Unit.MockChainSpec as MockChainSpec
import qualified Cooked.Unit.TweakSpec as TweakSpec
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
      testGroup
        "Unit tests"
        [ MockChainSpec.tests,
          MinAdaSpec.tests,
          AttackSpec.tests,
          TweakSpec.tests,
          LtlSpec.tests
        ],
      testGroup
        "Behaviour tests"
        [ InlineDatumsSpec.tests,
          ReferenceInputsSpec.tests,
          ReferenceScriptsSpec.tests,
          Elementary.tests
        ]
    ]
