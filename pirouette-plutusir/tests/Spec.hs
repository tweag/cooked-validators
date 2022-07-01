import qualified Language.Pirouette.PlutusIR.SymEvalSpec as PIRSymEval
import qualified Language.Pirouette.PlutusIR.SyntaxSpec as PIRBuiltins
import qualified Language.Pirouette.PlutusIR.ToTermSpec as FP
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Language"
    [ testGroup
        "PlutusIR"
        [ testGroup "ToTerm" FP.tests,
          testGroup "Builtins" PIRBuiltins.tests,
          testGroup "Symbolic evaluation" PIRSymEval.tests
        ]
    ]
