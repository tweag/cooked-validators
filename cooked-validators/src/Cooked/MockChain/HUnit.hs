module Cooked.MockChain.HUnit where

import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Monad.Staged
import Cooked.MockChain.UtxoState
import Test.HUnit.Base

assertSucceeds :: StagedMockChain a -> Assertion
assertSucceeds tr =
  mapM_ (uncurry succeeds1) $ interpretAndRun tr
  where
    succeeds1 :: Either MockChainError (a, UtxoState) -> TraceDescr -> IO ()
    succeeds1 (Right _) _ = return ()
    succeeds1 (Left err) descr = assertFailure msg
      where
        msg = show descr ++ "\nExpected success, received:\n " ++ show err
