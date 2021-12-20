module Cooked.MockChain.HUnit where

import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Monad.Staged
import Cooked.MockChain.UtxoState
import Test.HUnit.Base

-- | Produces a HUnit 'Assertion' that all universes of the non-deterministic
--  trace @tr@ succeed, i.e., satisfy @isRight@.
assertSucceeds :: StagedMockChain a -> Assertion
assertSucceeds tr =
  mapM_ (uncurry succeeds1) $ interpretAndRun tr
  where
    succeeds1 :: Either MockChainError (a, UtxoState) -> TraceDescr -> IO ()
    succeeds1 (Right _) _ = return ()
    succeeds1 (Left err) descr = mapM_ (putStrLn . ("    " ++)) ("" : lines (show descr)) >> assertFailure msg
      where
        msg = "Expected success, received:\n " ++ show err

-- | Produces a HUnit 'Assertion' asserting that all universes of the non-deterministic
--  trace @tr@ fail, i.e., satisfy @isLeft@.
assertFails :: StagedMockChain a -> Assertion
assertFails tr =
  mapM_ (uncurry succeeds1) $ interpretAndRun tr
  where
    succeeds1 :: Either MockChainError (a, UtxoState) -> TraceDescr -> IO ()
    succeeds1 (Left _) _ = return ()
    succeeds1 (Right _) descr = mapM_ (putStrLn . ("    " ++)) ("" : lines (show descr)) >> assertFailure msg
      where
        msg = "Expected failure, but trace succeeded."
