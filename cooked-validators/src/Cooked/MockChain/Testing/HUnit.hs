{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cooked.MockChain.HUnit where

import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Monad.Staged
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Test.HUnit.Base

-- | Asserts that the results produced by running the given 'StagedMockChain' from
-- some speficied 'InitialDistribution' satisfy a given assertion. This function
-- is mainly used as a building block for simpler predicates.
assertSatisfiesFrom' ::
  ([(Either MockChainError (a, UtxoState), TraceDescr)] -> res) ->
  InitialDistribution ->
  StagedMockChain a ->
  res
assertSatisfiesFrom' predi i0 = predi . interpretAndRunWith (runMockChainTFrom i0)

class XXX res where
  reportFailure :: String -> res

assertTwoRelatedBy ::
  (XXX res) =>
  (UtxoState -> UtxoState -> res) ->
  InitialDistribution ->
  StagedMockChain a ->
  res
assertTwoRelatedBy rel = assertSatisfiesFrom' $ \case
  [(ra, ta), (rb, tb)] -> case (ra, rb) of
    (Right resA, Right resB) -> rel (snd resA) (snd resB)
    (Left errA, Right _) ->
      reportFailure $ concat ["Expected two outcomes, the first failed with:", show errA, "\n", show ta]
    (Right _, Left errB) ->
      reportFailure $ concat ["Expected two outcomes, the second failed with:", show errB, "\n", show tb]
    (Left errA, Left errB) ->
      reportFailure $
        concat
          [ "Expected two outcomes, the both with:",
            show errA,
            "; ",
            show errB,
            "\n First: ",
            show ta,
            "\nSecond: ",
            show tb
          ]
  xs -> reportFailure $ "Expected exactly two outcomes, received: " ++ show (length xs)

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
