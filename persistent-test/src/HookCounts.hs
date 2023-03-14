{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HookCounts
  ( hookCountsShouldBe

  , HookCountRefs(..)
  , newHookCountRefs

  , HookCounts(..)

  , trackHookCounts
  ) where

import Control.Monad.IO.Unlift (MonadIO(liftIO))
import Data.Function ((&))
import Database.Persist.SqlBackend.SqlPoolHooks
  ( SqlPoolHooks, modifyAlterBackend, modifyRunAfter, modifyRunBefore, modifyRunOnException
  )
import Init (Expectation, HasCallStack, expectationFailure, guard)
import UnliftIO.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar)
import UnliftIO.Timeout (timeout)

hookCountsShouldBe :: HasCallStack => HookCountRefs -> HookCounts -> Expectation
hookCountsShouldBe hookCountRefs hookCounts =
  checkHookCounts hookCountRefs (== hookCounts)

checkHookCounts
  :: HasCallStack
  => HookCountRefs
  -> (HookCounts -> Bool)
  -> Expectation
checkHookCounts hookCountRefs p = do
  -- The input predicate can cause the STM transaction to retry, so the STM
  -- computation is wrapped in a timeout of 10 seconds in case the STM
  -- transaction never completes.
  mResult <- timeout 10000000 $ atomically $ do
    hookCounts <- hookCountsSTM hookCountRefs
    guard $ p hookCounts
  case mResult of
    Nothing -> expectationFailure "checkHookCounts: took too long"
    Just () -> pure ()

data HookCountRefs = HookCountRefs
  { alterBackendCountRef :: TVar Int
  , runBeforeCountRef :: TVar Int
  , runOnExceptionCountRef :: TVar Int
  , runAfterCountRef :: TVar Int
  }

newHookCountRefs :: IO HookCountRefs
newHookCountRefs =
  HookCountRefs
    <$> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0

hookCountsSTM :: HookCountRefs -> STM HookCounts
hookCountsSTM hookCountRefs =
  HookCounts
    <$> readTVar (alterBackendCountRef hookCountRefs)
    <*> readTVar (runBeforeCountRef hookCountRefs)
    <*> readTVar (runOnExceptionCountRef hookCountRefs)
    <*> readTVar (runAfterCountRef hookCountRefs)

data HookCounts = HookCounts
  { alterBackendCount :: Int
  , runBeforeCount :: Int
  , runOnExceptionCount :: Int
  , runAfterCount :: Int
  } deriving stock (Eq, Show)

trackHookCounts
  :: forall m backend
   . (MonadIO m)
  => HookCountRefs
  -> SqlPoolHooks m backend
  -> SqlPoolHooks m backend
trackHookCounts hookCountRefs sqlPoolHooks =
  sqlPoolHooks
    & flip modifyAlterBackend (\origAlterBackend conn -> do
        bumpCount alterBackendCountRef
        origAlterBackend conn
      )
    & flip modifyRunBefore (\origRunBefore conn level -> do
        bumpCount runBeforeCountRef
        origRunBefore conn level
      )
    & flip modifyRunOnException (\origRunOnException conn level ex -> do
        bumpCount runOnExceptionCountRef
        origRunOnException conn level ex
      )
    & flip modifyRunAfter (\origRunAfter conn level -> do
        bumpCount runAfterCountRef
        origRunAfter conn level
      )
  where
  bumpCount :: TVar Int -> m ()
  bumpCount countRef = do
    liftIO $ atomically $ modifyTVar' countRef (+ 1)

  HookCountRefs
    { alterBackendCountRef
    , runBeforeCountRef
    , runOnExceptionCountRef
    , runAfterCountRef
    } = hookCountRefs
