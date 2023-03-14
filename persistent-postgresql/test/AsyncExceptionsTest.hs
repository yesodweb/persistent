{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module AsyncExceptionsTest
  ( specs
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (MaskingState(MaskedUninterruptible), getMaskingState)
import Data.Function ((&))
import Database.Persist.SqlBackend.SqlPoolHooks
  ( modifyAlterBackend, modifyRunAfter, modifyRunBefore, modifyRunOnException
  )
import GHC.Stack (SrcLoc, callStack, getCallStack)
import Init (Expectation, aroundAll_, guard)
import PgInit
  ( MonadIO(..), PersistQueryWrite(deleteWhere), PersistStoreWrite(insert_)
  , RunConnArgs(sqlPoolHooks), Filter, HasCallStack, LoggingT, ReaderT, Spec, SqlBackend, Text
  , defaultRunConnArgs, describe, expectationFailure, it, mkMigrate, mkPersist, persistLowerCase
  , runConnUsing, runConn_, runMigrationSilent, share, sqlSettings, void
  )
import Test.HUnit.Lang (FailureReason(Reason), HUnitFailure(HUnitFailure))
import UnliftIO.Exception (bracket_, throwTo)
import UnliftIO.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar)
import UnliftIO.Timeout (timeout)

share
  [mkPersist sqlSettings, mkMigrate "asyncExceptionsTestMigrate"]
  [persistLowerCase|
    AsyncExceptionTestData
      stuff Text
      Primary stuff
      deriving Eq Show
  |]

setup :: IO ()
setup = runConn_ $ void $ runMigrationSilent asyncExceptionsTestMigrate

teardown :: IO ()
teardown = runConn_ cleanDB

cleanDB :: forall m. (MonadIO m) => ReaderT SqlBackend m ()
cleanDB = deleteWhere ([] :: [Filter AsyncExceptionTestData])

specs :: Spec
specs = aroundAll_ (bracket_ setup teardown) $ do
  describe "Testing async exceptions" $ do
    it "runOnException hook is executed" $ do
      insertDoneRef <- newEmptyMVar
      shouldProceedRef <- newEmptyMVar

      hookCountRefs <- newHookCountRefs
      runConnArgs <- mkRunConnArgs hookCountRefs

      threadId <- forkIO $ do
        runConnUsing runConnArgs $ do
          insert_ $ AsyncExceptionTestData "bloorp"
          liftIO $ do
            -- "Child" thread signals to the main thread that the insert was
            -- executed.
            putMVar insertDoneRef ()
            -- "Child" thread waits around indefinitely on this @MVar@.
            -- @shouldProceedRef@ is intentionally never written to in this test
            -- so that the "child" thread is blocked here until the main thread
            -- kills it via async exception. See the remaining comments in this
            -- test for more detail.
            takeMVar shouldProceedRef

      -- Main thread waits here for the signal from the "child" thread telling
      -- us the DB insert has been performed. More specifically, we know the
      -- following events have occurred in the "child" thread after this
      -- @takeMVar@ call succeeds:
      --
      -- 1) The @alterBackend@ hook was executed
      -- 2) The @runBefore@ hook was executed
      -- 3) The insert of our test data was executed
      -- 4) Execution is blocked right after the insert, so either of the
      -- @runOnException@ or @runAfter@ hooks have not yet been executed.
      takeMVar insertDoneRef

      -- Verify that the actual hook execution in the "child" thread is as
      -- described previously.
      hookCountRefs `hookCountsShouldBe`
        HookCounts
          { alterBackendCount = 1
          , runBeforeCount = 1
          , runOnExceptionCount = 0
          , runAfterCount = 0
          }

      -- Main thread kills the "child" thread via async exception while the
      -- "child" thread is still in its user-specified DB action, which should
      -- cause the @runOnException@ hook to fire, rolling back the transaction.
      --
      -- Note that the @runOnException@ hook produced by @mkRunConnArgs@ also
      -- ensures the handler's masking state is uninterruptible. See
      -- @mkRunConnArgs@ for that check's implementation.
      killThread threadId

      -- Verify that the @runOnException@ hook was indeed executed.
      hookCountRefs `hookCountsShouldBe`
        HookCounts
          { alterBackendCount = 1
          , runBeforeCount = 1
          , runOnExceptionCount = 1
          , runAfterCount = 0
          }

-- | Build a 'RunConnArgs' value for use in this module's specs.
--
-- This function should only be called from the main thread.
mkRunConnArgs
  :: forall m
   . (MonadIO m)
  => HookCountRefs
  -> m (RunConnArgs m)
mkRunConnArgs hookCountRefs = do
  threadId <- liftIO myThreadId
  pure $ (defaultRunConnArgs @m)
    { sqlPoolHooks =
        sqlPoolHooks defaultRunConnArgs
          & flip modifyAlterBackend (\origAlterBackend conn -> do
              bumpCount alterBackendCountRef
              origAlterBackend conn
            )
          & flip modifyRunBefore (\origRunBefore conn level -> do
              bumpCount runBeforeCountRef
              origRunBefore conn level
            )
          & flip modifyRunOnException (\origRunOnException conn level ex -> do
              -- It's sneaky to make this masking state assertion here rather
              -- than explicitly in a spec. At this time, it feels a bit cleaner
              -- to keep this assertion tucked away in here. The downside is
              -- that this function does not run in the main thread, so we must
              -- throw an expectation failure into the main thread on assertion
              -- failure to have it reported by Hspec.
              liftIO $
                getMaskingState >>= \case
                  MaskedUninterruptible -> pure ()
                  _ ->
                    throwExpectationFailureTo
                      threadId
                      "Expected runOnException masking to be uninterruptible"

              bumpCount runOnExceptionCountRef
              origRunOnException conn level ex
            )
          & flip modifyRunAfter (\origRunAfter conn level -> do
              bumpCount runAfterCountRef
              origRunAfter conn level
            )
    }
  where
  bumpCount :: TVar Int -> LoggingT m ()
  bumpCount countRef = do
    liftIO $ atomically $ modifyTVar' countRef (+ 1)

  HookCountRefs
    { alterBackendCountRef
    , runBeforeCountRef
    , runOnExceptionCountRef
    , runAfterCountRef
    } = hookCountRefs

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

throwExpectationFailureTo
  :: HasCallStack
  => ThreadId
  -> String
  -> IO ()
throwExpectationFailureTo threadId msg =
  throwTo threadId $ HUnitFailure location $ Reason msg

location :: HasCallStack => Maybe SrcLoc
location = case reverse $ getCallStack callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing
