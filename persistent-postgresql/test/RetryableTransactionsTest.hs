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
module RetryableTransactionsTest
  ( specs
  ) where

import Control.Concurrent (threadDelay)
import Data.Foldable (find)
import Database.Persist.Postgresql (isSerializationFailure)
import Init (IsolationLevel(Serializable), aroundAll_, guard)
import PgInit
  ( MonadIO(..), PersistQueryWrite(deleteWhere), RunConnArgs(level, shouldRetry), Single(unSingle)
  , (+=.), (-=.), Filter, ReaderT, Spec, SqlBackend, Text, defaultRunConnArgs, describe
  , expectationFailure, get, insert, it, mkMigrate, mkPersist, persistLowerCase, rawSql
  , runConnUsing, runConn_, runMigrationSilent, share, shouldReturn, sqlSettings, update, void
  )
import UnliftIO.Async (Concurrently(Concurrently, runConcurrently))
import UnliftIO.Exception (bracket_)
import UnliftIO.STM (atomically, newTVarIO, readTVar, writeTVar)
import UnliftIO.Timeout (timeout)
import qualified Data.Text as Text

share
  [mkPersist sqlSettings, mkMigrate "retryableTransactionsTestMigrate"]
  [persistLowerCase|
    RetryableTransactionsTestData
      stuff Text
      things Int
      Primary stuff
      deriving Eq Show
  |]

setup :: IO ()
setup = runConn_ $ void $ runMigrationSilent retryableTransactionsTestMigrate

teardown :: IO ()
teardown = runConn_ cleanDB

cleanDB :: forall m. (MonadIO m) => ReaderT SqlBackend m ()
cleanDB = deleteWhere ([] :: [Filter RetryableTransactionsTestData])

specs :: Spec
specs = aroundAll_ (bracket_ setup teardown) $ do
  describe "Testing retryable transactions" $ do
    it "serializable isolation" $ do
      let runConnArgs =
            defaultRunConnArgs
              { level = Just Serializable
              , shouldRetry = isSerializationFailure
              }

      child1WithinTxRef <- newTVarIO False
      child1ShouldUpdateRef <- newTVarIO False
      child1UpdateDoneRef <- newTVarIO False
      child1ShouldCommitRef <- newTVarIO False

      child2WithinTxRef <- newTVarIO False
      child2ShouldUpdateRef <- newTVarIO False

      -- From the main thread, insert a row for subsequent use from the spawned
      -- threads.
      key <- runConnUsing runConnArgs $ do
        insert $ RetryableTransactionsTestData "bloorp" 42

      -- This test launches and waits for three threads. The first two threads
      -- perform an update on the same row. The third thread coordinates when
      -- the first two threads can proceed with their steps. This test will
      -- reproduce a database-level serialization error from thread 2, and so
      -- thread 2's transaction should be retried. While it isn't much code, the
      -- sequence is nuanced. The exact sequence is as follows:
      --
      -- 1) Threads 1 and 2 each start up a serializable transaction and
      -- indicate to thread 3 that they have done so.
      -- 2) Threads 1 and 2 await a go-ahead from thread 3 for them to proceed.
      -- 3) When thread 3 receives the signals indicating threads 1 and 2 are
      -- currently within transactions, it signals to thread 1 that it may
      -- proceed with its update.
      -- 4) Thread 1 performs its update and indicates to thread 3 that it has
      -- done so.
      -- 5) Thread 1 awaits a go-ahead from thread 3 for it to commit its
      -- transaction.
      -- 6) When thread 3 receives the signal indicating thread 1 has performed
      -- its update, it signals to thread 2 that it may proceed with its update.
      -- 7) Thread 2 attempts to perform its update. This update is blocking due
      -- to serializable isolation, i.e. thread 1 has already performed an
      -- update on the row and now thread 2 is attempting to update the same
      -- row.
      -- 8) Thread 3 polls the database for a signal indicating that thread 2's
      -- update statement is blocked.
      -- 9) When thread 3 receives the signal indicating thread 2's update
      -- statement is indeed blocked, it signals to thread 1 that it may commit
      -- its transaction.
      -- 10) Thread 1 commmits its transaction. The database then reports a
      -- serialization error in thread 2's open transaction. With support in
      -- persistent for retryable transactions, this error can be detected and
      -- thread 2's transaction can be retried immediately. The subsequent retry
      -- of the transaction will complete successfully, as there will be no
      -- other concurrent transactions trying to update the same row this time
      -- around.
      mTimeoutRes <- timeout 10000000 $ runConcurrently $
        (\() () () -> ())
          <$> Concurrently
                ( runConnUsing runConnArgs $ do
                    liftIO $ atomically $ writeTVar child1WithinTxRef True
                    liftIO $ atomically $ guard =<< readTVar child1ShouldUpdateRef
                    update key [RetryableTransactionsTestDataThings -=. 1]
                    liftIO $ atomically $ writeTVar child1UpdateDoneRef True
                    liftIO $ atomically $ guard =<< readTVar child1ShouldCommitRef
                )
          <*> Concurrently
                ( runConnUsing runConnArgs $ do
                    liftIO $ atomically $ writeTVar child2WithinTxRef True
                    liftIO $ atomically $ guard =<< readTVar child2ShouldUpdateRef
                    update key [RetryableTransactionsTestDataThings +=. 1]
                )
          <*> Concurrently
                ( do
                    atomically $ do
                      child1WithinTx <- readTVar child1WithinTxRef
                      child2WithinTx <- readTVar child2WithinTxRef
                      guard $ child1WithinTx && child2WithinTx
                      writeTVar child1ShouldUpdateRef True

                    atomically $ do
                      guard =<< readTVar child1UpdateDoneRef
                      writeTVar child2ShouldUpdateRef True

                    pollForBlockedQuery runConnArgs $ Text.unwords
                      [ "UPDATE \"retryable_transactions_test_data\""
                      , "SET \"things\"=\"things\"+1"
                      , "WHERE \"stuff\"='bloorp' "
                      ]

                    atomically $ do
                      writeTVar child1ShouldCommitRef True
                )

      case mTimeoutRes of
        Nothing ->
          expectationFailure "Serializable isolation test threads took too long"
        Just () -> pure ()

      runConnUsing runConnArgs (get key)
        `shouldReturn` Just (RetryableTransactionsTestData "bloorp" 42)

pollForBlockedQuery :: RunConnArgs IO -> Text -> IO ()
pollForBlockedQuery runConnArgs targetBlockedQuery = do
  timeout 10000000 go >>= \case
    Nothing -> expectationFailure "pollForBlockedQuery: took too long"
    Just () -> pure ()
  where
  go = do
    blockedQueries :: [Text] <-
      fmap (fmap unSingle) $ runConnUsing runConnArgs $ do
        rawSql query []
    case find (== targetBlockedQuery) blockedQueries of
      Nothing ->
        threadDelay 200000 *> pollForBlockedQuery runConnArgs targetBlockedQuery
      Just {} -> pure ()

  query =
    Text.unwords
      [ "select query"
      , "from pg_stat_activity"
      , "where cardinality(pg_blocking_pids(pid)) > 0"
      ]
