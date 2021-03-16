{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveAnyClass             #-}

module QueryInProgressTest where

import qualified Control.Concurrent            as Concurrent
import qualified Control.Concurrent.Async      as Concurrent
import qualified Control.Exception             as Exception
import qualified Control.Monad                 as Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Logger          as Logger
import qualified Data.ByteString               as BS
import           Data.Either
import qualified Data.Pool                     as Pool
import           Data.Time
import qualified Database.Persist              as Persist
import qualified Database.Persist.Postgresql   as Persist
import qualified Database.Persist.Sql          as Persist
import           PgInit
-- import Database.PostgreSQL.LibPQ (transactionStatus)
import Data.Maybe

createTableFoo :: Pool.Pool Persist.SqlBackend -> IO ()
createTableFoo pool = flip Persist.runSqlPersistMPool pool
    $ Persist.rawExecute "CREATE table if not exists foobar(id int);" []

testInterruptedConnection :: IO (Either SomeException [Maybe (Single String)])
testInterruptedConnection = do
    pool <- Logger.runNoLoggingT $ Persist.createPostgresqlPool
        "postgresql://postgres:secret@localhost/postgres"
        1

    Monad.void $ createTableFoo pool

    simulateFailedLongRunningPostgresCall pool

    result :: Either SomeException [Maybe (Single String)] <-
        Exception.try . flip Persist.runSqlPool pool $ do

            Persist.rawSql @(Maybe (Persist.Single String))
                "select pg_sleep(10)"
                []

    pure result


specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith _ =
    describe "QueryInProgress" $ fit "queries are cleaned up correctly" $ do

  result <- testInterruptedConnection
  result `shouldNotSatisfy` isLeft

  -- results :: [Either SomeException [Maybe (Single String)]] <- Concurrent.replicateConcurrently 5 testInterruptedConnection
  -- results `shouldSatisfy` all (== [Nothing]) . rights




simulateFailedLongRunningPostgresCall :: Pool.Pool Persist.SqlBackend -> IO ()
simulateFailedLongRunningPostgresCall pool = do
    threadId <- Concurrent.forkIO
        (do

            putStrLn
                "verify pool can't be borrowed and we've set things up correctly"

            let numThings :: Int = 100000000
            putStrLn $ "start inserting " <> show numThings <> " things"

            Monad.forM_ [1 .. numThings] $ \i -> do
                Logger.runStdoutLoggingT
                    $ flip Persist.runSqlPool pool
                    $
                      Persist.rawExecute "insert into foobar values(?);"
                                         [toPersistValue i]
        )

    putStrLn "waiting 5 seconds"
    Concurrent.threadDelay 5000000
    Monad.void $ Concurrent.killThread threadId
    putStrLn "killed thread"
