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

share [mkPersist sqlSettings, mkMigrate "migration"] [persistUpperCase|
  Wombat
     name        Text sqltype=varchar(80)

     Primary name
     deriving Eq Show Ord

|]

createTableFoo :: Pool.Pool Persist.SqlBackend -> IO ()
createTableFoo pool = flip Persist.runSqlPersistMPool pool
    $ Persist.rawExecute "CREATE table if not exists foobar(id int);" []

testInterruptedConnection = do
    pool <- Logger.runNoLoggingT $ Persist.createPostgresqlPool
        "postgresql://postgres:secret@localhost/postgres"
        1

    Monad.void $ createTableFoo pool

    simulateFailedLongRunningPostgresCall pool

    result :: Either SomeException [Maybe (Single String)] <-
        Exception.try . flip Persist.runSqlPool pool $ do

            Persist.rawSql @(Maybe (Persist.Single String))
                "select pg_sleep(5)"
                []

    pure result


specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith _ =
    describe "QueryInProgress" $ fit "queries are cleaned up correctly" $ do
        results <- Concurrent.replicateConcurrently
            10
            testInterruptedConnection
        results `shouldNotSatisfy` any isLeft





simulateFailedLongRunningPostgresCall :: Pool.Pool Persist.SqlBackend -> IO ()
simulateFailedLongRunningPostgresCall pool = do
    threadId <- Concurrent.forkIO
        (do

            putStrLn
                "verify pool can't be borrowed and we've set things up correctly"

            -- maybe
            --   (putStrLn "pool exhausted")
            --   (const $ putStrLn "pool could be borrowed")
            --   =<< Pool.tryTakeResource pool


            let numThings :: Int = 100000000
            putStrLn $ "start inserting " <> show numThings <> " things"

            Monad.forM_ [1 .. numThings] $ \i -> do
                Logger.runStdoutLoggingT
                    $ flip Persist.runSqlPool pool
                    $

                  -- this is basically calling Acquire.with in a loop and Resource.mkAcquireType
                  -- if Pool's withResource is interruptible this calls putResource numThings times
                      Persist.rawExecute "insert into foobar values(?);"
                                         [toPersistValue i]
        )

    -- Concurrent.threadDelay 5000000
    Monad.void $ Concurrent.killThread threadId
    putStrLn "killed thread"
