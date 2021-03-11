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

import PgInit
import qualified Control.Monad as Monad
import Control.Monad.IO.Class
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Database.Persist as Persist
import qualified Database.Persist.Sql as Persist
import qualified Database.Persist.Postgresql as Persist
import qualified Control.Monad.Logger as Logger
import qualified Data.ByteString as BS
import qualified Data.Pool as Pool
import Data.Time
-- import Database.PostgreSQL.LibPQ (transactionStatus)

share [mkPersist sqlSettings, mkMigrate "migration"] [persistUpperCase|
  Wombat
     name        Text sqltype=varchar(80)

     Primary name
     deriving Eq Show Ord

|]

createTableFoo :: Pool.Pool Persist.SqlBackend -> IO ()
createTableFoo pool = (flip Persist.runSqlPersistMPool) pool $ do
  Persist.rawExecute "CREATE table if not exists foobar(id int);" []


specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "QueryInProgress" $ do
    fit ("queries are cleaned up correctly") $ do

      pool <- Logger.runNoLoggingT $ Persist.createPostgresqlPool "postgresql://postgres:secret@localhost/postgres" 1

      Monad.void $ createTableFoo pool

      getCurrentTime >>= \now ->
        simulateFailedLongRunningPostgresCall pool 

      putStrLn "normal query"
      putStrLn "first query"
      void . ((flip Persist.runSqlPersistMPool) pool) $ do
        Persist.rawSql @(Maybe (Persist.Single String)) "select pg_sleep(5)" []
      putStrLn "normal query should have close/masking state message above"

      result :: Either Exception.SomeException [Persist.Single String] <-
        Exception.try . ((flip Persist.runSqlPersistMPool) pool) $ do
        Persist.rawSql @(Persist.Single String) "select pg_sleep(5)" []

      let exceptionStr :: String = either Exception.displayException (const "no exception") result
      liftIO $ exceptionStr @/= "libpq: failed (another command is already in progress\n)"
      -- void $ rawSql @(Maybe (Single String)) "select pg_sleep(1)" []
      


simulateFailedLongRunningPostgresCall :: Pool.Pool Persist.SqlBackend -> IO ()
simulateFailedLongRunningPostgresCall pool = do
  threadId <- Concurrent.forkIO
    $ (do
        let numThings :: Int = 100000000
        putStrLn $ "start inserting " <> show numThings <> " things"
        Monad.forM_ [1 .. numThings] $ \_ -> do
          (flip Persist.runSqlPersistMPool) pool $
            Persist.rawExecute "insert into foobar values(1);" []
      )
  Concurrent.threadDelay 5000000
  Monad.void $ Concurrent.killThread threadId
  putStrLn "killed thread"
