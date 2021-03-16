{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import  qualified Control.Monad as Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Database.Persist as Persist
import qualified Database.Persist.Sql as Persist
import qualified Database.Persist.Postgresql as Persist
import qualified Control.Monad.Logger as Logger
import qualified Data.ByteString as BS
import qualified Data.Pool as Pool
import Data.Time

main :: IO ()
main = do

  -- I started a postgres server with:
  -- docker run --rm --name some-postgres -p 5432:5432 -e POSTGRES_PASSWORD=secret postgres
  pool <- Logger.runNoLoggingT $ Persist.createPostgresqlPool "postgresql://postgres:secret@localhost:5433/postgres" 1

  Monad.void $ createTableFoo pool

  getCurrentTime >>= \now ->
    simulateFailedLongRunningPostgresCall pool

  Pool.destroyAllResources pool

  result :: Either Exception.SomeException [Persist.Single String] <-
    Exception.try . ((flip Persist.runSqlPersistMPool) pool) $ do
        Persist.rawSql @(Persist.Single String) "select pg_sleep(5)" []

  -- when we try the above we get back:
  -- 'result: Left libpq: failed (another command is already in progress'
  -- this is because the connection went back into the pool before it was ready
  -- or perhaps it should have been destroyed and a new connection created and put into the pool?
  putStrLn $ "result: " <> show result

createTableFoo :: Pool.Pool Persist.SqlBackend -> IO ()
createTableFoo pool = (flip Persist.runSqlPersistMPool) pool $ do
  Persist.rawExecute "CREATE table if not exists foo(id int);" []

simulateFailedLongRunningPostgresCall :: Pool.Pool Persist.SqlBackend -> IO ()
simulateFailedLongRunningPostgresCall pool = do
  threadId <- Concurrent.forkIO
    $ (do
        let numThings :: Int = 100000000
        putStrLn $ "start inserting " <> show numThings <> " things"
        Monad.forM_ [1 .. numThings] $ \_ -> do
          (flip Persist.runSqlPersistMPool) pool $
            Persist.rawExecute "insert into foo values(1);" []
      )
  Concurrent.threadDelay 5000000
  Monad.void $ Concurrent.killThread threadId
  putStrLn "killed thread"
