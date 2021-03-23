{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings, QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# language OverloadedStrings #-}

-- | This executable is a test of the issue raised in #1199.
module Main where

import Prelude hiding (show)
import qualified Prelude

import qualified Data.Text as Text
import  Control.Monad.IO.Class
import  qualified Control.Monad as Monad
import qualified UnliftIO.Concurrent as Concurrent
import qualified UnliftIO.Exception as Exception
import qualified Database.Persist as Persist
import qualified Database.Persist.Sql as Persist
import qualified Database.Persist.Postgresql as Persist
import qualified Control.Monad.Logger as Logger
import Control.Monad.Logger
import qualified Data.ByteString as BS
import qualified Data.Pool as Pool
import Data.Time
import UnliftIO
import Data.Coerce
import Control.Monad.Trans.Reader
import Control.Monad.Trans

newtype LogPrefixT m a = LogPrefixT { runLogPrefixT :: ReaderT LogStr m a }
    deriving newtype
        (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadLogger m => MonadLogger (LogPrefixT m) where
    monadLoggerLog loc src lvl msg = LogPrefixT $ ReaderT $ \prefix ->
        monadLoggerLog loc src lvl (toLogStr prefix <> toLogStr msg)

deriving newtype instance (forall a b. Coercible a b => Coercible (m a) (m b), MonadUnliftIO m) => MonadUnliftIO (LogPrefixT m)

prefixLogs :: Text.Text -> LogPrefixT m a -> m a
prefixLogs prefix =
    flip runReaderT (toLogStr $! mconcat ["[", prefix, "] "]) . runLogPrefixT

infixr 5 `prefixLogs`
show :: Show a => a -> Text.Text
show = Text.pack . Prelude.show

main :: IO ()
main = runStdoutLoggingT $ Concurrent.myThreadId >>= \tid -> prefixLogs (show tid) $ do

  -- I started a postgres server with:
  -- docker run --rm --name some-postgres -p 5432:5432 -e POSTGRES_PASSWORD=secret postgres
  pool <- Logger.runNoLoggingT $ Persist.createPostgresqlPool "postgresql://postgres:secret@localhost:5433/postgres" 1

  logInfoN "creating table..."
  Monad.void $ liftIO $ createTableFoo pool

  liftIO getCurrentTime >>= \now ->
    simulateFailedLongRunningPostgresCall pool

  -- logInfoN "destroying resources"
  -- liftIO $ Pool.destroyAllResources pool

  logInfoN "pg_sleep"
  result :: Either Exception.SomeException [Persist.Single (Maybe String)] <-
    Exception.try . (liftIO . (flip Persist.runSqlPersistMPool) pool) $ do
        Persist.rawSql @(Persist.Single (Maybe String)) "select pg_sleep(2)" []

  -- when we try the above we get back:
  -- 'result: Left libpq: failed (another command is already in progress'
  -- this is because the connection went back into the pool before it was ready
  -- or perhaps it should have been destroyed and a new connection created and put into the pool?
  logInfoN $ "result: " <> show result

createTableFoo :: Pool.Pool Persist.SqlBackend -> IO ()
createTableFoo pool = (flip Persist.runSqlPersistMPool) pool $ do
  Persist.rawExecute "CREATE table if not exists foo(id int);" []

simulateFailedLongRunningPostgresCall
    :: (MonadLogger m, MonadUnliftIO m, forall a b. Coercible a b => Coercible (m a) (m b)) => Pool.Pool Persist.SqlBackend -> m ()
simulateFailedLongRunningPostgresCall pool = do
  threadId <- Concurrent.forkIO
    $ (do
        me <- Concurrent.myThreadId
        prefixLogs (show me) $ do
            let numThings :: Int = 100000000
            logInfoN $ "start inserting " <> show numThings <> " things"

            (`Persist.runSqlPool` pool) $ do
                logInfoN "inside of thing"
                Monad.forM_ [1 .. numThings] $ \i -> do
                    Monad.when (i `mod` 1000 == 0) $
                        logInfoN $ "Thing #: " <> show i
                    Persist.rawExecute "insert into foo values(1);" []
      )
  Concurrent.threadDelay 1000000
  Monad.void $ Concurrent.killThread threadId
  logInfoN "killed thread"
