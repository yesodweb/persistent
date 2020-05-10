{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main where

import Criterion.Main
import Control.Exception (SomeException)
import Control.Monad (void, replicateM, liftM, when, forM_)
import Control.Monad.Trans.Reader
import Data.Aeson (Value(..))
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase, MkPersistSettings(..))
import Database.Persist.Sql.Raw.QQ
import Database.Persist.Postgresql.JSON()
import Data.Time.Clock (getCurrentTime)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO)
import UTCTimeSetup
import Control.Monad.Logger
import Database.Persist.Sql
import Database.Persist.Postgresql
import System.Log.FastLogger (fromLogStr)
import Chronos (now, Offset(..), timeToOffsetDatetime)


runConn :: MonadUnliftIO m => SqlPersistT (LoggingT m) t -> m ()
runConn f = runConn_ f >>= const (return ())

runConn_ :: MonadUnliftIO m => SqlPersistT (LoggingT m) t -> m t
runConn_ f = do
  let debugPrint = False
  let printDebug = if debugPrint then print . fromLogStr else void . return
  flip runLoggingT (\_ _ _ s -> printDebug s) $ do
    withPostgresqlPool ("host=" <> "localhost" <> " port=5432 user=postgres dbname=test") 1 $ runSqlPool f

setup :: MonadIO m => Migration -> ReaderT SqlBackend m ()
setup migration = do
  printMigration migration
  runMigrationUnsafe migration

-- Our benchmark harness.
main = do

  runConn $ do
    mapM_ setup
      [ utcTimeBenchmarkMigration
      ]

  runConn $ do
    deleteWhere ([] :: [Filter UserWithTimestamps])

  -- currTime <- getCurrentTime
  currTime <- now
  let utcNow = timeToOffsetDatetime (Offset 0) currTime
  let manyUsers = replicate 10000 $ UserWithTimestamps "first" "last" utcNow utcNow
  runConn $ do
    insertMany_ manyUsers

  let debugPrint = False
  let printDebug = if debugPrint then print . fromLogStr else void . return
  -- flip runLoggingT (\_ _ _ s -> printDebug s) $ do
  runNoLoggingT $ do
    withPostgresqlPool ("host=" <> "localhost" <> " port=5432 user=postgres dbname=test") 1 $ \pool -> do

      NoLoggingT (defaultMain
        [ bench "postAdminOrganizationsStatusR" $ whnfIO (runSqlPool (selectList ([] :: [Filter UserWithTimestamps]) []) pool)
        ])
