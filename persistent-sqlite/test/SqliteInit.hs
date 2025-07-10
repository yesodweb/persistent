{-# LANGUAGE OverloadedStrings #-}

module SqliteInit
    ( (@/=)
    , (@==)
    , (==@)
    , asIO
    , assertNotEqual
    , assertNotEmpty
    , assertEmpty
    , isTravis
    , BackendMonad
    , runConn
    , MonadIO
    , persistSettings
    , MkPersistSettings (..)
    , db
    , sqlite_database
    , sqlite_database_file
    , BackendKey (..)
    , GenerateKey (..)
    , RunDb
    -- re-exports
    , module Database.Persist
    , module Test.Hspec
    , module Test.HUnit
    , liftIO
    , mkPersist
    , mkMigrate
    , share
    , sqlSettings
    , persistLowerCase
    , persistUpperCase
    , Int32
    , Int64
    , Text
    , module Control.Monad.Trans.Reader
    , module Control.Monad
    , module Database.Persist.Sql
    , BS.ByteString
    , SomeException
    , TestFn (..)
    , truncateTimeOfDay
    , truncateToMicro
    , truncateUTCTime
    , arbText
    , liftA2
    , MonadFail
    ) where

import Init
    ( GenerateKey (..)
    , MonadFail
    , RunDb
    , TestFn (..)
    , arbText
    , asIO
    , assertEmpty
    , assertNotEmpty
    , assertNotEqual
    , isTravis
    , liftA2
    , truncateTimeOfDay
    , truncateToMicro
    , truncateUTCTime
    , (==@)
    , (@/=)
    , (@==)
    )

-- re-exports
import Control.Exception (SomeException)
import Control.Monad (forM_, liftM, replicateM, void, when)
import Control.Monad.Trans.Reader
import Database.Persist.TH
    ( MkPersistSettings (..)
    , mkMigrate
    , mkPersist
    , persistLowerCase
    , persistUpperCase
    , share
    , sqlSettings
    )
import Test.Hspec

-- testing
import Test.HUnit (Assertion, assertBool, assertFailure, (@=?), (@?=))

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString as BS
import Data.Text (Text)
import System.Log.FastLogger (fromLogStr)

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH ()

-- Data types
import Control.Monad.IO.Class
import Data.Int (Int32, Int64)

_debugOn :: Bool
_debugOn = False

persistSettings :: MkPersistSettings
persistSettings = sqlSettings{mpsGeneric = True}

type BackendMonad = SqlBackend

sqlite_database_file :: Text
sqlite_database_file = "testdb.sqlite3"

sqlite_database :: SqliteConnectionInfo
sqlite_database = mkSqliteConnectionInfo sqlite_database_file

runConn :: (MonadUnliftIO m) => SqlPersistT (LoggingT m) t -> m ()
runConn f = do
    travis <- liftIO isTravis
    let
        debugPrint = not travis && _debugOn
    let
        printDebug = if debugPrint then print . fromLogStr else void . return
    void $ flip runLoggingT (\_ _ _ s -> printDebug s) $ do
        withSqlitePoolInfo sqlite_database 1 $ runSqlPool f

db :: SqlPersistT (LoggingT (ResourceT IO)) () -> Assertion
db actions = do
    runResourceT $ runConn $ actions >> transactionUndo
