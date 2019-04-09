{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module SqliteInit (
  (@/=), (@==), (==@)
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
  , BackendKey(..)
  , GenerateKey(..)

  , RunDb
   -- re-exports
  , (A.<$>), (A.<*>)
  , module Database.Persist
  , module Database.Persist.Sql.Raw.QQ
  , module Test.Hspec
  , module Test.HUnit
  , liftIO
  , mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase
  , Int32, Int64
  , Text
  , module Control.Monad.Trans.Reader
  , module Control.Monad
  , module Database.Persist.Sql
  , BS.ByteString
  , SomeException
  , MonadFail
  , TestFn(..)
  , truncateTimeOfDay
  , truncateToMicro
  , truncateUTCTime
  , arbText
  , liftA2
  ) where

import Init
    ( TestFn(..), AbstractTest, truncateTimeOfDay, truncateUTCTime
    , truncateToMicro, arbText, liftA2, GenerateKey(..)
    , (@/=), (@==), (==@)
    , assertNotEqual, assertNotEmpty, assertEmpty, asIO
    , isTravis, RunDb
    )


-- re-exports
import Control.Applicative (liftA2)
import Test.QuickCheck.Instances ()
import Data.Char (generalCategory, GeneralCategory(..))
import qualified Data.Text as T
import Data.Fixed (Pico,Micro)
import Data.Time
import Control.Monad.Fail
import Control.Applicative as A ((<$>), (<*>))
import Control.Exception (SomeException)
import Control.Monad (void, replicateM, liftM, when, forM_)
import Control.Monad.Trans.Reader
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase, MkPersistSettings(..))
import Database.Persist.Sql.Raw.QQ
import Test.Hspec

-- testing
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)
import Test.QuickCheck

import qualified Data.ByteString as BS
import Data.Text (Text, unpack)
import Database.Persist
import Database.Persist.TH ()
import System.Environment (getEnvironment)

import Control.Monad.Logger
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.Sql
import System.Log.FastLogger (fromLogStr)

import Database.Persist.Sqlite
import Data.IORef (newIORef, IORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)

-- Data types
import Data.Int (Int32, Int64)

import Control.Monad.IO.Class

_debugOn :: Bool
_debugOn = False

persistSettings :: MkPersistSettings
persistSettings = sqlSettings { mpsGeneric = True }
type BackendMonad = SqlBackend

sqlite_database_file :: Text
sqlite_database_file = "testdb.sqlite3"
sqlite_database :: SqliteConnectionInfo
sqlite_database = mkSqliteConnectionInfo sqlite_database_file
runConn :: MonadUnliftIO m => SqlPersistT (LoggingT m) t -> m ()
runConn f = do
  travis <- liftIO isTravis
  let debugPrint = not travis && _debugOn
  let printDebug = if debugPrint then print . fromLogStr else void . return
  flip runLoggingT (\_ _ _ s -> printDebug s) $ do
    _<-withSqlitePoolInfo sqlite_database 1 $ runSqlPool f
    return ()

db :: SqlPersistT (LoggingT (ResourceT IO)) () -> Assertion
db actions = do
  runResourceT $ runConn $ actions >> transactionUndo
