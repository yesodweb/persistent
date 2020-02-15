{-# LANGUAGE OverloadedStrings #-}

module MyInit (
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
  , BackendKey(..)
  , GenerateKey(..)

  , RunDb
   -- re-exports
  , module Database.Persist
  , module Database.Persist.Sql.Raw.QQ
  , module Test.Hspec
  , module Test.HUnit
  , MonadUnliftIO
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
    ( TestFn(..), truncateTimeOfDay, truncateUTCTime
    , truncateToMicro, arbText, GenerateKey(..)
    , (@/=), (@==), (==@)
    , assertNotEqual, assertNotEmpty, assertEmpty, asIO
    , isTravis, RunDb, MonadFail
    )

-- re-exports
import Control.Applicative (liftA2)
import Control.Exception (SomeException)
import Control.Monad (void, replicateM, liftM, when, forM_)
import Control.Monad.Trans.Reader
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase, MkPersistSettings(..))
import Database.Persist.Sql.Raw.QQ
import Test.Hspec
import Test.QuickCheck.Instances ()

-- testing
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Database.MySQL.Base as MySQL
import System.Log.FastLogger (fromLogStr)

import Database.Persist
import Database.Persist.MySQL
import Database.Persist.Sql
import Database.Persist.TH ()

_debugOn :: Bool
_debugOn = False

persistSettings :: MkPersistSettings
persistSettings = sqlSettings { mpsGeneric = True }

type BackendMonad = SqlBackend

runConn :: MonadUnliftIO m => SqlPersistT (LoggingT m) t -> m ()
runConn f = do
  travis <- liftIO isTravis
  let debugPrint = not travis && _debugOn
  let printDebug = if debugPrint then print . fromLogStr else void . return
  flip runLoggingT (\_ _ _ s -> printDebug s) $ do
    -- Since version 5.7.5, MySQL adds a mode value `STRICT_TRANS_TABLES`
    -- which can cause an exception in MaxLenTest, depending on the server
    -- configuration.  Persistent tests do not need any of the modes which are
    -- set by default, so it is simplest to clear `sql_mode` for the session.
    let baseConnectInfo =
            defaultConnectInfo {
                connectOptions =
                    connectOptions defaultConnectInfo
                    ++ [MySQL.InitCommand "SET SESSION sql_mode = '';\0"]
            }
    _ <- if not travis
      then withMySQLPool baseConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        } 1 $ runSqlPool f
      else withMySQLPool baseConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "travis"
                        , connectPassword = ""
                        , connectDatabase = "persistent"
                        } 1 $ runSqlPool f
    return ()

db :: SqlPersistT (LoggingT (ResourceT IO)) () -> Assertion
db actions = do
  runResourceT $ runConn $ actions >> transactionUndo
