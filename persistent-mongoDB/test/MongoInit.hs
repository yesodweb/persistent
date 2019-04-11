{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- We create an orphan instance for GenerateKey here to avoid a circular
-- dependency between:
--
-- a) persistent-mongoDB:test depends on
-- b) persistent-test:lib depends on
-- c) persistent-mongODB:lib
--
-- This kind of cycle is all kinds of bad news.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MongoInit (
  BackendMonad
  , runConn
  , MonadIO
  , persistSettings
  , MkPersistSettings (..)
  , dbName
  , db'
  , setup
  , mkPersistSettings
  , Action
  , Context
  , BackendKey(MongoKey)

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
  , PersistFieldSql(..)
  , BS.ByteString
  , SomeException
  , module Init
  ) where

-- we have to be careful with this import becuase CPP is still a problem
import Init
    ( TestFn(..), truncateTimeOfDay, truncateUTCTime
    , truncateToMicro, arbText, liftA2, GenerateKey(..)
    , (@/=), (@==), (==@)
    , assertNotEqual, assertNotEmpty, assertEmpty, asIO
    , isTravis
    )

-- re-exports
import Control.Applicative as A ((<$>), (<*>))
import Control.Exception (SomeException)
import Control.Monad (void, replicateM, liftM, when, forM_)
import Control.Monad.Trans.Reader
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase, MkPersistSettings(..))
import Database.Persist.Sql.Raw.QQ
import Test.Hspec

-- testing
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)

import qualified Data.ByteString as BS
import Data.Text (Text)
import Database.Persist
import Database.Persist.TH ()

import Database.Persist.Sql (PersistFieldSql(..))
import Database.Persist.TH (mkPersistSettings)
import Language.Haskell.TH.Syntax (Type(..))

import qualified Database.MongoDB as MongoDB
import Database.Persist.MongoDB (Action, withMongoPool, runMongoDBPool, defaultMongoConf, applyDockerEnv, BackendKey(..))

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)

-- Data types
import Data.Int (Int32, Int64)

import Control.Monad.IO.Class


setup :: Action IO ()
setup = setupMongo
type Context = MongoDB.MongoContext

_debugOn :: Bool
_debugOn = True

persistSettings :: MkPersistSettings
persistSettings = (mkPersistSettings $ ConT ''Context) { mpsGeneric = True }

dbName :: Text
dbName = "persistent"

type BackendMonad = Context

runConn :: MonadUnliftIO m => Action m backend -> m ()
runConn f = do
  conf <- liftIO $ applyDockerEnv $ defaultMongoConf dbName -- { mgRsPrimary = Just "replicaset" }
  void $ withMongoPool conf $ runMongoDBPool MongoDB.master f

setupMongo :: Action IO ()
setupMongo = void $ MongoDB.dropDatabase dbName

db' :: Action IO () -> Action IO () -> Assertion
db' actions cleanDB = do
  r <- runConn (actions >> cleanDB)
  return r

instance GenerateKey MongoDB.MongoContext where
    generateKey = MongoKey `liftM` MongoDB.genObjectId
