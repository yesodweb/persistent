{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Init (
  (@/=), (@==), (==@)
  , assertNotEqual
  , assertNotEmpty
  , assertEmpty
  , BackendMonad
  , runConn

#ifdef WITH_MONGODB
  , db'
  , setupMongo
  , MkPersistSettings (..)
  , mkPersistSettings
  , persistSettings
  , Action
#else
  , db
  , sqlite_database
#endif

   -- re-exports
  , module Database.Persist
  , module Test.Hspec
  , module Test.Hspec.HUnit
  , module Test.HUnit
  , liftIO
  , mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase
  , Int32, Int64
  , Text
) where

-- re-exports
import Test.Hspec
import Test.Hspec.HUnit
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase)

-- testing
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)
import Test.QuickCheck

import Database.Persist
import Data.Text (Text)

#ifdef WITH_MONGODB
import qualified Database.MongoDB as MongoDB
import Database.Persist.MongoDB (Action, withMongoDBConn, runMongoDBPool, MongoBackend)
import Language.Haskell.TH.Syntax (Type(..))
import Database.Persist.TH (mkPersistSettings, MkPersistSettings(..))
import Control.Monad (replicateM)
import qualified Data.ByteString as BS

import Network (PortID (PortNumber))

#else
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Logger

#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
#if WITH_MYSQL
import Database.Persist.MySQL
#endif

#endif

import Control.Monad (unless)
import Control.Monad.Trans.Control (MonadBaseControl)

-- Data types
import Data.Int (Int32, Int64)

import Control.Monad.IO.Class
#if !MIN_VERSION_random(1,0,1)
import System.Random
#endif

(@/=), (@==), (==@) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
infix 1 @/= --, /=@
actual @/= expected = liftIO $ assertNotEqual "" expected actual

infix 1 @==, ==@
expected @== actual = liftIO $ expected @?= actual
expected ==@ actual = liftIO $ expected @=? actual

{-
expected /=@ actual = liftIO $ assertNotEqual "" expected actual
-}


assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual preface expected actual =
  unless (actual /= expected) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n to not equal: " ++ show actual

assertEmpty :: (Monad m, MonadIO m) => [a] -> m ()
assertEmpty xs    = liftIO $ assertBool "" (null xs)

assertNotEmpty :: (Monad m, MonadIO m) => [a] -> m ()
assertNotEmpty xs = liftIO $ assertBool "" (not (null xs))

#ifdef WITH_MONGODB
persistSettings :: MkPersistSettings
persistSettings = mkPersistSettings $ ConT ''MongoBackend

type BackendMonad = MongoBackend
runConn :: (MonadIO m, MonadBaseControl IO m) => Action m backend -> m ()
runConn f = do
  _<-withMongoDBConn "test" "127.0.0.1" (PortNumber 27017) Nothing 5 $
      runMongoDBPool MongoDB.master f
  return ()

--setup :: MongoPersist IO ()
setupMongo :: Action IO ()
setupMongo = do
  -- TODO: check version
  -- v <- MongoDB.access MongoDB.pipe MongoDB.slaveOk "admin" $ MongoDB.runCommand1 "buildInfo"
  v <- MongoDB.serverVersion
  liftIO $ putStrLn $ "version: " ++ show v
  if andVersion v then return () else error "mongoDB version not supported: need at least 1.9.1"
  -- TODO: use dropDatabase
  _<-MongoDB.dropDatabase "test"
  return ()
  where
    andVersion vresult = case show vresult of
      '"':'1':'.':n:'.':minor -> let i = ((read [n]) ::Int) in i > 9 || (i == 9 && ((read $ init minor)::Int) >= 1)
      '"':'2':'.':_ -> True
      _ -> error "unkown version"


db' :: Action IO () -> Action IO () -> Assertion
db' actions cleanDB = do
  r <- runConn (actions >> cleanDB)
  return r

instance Arbitrary PersistValue where
    arbitrary = PersistObjectId `fmap` BS.pack `fmap` replicateM 12 arbitrary

#else
type BackendMonad = SqlBackend
sqlite_database :: Text
sqlite_database = "test/testdb.sqlite3"
-- sqlite_database = ":memory:"
runConn :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT (NoLoggingT m) t -> m ()
runConn f = runNoLoggingT $ do
    _<-withSqlitePool sqlite_database 1 $ runSqlPool f
#  if WITH_POSTGRESQL
    _<-withPostgresqlPool "host=localhost port=5432 user=test dbname=test password=test" 1 $ runSqlPool f
#  endif
#  if WITH_MYSQL
    _ <- withMySQLPool defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        } 1 $ runSqlPool f
#  endif
    return ()

db :: SqlPersistT (NoLoggingT (ResourceT IO)) () -> Assertion
db actions = do
  runResourceT $ runConn $ actions >> transactionUndo

#if !MIN_VERSION_random(1,0,1)
instance Random Int32 where
    random g =
        let ((i::Int), g') = random g in
        (fromInteger $ toInteger i, g')
    randomR (lo, hi) g =
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')

instance Random Int64 where
    random g =
        let ((i0::Int32), g0) = random g
            ((i1::Int32), g1) = random g0 in
        (fromInteger (toInteger i0) + fromInteger (toInteger i1) * 2 ^ (32::Int), g1)
    randomR (lo, hi) g = -- TODO : generate on the whole range, and not only on a part of it
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')
#endif

instance Arbitrary PersistValue where
    arbitrary = PersistInt64 `fmap` choose (0, maxBound)
#endif

instance PersistEntity record => Arbitrary (KeyBackend backend record) where
  arbitrary = persistValueToPersistKey `fmap` arbitrary
