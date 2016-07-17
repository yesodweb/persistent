{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Init (
  (@/=), (@==), (==@)
  , assertNotEqual
  , assertNotEmpty
  , assertEmpty
  , isTravis
  , BackendMonad
  , runConn

  , MonadIO
  , persistSettings
  , MkPersistSettings (..)
#ifdef WITH_NOSQL
  , dbName
  , db'
  , setup
  , mkPersistSettings
  , Action
  , Context
#else
  , db
  , sqlite_database
#endif
  , BackendKey(..)
  , generateKey

   -- re-exports
  , (<$>), (<*>)
  , module Database.Persist
  , module Test.Hspec
  , module Test.HUnit
  , liftIO
  , mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase
  , Int32, Int64
  , Text
  , module Control.Monad.Trans.Reader
  , module Control.Monad
#ifndef WITH_NOSQL
  , module Database.Persist.Sql
#else
  , PersistFieldSql(..)
#endif
  , ByteString
) where

-- re-exports
import Control.Applicative ((<$>), (<*>))
import Control.Monad (void, replicateM, liftM, when, forM_)
import Control.Monad.Trans.Reader
import Test.Hspec
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase, MkPersistSettings(..))

-- testing
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)
import Test.QuickCheck

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Database.Persist
import Database.Persist.TH ()
import Data.Text (Text, unpack)
import System.Environment (getEnvironment)

#ifdef WITH_NOSQL
import Language.Haskell.TH.Syntax (Type(..))
import Database.Persist.TH (mkPersistSettings)
import Database.Persist.Sql (PersistFieldSql(..))

#  ifdef WITH_MONGODB
import qualified Database.MongoDB as MongoDB
import Database.Persist.MongoDB (Action, withMongoPool, runMongoDBPool, defaultMongoConf, applyDockerEnv, BackendKey(..))
#  endif

#  ifdef WITH_ZOOKEEPER
import qualified Database.Zookeeper as Z
import Database.Persist.Zookeeper (Action, withZookeeperPool, runZookeeperPool, ZookeeperConf(..), defaultZookeeperConf, BackendKey(..), deleteRecursive)
import Data.IORef (newIORef, IORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
#  endif

#else
import Database.Persist.Sql
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Logger
import System.Log.FastLogger (fromLogStr)

#  ifdef WITH_POSTGRESQL
import Database.Persist.Postgresql
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
#  endif
#  ifdef WITH_SQLITE
import Database.Persist.Sqlite
#  endif
#  ifdef WITH_MYSQL
import Database.Persist.MySQL
#  endif
import Data.IORef (newIORef, IORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
#endif

import Control.Monad (unless, (>=>))
import Control.Monad.Trans.Control (MonadBaseControl)

-- Data types
import Data.Int (Int32, Int64)

import Control.Monad.IO.Class


#ifdef WITH_MONGODB
setup :: Action IO ()
setup = setupMongo
type Context = MongoDB.MongoContext
#endif

#ifdef WITH_ZOOKEEPER
setup :: Action IO ()
setup = setupZookeeper
type Context = Z.Zookeeper
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

isTravis :: IO Bool
isTravis = do
  env <- liftIO getEnvironment
  return $ case lookup "TRAVIS" env of
    Just "true" -> True
    _ -> False

debugOn :: Bool
#ifdef DEBUG
debugOn = True
#else
debugOn = False
#endif

#ifdef WITH_POSTGRESQL
dockerPg :: IO (Maybe BS.ByteString)
dockerPg = do
  env <- liftIO getEnvironment
  return $ case lookup "POSTGRES_NAME" env of
    Just _name -> Just "postgres" -- /persistent/postgres
    _ -> Nothing
#endif

#ifdef WITH_NOSQL
persistSettings :: MkPersistSettings
persistSettings = (mkPersistSettings $ ConT ''Context) { mpsGeneric = True }

dbName :: Text
dbName = "persistent"

type BackendMonad = Context

#ifdef WITH_MONGODB
runConn :: (MonadIO m, MonadBaseControl IO m) => Action m backend -> m ()
runConn f = do
  conf <- liftIO $ applyDockerEnv $ defaultMongoConf dbName -- { mgRsPrimary = Just "replicaset" }
  void $ withMongoPool conf $ runMongoDBPool MongoDB.master f

setupMongo :: Action IO ()
setupMongo = void $ MongoDB.dropDatabase dbName
#endif

#ifdef WITH_ZOOKEEPER
runConn :: (MonadIO m, MonadBaseControl IO m) => Action m backend -> m ()
runConn f = do
  let conf = defaultZookeeperConf {zCoord = "localhost:2181/" ++ T.unpack dbName}
  void $ withZookeeperPool conf $ runZookeeperPool f

setupZookeeper :: Action IO ()
setupZookeeper = do
  liftIO $ Z.setDebugLevel Z.ZLogError
  deleteRecursive "/"
#endif

db' :: Action IO () -> Action IO () -> Assertion
db' actions cleanDB = do
  r <- runConn (actions >> cleanDB)
  return r

instance Arbitrary PersistValue where
    arbitrary = PersistObjectId `fmap` BS.pack `fmap` replicateM 12 arbitrary
#else
persistSettings :: MkPersistSettings
persistSettings = sqlSettings { mpsGeneric = True }
type BackendMonad = SqlBackend
sqlite_database :: Text
sqlite_database = "test/testdb.sqlite3"
-- sqlite_database = ":memory:"
runConn :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT (LoggingT m) t -> m ()
runConn f = do
  travis <- liftIO isTravis
  let debugPrint = not travis && debugOn
  let printDebug = if debugPrint then print . fromLogStr else void . return
  flip runLoggingT (\_ _ _ s -> printDebug s) $ do
#  ifdef WITH_POSTGRESQL
    _ <- if travis
      then withPostgresqlPool "host=localhost port=5432 user=postgres dbname=persistent" 1 $ runSqlPool f
      else do
        host <- fromMaybe "localhost" <$> liftIO dockerPg
        withPostgresqlPool ("host=" <> host <> " port=5432 user=postgres dbname=test") 1 $ runSqlPool f
#  else
#    ifdef WITH_MYSQL
    _ <- if not travis
      then withMySQLPool defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "test"
                        , connectPassword = "test"
                        , connectDatabase = "test"
                        } 1 $ runSqlPool f
      else withMySQLPool defaultConnectInfo
                        { connectHost     = "localhost"
                        , connectUser     = "travis"
                        , connectPassword = ""
                        , connectDatabase = "persistent"
                        } 1 $ runSqlPool f
#    else
    _<-withSqlitePool sqlite_database 1 $ runSqlPool f
#    endif
#  endif
    return ()

db :: SqlPersistT (LoggingT (ResourceT IO)) () -> Assertion
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

instance PersistStore backend => Arbitrary (BackendKey backend) where
  arbitrary = (errorLeft . fromPersistValue) `fmap` arbitrary
    where
      errorLeft x = case x of
          Left e -> error $ unpack e
          Right r -> r

#ifdef WITH_NOSQL
#ifdef WITH_MONGODB
generateKey :: IO (BackendKey Context)
generateKey = MongoKey `liftM` MongoDB.genObjectId
#endif

#ifdef WITH_ZOOKEEPER
keyCounter :: IORef Int64
keyCounter = unsafePerformIO $ newIORef 1
{-# NOINLINE keyCounter #-}

generateKey :: IO (BackendKey Context)
generateKey = do
    i <- readIORef keyCounter
    writeIORef keyCounter (i + 1)
    return $ ZooKey $ T.pack $ show i
#endif

#else
keyCounter :: IORef Int64
keyCounter = unsafePerformIO $ newIORef 1
{-# NOINLINE keyCounter #-}

generateKey :: IO (BackendKey SqlBackend)
generateKey = do
    i <- readIORef keyCounter
    writeIORef keyCounter (i + 1)
    return $ SqlBackendKey $ i
#endif
