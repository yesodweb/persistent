{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Init (
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
  , sqlite_database_file
#endif
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
#ifndef WITH_NOSQL
  , module Database.Persist.Sql
#else
  , PersistFieldSql(..)
#endif
  , BS.ByteString
  , SomeException
  , MonadFail
  , TestFn(..)
  , truncateTimeOfDay
  , truncateToMicro
  , truncateUTCTime
  , arbText
  , liftA2
  , AbstractTest
  ) where

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
import qualified Database.MongoDB as MongoDB
import qualified Database.Persist.MongoDB as MongoDB

#ifdef WITH_NOSQL
import Database.Persist.Sql (PersistFieldSql(..))
import Database.Persist.TH (mkPersistSettings)
import Language.Haskell.TH.Syntax (Type(..))

#  ifdef WITH_MONGODB
import Database.Persist.MongoDB (Action, withMongoPool, runMongoDBPool, defaultMongoConf, applyDockerEnv, BackendKey(..))
#  endif

#else
import Control.Monad.Logger
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.Sql
import System.Log.FastLogger (fromLogStr)

#  ifdef WITH_POSTGRESQL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.Persist.Postgresql
#  endif
#  ifdef WITH_SQLITE
import Database.Persist.Sqlite
#  endif
#  ifdef WITH_MYSQL
import Database.Persist.MySQL
import qualified Database.MySQL.Base as MySQL
#  endif
import Data.IORef (newIORef, IORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
#endif

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)

-- Data types
import Data.Int (Int32, Int64)

import Control.Monad.IO.Class


asIO :: IO a -> IO a
asIO a = a

#ifdef WITH_MONGODB
setup :: Action IO ()
setup = setupMongo
type Context = MongoDB.MongoContext
#endif


(@/=), (@==), (==@) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
infix 1 @/= --, /=@
actual @/= expected = liftIO $ assertNotEqual "" expected actual

infix 1 @==, ==@
actual @== expected = liftIO $ actual @?= expected
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

_debugOn :: Bool
#ifdef DEBUG
_debugOn = True
#else
_debugOn = False
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
runConn :: MonadUnliftIO m => Action m backend -> m ()
runConn f = do
  conf <- liftIO $ applyDockerEnv $ defaultMongoConf dbName -- { mgRsPrimary = Just "replicaset" }
  void $ withMongoPool conf $ runMongoDBPool MongoDB.master f

setupMongo :: Action IO ()
setupMongo = void $ MongoDB.dropDatabase dbName
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
#  ifdef WITH_SQLITE
sqlite_database_file :: Text
sqlite_database_file = "testdb.sqlite3"
sqlite_database :: SqliteConnectionInfo
sqlite_database = mkSqliteConnectionInfo sqlite_database_file
#  else
sqlite_database_file :: Text
sqlite_database_file = error "Sqlite tests disabled"
sqlite_database :: ()
sqlite_database = error "Sqlite tests disabled"
#  endif
runConn :: MonadUnliftIO m => SqlPersistT (LoggingT m) t -> m ()
runConn f = do
  travis <- liftIO isTravis
  let debugPrint = not travis && _debugOn
  let printDebug = if debugPrint then print . fromLogStr else void . return
  flip runLoggingT (\_ _ _ s -> printDebug s) $ do
#  ifdef WITH_POSTGRESQL
    _ <- if travis
      then withPostgresqlPool "host=localhost port=5432 user=postgres dbname=persistent" 1 $ runSqlPool f
      else do
        host <- fromMaybe "localhost" A.<$> liftIO dockerPg
        withPostgresqlPool ("host=" <> host <> " port=5432 user=postgres dbname=test") 1 $ runSqlPool f
#  else
#    ifdef WITH_MYSQL
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
#    else
    _<-withSqlitePoolInfo sqlite_database 1 $ runSqlPool f
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

class GenerateKey backend where
    generateKey :: IO (BackendKey backend)

instance GenerateKey MongoDB.MongoContext where
    generateKey = MongoDB.MongoKey `liftM` MongoDB.genObjectId

instance GenerateKey SqlBackend where
    generateKey = do
        i <- readIORef keyCounter
        writeIORef keyCounter (i + 1)
        return $ SqlBackendKey $ i

keyCounter :: IORef Int64
keyCounter = unsafePerformIO $ newIORef 1
{-# NOINLINE keyCounter #-}

-- | A datatype that wraps a function on @entity@ that can has testable results.
--
-- Allows us to write:
--
-- @
-- foo :: entity -> entity -> [TestFn entity] -> Bool
-- foo e0 e1 = all (\(TestFn msg f) -> f e0 == f e1)
-- @
data TestFn entity where
    TestFn
        :: (Show a, Eq a)
        => String
        -> (entity -> a)
        -> TestFn entity

truncateTimeOfDay :: TimeOfDay -> Gen TimeOfDay
truncateTimeOfDay (TimeOfDay h m s) =
  return $ TimeOfDay h m $ truncateToMicro s

-- truncate less significant digits
truncateToMicro :: Pico -> Pico
truncateToMicro p = let
  p' = fromRational . toRational $ p  :: Micro
  in   fromRational . toRational $ p' :: Pico

truncateUTCTime :: UTCTime -> Gen UTCTime
truncateUTCTime (UTCTime d dift) = do
  let pico = fromRational . toRational $ dift :: Pico
      picoi= truncate . (*1000000000000) . toRational $ truncateToMicro pico :: Integer
      -- https://github.com/lpsmith/postgresql-simple/issues/123
      d' = max d $ fromGregorian 1950 1 1
  return $ UTCTime d' $ picosecondsToDiffTime picoi

arbText :: Gen Text
arbText =
     T.pack
  .  filter ((`notElem` forbidden) . generalCategory)
  .  filter (<= '\xFFFF') -- only BMP
  .  filter (/= '\0')     -- no nulls
  .  T.unpack
  <$> arbitrary
  where forbidden = [NotAssigned, PrivateUse]

type RunDb backend m = ReaderT backend m () -> IO ()

type AbstractTest backend entity m =
    ( PersistEntity entity
    , PersistEntityBackend entity ~ BaseBackend backend
    , Show entity, Eq entity
    , MonadIO m, MonadFail m
    , PersistStoreWrite backend
    )
