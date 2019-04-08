{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module PgInit (
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

import Control.Monad.Logger
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.Sql
import System.Log.FastLogger (fromLogStr)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.Persist.Postgresql
import Data.IORef (newIORef, IORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)

-- Data types
import Data.Int (Int32, Int64)

import Control.Monad.IO.Class


asIO :: IO a -> IO a
asIO a = a

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
_debugOn = True

dockerPg :: IO (Maybe BS.ByteString)
dockerPg = do
  env <- liftIO getEnvironment
  return $ case lookup "POSTGRES_NAME" env of
    Just _name -> Just "postgres" -- /persistent/postgres
    _ -> Nothing

persistSettings :: MkPersistSettings
persistSettings = sqlSettings { mpsGeneric = True }
type BackendMonad = SqlBackend

runConn :: MonadUnliftIO m => SqlPersistT (LoggingT m) t -> m ()
runConn f = do
  travis <- liftIO isTravis
  let debugPrint = not travis && _debugOn
  let printDebug = if debugPrint then print . fromLogStr else void . return
  flip runLoggingT (\_ _ _ s -> printDebug s) $ do
    _ <- if travis
      then withPostgresqlPool "host=localhost port=5432 user=postgres dbname=persistent" 1 $ runSqlPool f
      else do
        host <- fromMaybe "localhost" A.<$> liftIO dockerPg
        withPostgresqlPool ("host=" <> host <> " port=5432 user=postgres dbname=test") 1 $ runSqlPool f
    return ()

db :: SqlPersistT (LoggingT (ResourceT IO)) () -> Assertion
db actions = do
  runResourceT $ runConn $ actions >> transactionUndo

instance Arbitrary PersistValue where
    arbitrary = PersistObjectId `fmap` BS.pack `fmap` replicateM 12 arbitrary

instance PersistStore backend => Arbitrary (BackendKey backend) where
  arbitrary = (errorLeft . fromPersistValue) `fmap` arbitrary
    where
      errorLeft x = case x of
          Left e -> error $ unpack e
          Right r -> r

class GenerateKey backend where
    generateKey :: IO (BackendKey backend)

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
