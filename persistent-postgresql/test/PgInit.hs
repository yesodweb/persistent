{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PgInit (
  runConn
  , runConn_
  , runConnAssert
  , runConnAssertUseConf

  , MonadIO
  , persistSettings
  , MkPersistSettings (..)
  , BackendKey(..)
  , GenerateKey(..)

   -- re-exports
  , module Control.Monad.Trans.Reader
  , module Control.Monad
  , module Database.Persist.Sql
  , module Database.Persist
  , module Database.Persist.Sql.Raw.QQ
  , module Init
  , module Test.Hspec
  , module Test.HUnit
  , BS.ByteString
  , Int32, Int64
  , liftIO
  , mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase
  , SomeException
  , Text
  , TestFn(..)
  ) where

import Init
    ( TestFn(..), truncateTimeOfDay, truncateUTCTime
    , truncateToMicro, arbText, liftA2, GenerateKey(..)
    , (@/=), (@==), (==@), MonadFail
    , assertNotEqual, assertNotEmpty, assertEmpty, asIO
    , isTravis, RunDb
    )

-- re-exports
import Control.Exception (SomeException)
import Control.Monad (void, replicateM, liftM, when, forM_)
import Control.Monad.Trans.Reader
import Data.Aeson (Value(..))
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase, MkPersistSettings(..))
import Database.Persist.Sql.Raw.QQ
import Database.Persist.Postgresql.JSON()
import Test.Hspec
import Test.QuickCheck.Instances ()

-- testing
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)
import Test.QuickCheck

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.Environment (getEnvironment)
import System.Log.FastLogger (fromLogStr)

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Persist.TH ()

_debugOn :: Bool
_debugOn = False

dockerPg :: IO (Maybe BS.ByteString)
dockerPg = do
  env <- liftIO getEnvironment
  return $ case lookup "POSTGRES_NAME" env of
    Just _name -> Just "postgres" -- /persistent/postgres
    _ -> Nothing

persistSettings :: MkPersistSettings
persistSettings = sqlSettings { mpsGeneric = True }

runConn :: MonadUnliftIO m => SqlPersistT (LoggingT m) t -> m ()
runConn f = runConn_ f >>= const (return ())

runConn_ :: MonadUnliftIO m => SqlPersistT (LoggingT m) t -> m t
runConn_ f = runConnInternal RunConnBasic f

-- | Data type to switch between pool creation functions, to ease testing both.
data RunConnType =
    RunConnBasic -- ^ Use 'withPostgresqlPool'
  | RunConnConf -- ^ Use 'withPostgresqlPoolWithConf'
  deriving (Show, Eq)

runConnInternal :: MonadUnliftIO m => RunConnType -> SqlPersistT (LoggingT m) t -> m t
runConnInternal connType f = do
  travis <- liftIO isTravis
  let debugPrint = not travis && _debugOn
      printDebug = if debugPrint then print . fromLogStr else void . return
      poolSize = 1
  connString <- if travis
    then do
      pure "host=localhost port=5432 user=perstest password=perstest dbname=persistent"
    else do
      host <- fromMaybe "localhost" <$> liftIO dockerPg
      pure ("host=" <> host <> " port=5432 user=postgres dbname=test")

  flip runLoggingT (\_ _ _ s -> printDebug s) $ do
    logInfoN (if travis then "Running in CI" else "CI not detected")
    case connType of
      RunConnBasic -> withPostgresqlPool connString poolSize $ runSqlPool f
      RunConnConf -> do
        let conf = PostgresConf
              { pgConnStr = connString
              , pgPoolStripes = 1
              , pgPoolIdleTimeout = 60
              , pgPoolSize = poolSize
              }
            hooks = defaultPostgresConfHooks
        withPostgresqlPoolWithConf conf hooks (runSqlPool f)

runConnAssert :: SqlPersistT (LoggingT (ResourceT IO)) () -> Assertion
runConnAssert actions = do
  runResourceT $ runConn $ actions >> transactionUndo

-- | Like runConnAssert, but uses the "conf" flavor of functions to test that code path.
runConnAssertUseConf :: SqlPersistT (LoggingT (ResourceT IO)) () -> Assertion
runConnAssertUseConf actions = do
  runResourceT $ runConnInternal RunConnConf (actions >> transactionUndo)

instance Arbitrary Value where
  arbitrary = frequency [ (1, pure Null)
                        , (1, Bool <$> arbitrary)
                        , (2, Number <$> arbitrary)
                        , (2, String <$> arbText)
                        , (3, Array <$> limitIt 4 arbitrary)
                        , (3, Object <$> arbObject)
                        ]
    where limitIt i x = sized $ \n -> do
            let m = if n > i then i else n
            resize m x
          arbObject = limitIt 4 -- Recursion can make execution divergent
                    $ fmap HM.fromList -- HashMap -> [(,)]
                    . listOf -- [(,)] -> (,)
                    . liftA2 (,) arbText -- (,) -> Text and Value
                    $ limitIt 4 arbitrary -- Again, precaution against divergent recursion.
