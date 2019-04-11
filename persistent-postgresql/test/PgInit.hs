{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module PgInit (
  runConn

  , MonadIO
  , persistSettings
  , MkPersistSettings (..)
  , db
  , BackendKey(..)
  , GenerateKey(..)

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
  , TestFn(..)
  , module Init
  ) where

import Init
    ( TestFn(..), truncateTimeOfDay, truncateUTCTime
    , truncateToMicro, arbText, liftA2, GenerateKey(..)
    , (@/=), (@==), (==@), MonadFail
    , assertNotEqual, assertNotEmpty, assertEmpty, asIO
    , isTravis, RunDb
    )

-- re-exports
import Test.QuickCheck.Instances ()
import Control.Applicative as A ((<$>), (<*>))
import Control.Exception (SomeException)
import Control.Monad (void, replicateM, liftM, when, forM_)
import Control.Monad.Trans.Reader
import Database.Persist.TH (mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase, MkPersistSettings(..))
import Database.Persist.Sql.Raw.QQ
import Test.Hspec
import Data.Aeson (Value(..))
import Database.Persist.Postgresql.JSON ()
import qualified Data.HashMap.Strict as HM

-- testing
import Test.HUnit ((@?=),(@=?), Assertion, assertFailure, assertBool)
import Test.QuickCheck

import qualified Data.ByteString as BS
import Data.Text (Text)
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

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)

-- Data types
import Data.Int (Int32, Int64)

import Control.Monad.IO.Class


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
