{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ArrayTest where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.List (sort)
import qualified Data.Text as T
import Test.Hspec.Expectations ()

import PersistentTestModels
import PgInit
import ArrayTest.Instances

share [mkPersist persistSettings,  mkMigrate "migrate"] [persistLowerCase|
  TestRoundtrip
    test RoundtripTextArray
    deriving Show Eq
  TestListHack
    test ListHackTextArray
    deriving Show Eq
  TestIntArray
    test IntArray
    deriving Show Eq
  TestJSONArray
    test (JSONArray Text)
    deriving Show Eq
|]

-- cleanDB :: (BaseBackend backend ~ SqlBackend, PersistQueryWrite backend, MonadIO m) => ReaderT backend m ()
-- cleanDB = deleteWhere ([] :: [Filter TestValue])

-- setup :: IO TestKeys
-- setup = asIO $ runConn_ $ do
--   void $ runMigrationSilent migrate

-- teardown = cleanDB

shouldBeIO :: (Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldBeIO x y = liftIO $ shouldBe x y

roundTrip :: (MonadIO m, PersistStoreWrite backend,
                    PersistEntity a, Show a, Eq a,
                    PersistEntityBackend a ~ BaseBackend backend) =>
                   a -> ReaderT backend m ()
roundTrip x = do
  xId <- insert x
  maybeX <- get xId
  case maybeX of
    Nothing -> error "expected to get Just"
    Just x2 -> x2 `shouldBeIO` x

specs :: Spec
specs = do
  describe "Roundtripping from PersistArray" $ do
    it "can insert a value serialized to PersistArray, then deserialize from it, and it will be equivalent" $ do
      runConnAssert $ do
        -- This will fail, because it will get a PersistList when deserializing instead of a PersistArray
        roundTrip $ TestRoundtrip $ RoundtripTextArray ["x"]
    describe "list hack workaround" $ do
      it "can insert a value serialized to PersistArray, then deserialize from it, and it will be equivalent" $ do
        runConnAssert $ do
          roundTrip $ TestListHack $ ListHackTextArray ["x"]
      it "works on data that will need escaping" $ do
        runConnAssert $ do
          roundTrip (TestListHack $ ListHackTextArray ["\""])
    describe "IntArray" $ do
      it "works for ints" $ do
        runConnAssert $ do
          roundTrip (TestIntArray $ IntArray [1,2,3])
    describe "JSONArray" $ do
      it "works for json" $ do
        runConnAssert $ do
          -- This will fail with this error:
          -- SqlError {sqlState = "42804", sqlExecStatus = FatalError, sqlErrorMsg = "column \"test\" is of type jsonb[] but expression is of type text[]", sqlErrorDetail = "", sqlErrorHint = "You will need to rewrite or cast the expression."}
          roundTrip (TestJSONArray $ JSONArray ["x"])





