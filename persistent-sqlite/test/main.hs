{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE ScopedTypeVariables #-}

import SqliteInit

import qualified CompositeTest
import qualified CustomPersistFieldTest
import qualified CustomPrimaryKeyReferenceTest
import qualified DataTypeTest
import qualified EmbedOrderTest
import qualified EmbedTest
import qualified EmptyEntityTest
import qualified HtmlTest
import qualified LargeNumberTest
import qualified UpsertTest
import qualified MaxLenTest
import qualified MigrationOnlyTest
import qualified PersistentTest
import qualified RawSqlTest
import qualified MpsNoPrefixTest
import qualified PersistUniqueTest
import qualified PrimaryTest
import qualified Recursive
import qualified RenameTest
import qualified SumTypeTest
import qualified UniqueTest
import qualified MigrationColumnLengthTest
import qualified EquivalentTypeTest
import qualified TransactionLevelTest

import Data.Fixed
import Data.IntMap (IntMap)
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import qualified Database.Sqlite as Sqlite
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

import Control.Exception (handle, IOException)
import Filesystem (removeFile)
import Filesystem.Path.CurrentOS (fromText)
import qualified MigrationTest

type Tuple = (,)

-- Test lower case names
share [mkPersist persistSettings, mkMigrate "dataTypeMigrate"] [persistLowerCase|
DataTypeTable no-json
    text Text
    textMaxLen Text maxlen=100
    bytes ByteString
    bytesTextTuple (Tuple ByteString Text)
    bytesMaxLen ByteString maxlen=100
    int Int
    intList [Int]
    intMap (IntMap Int)
    double Double
    bool Bool
    day Day
    pico Pico
    time TimeOfDay
    utc UTCTime
|]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Test
  time UTCTime
|]

setup :: MonadIO m => Migration -> ReaderT SqlBackend m ()
setup migration = do
  printMigration migration
  runMigrationUnsafe migration

main :: IO ()
main = do
  handle (\(_ :: IOException) -> return ())
    $ removeFile $ fromText sqlite_database_file

  runConn $ do
    mapM_ setup
      [ PersistentTest.testMigrate
      , PersistentTest.noPrefixMigrate
      , EmbedTest.embedMigrate
      , EmbedOrderTest.embedOrderMigrate
      , LargeNumberTest.numberMigrate
      , UniqueTest.uniqueMigrate
      , MaxLenTest.maxlenMigrate
      , Recursive.recursiveMigrate
      , CompositeTest.compositeMigrate
      , MigrationTest.migrationMigrate
      , PersistUniqueTest.migration
      , RenameTest.migration
      , CustomPersistFieldTest.customFieldMigrate
      , PrimaryTest.migration
      , CustomPrimaryKeyReferenceTest.migration
      , MigrationColumnLengthTest.migration
      , TransactionLevelTest.migration
      ]
    PersistentTest.cleanDB

  hspec $ do
    RenameTest.specs
    DataTypeTest.specs
    HtmlTest.specs
    EmbedTest.specs
    EmbedOrderTest.specs
    LargeNumberTest.specs
    UniqueTest.specs
    MaxLenTest.specs
    Recursive.specs
    SumTypeTest.specs
    MigrationOnlyTest.specs
    PersistentTest.specs
    PersistentTest.filterOrSpecs db
    RawSqlTest.specs
    UpsertTest.specsWith
        db
        UpsertTest.Don'tUpdateNull
        UpsertTest.UpsertPreserveOldKey

    MpsNoPrefixTest.specs
    EmptyEntityTest.specs
    CompositeTest.specs
    PersistUniqueTest.specs
    PrimaryTest.specs
    CustomPersistFieldTest.specs
    CustomPrimaryKeyReferenceTest.specs
    MigrationColumnLengthTest.specs
    EquivalentTypeTest.specs
    TransactionLevelTest.specs
    MigrationTest.specs

    it "issue #328" $ asIO $ runSqliteInfo (mkSqliteConnectionInfo ":memory:") $ do
        runMigration migrateAll
        _ <- insert . Test $ read "2014-11-30 05:15:25.123"
        [Single x] <- rawSql "select strftime('%s%f',time) from test" []
        liftIO $ x `shouldBe` Just ("141732452525.123" :: String)
    it "issue #339" $ asIO $ runSqliteInfo (mkSqliteConnectionInfo ":memory:") $ do
        runMigration migrateAll
        now <- liftIO getCurrentTime
        tid <- insert $ Test now
        Just (Test now') <- get tid
        liftIO $ now' `shouldBe` now
    it "issue #564" $ asIO $ withSystemTempFile "test564.sqlite3"$ \fp h -> do
        hClose h
        conn <- Sqlite.open (T.pack fp)
        Sqlite.close conn
        return ()
    it "issue #527" $ asIO $ runSqliteInfo (mkSqliteConnectionInfo ":memory:") $ do
        runMigration migrateAll
        insertMany_ $ replicate 1000 (Test $ read "2014-11-30 05:15:25.123")
