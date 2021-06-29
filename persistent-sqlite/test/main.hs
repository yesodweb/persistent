{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import SqliteInit

import qualified CompositeTest
import qualified CustomPersistFieldTest
import qualified CustomPrimaryKeyReferenceTest
import qualified DataTypeTest
import qualified EmbedOrderTest
import qualified EmbedTest
import qualified EmptyEntityTest
import qualified EquivalentTypeTest
import qualified ForeignKey
import qualified GeneratedColumnTestSQL
import qualified HtmlTest
import qualified LargeNumberTest
import qualified LongIdentifierTest
import qualified MaxLenTest
import qualified MaybeFieldDefsTest
import qualified MigrationColumnLengthTest
import qualified MigrationOnlyTest
import qualified MpsCustomPrefixTest
import qualified MpsNoPrefixTest
import qualified PersistUniqueTest
import qualified PersistentTest
import qualified PrimaryTest
import qualified RawSqlTest
import qualified ReadWriteTest
import qualified Recursive
import qualified RenameTest
import qualified SumTypeTest
import qualified TransactionLevelTest
import qualified UniqueTest
import qualified UpsertTest

import Control.Exception (IOException, handle, throwIO)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString as BS
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import Data.Fixed
import Data.IntMap (IntMap)
import qualified Data.Text as T
import Data.Time
import Filesystem (removeFile)
import Filesystem.Path.CurrentOS (fromText)
import qualified Lens.Micro as Lens
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import Database.Persist.Sqlite
import qualified Database.Sqlite as Sqlite
import PersistentTestModels

import qualified Database.Persist.Sqlite.CompositeSpec as CompositeSpec
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

share [mkPersist sqlSettings, mkMigrate "idSetup"] [persistLowerCase|
Simple
    text Text
    deriving Show Eq

SimpleReference
    simpleCompositeId SimpleId
    text Text
    deriving Show Eq
|]

share [mkPersist sqlSettings, mkMigrate "idMigrateTest"] [persistLowerCase|
Simple2 sql=simple
    text Text
    int Int default=0
    deriving Show Eq

SimpleReference2 sql=simple_reference
    simpleCompositeId Simple2Id
    text Text
    deriving Show Eq
|]

instance Arbitrary DataTypeTable where
  arbitrary = DataTypeTable
     <$> arbText                -- text
     <*> (T.take 100 <$> arbText)          -- textManLen
     <*> arbitrary              -- bytes
     <*> liftA2 (,) arbitrary arbText      -- bytesTextTuple
     <*> (BS.take 100 <$> arbitrary)       -- bytesMaxLen
     <*> arbitrary              -- int
     <*> arbitrary              -- intList
     <*> arbitrary              -- intMap
     <*> arbitrary              -- double
     <*> arbitrary              -- bool
     <*> arbitrary              -- day
     <*> arbitrary              -- pico
     <*> (truncateTimeOfDay =<< arbitrary) -- time
     <*> (truncateUTCTime   =<< arbitrary) -- utc

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
            [ ForeignKey.compositeMigrate
            , PersistentTest.testMigrate
            , PersistentTest.noPrefixMigrate
            , PersistentTest.customPrefixMigrate
            , EmbedTest.embedMigrate
            , EmbedOrderTest.embedOrderMigrate
            , LargeNumberTest.numberMigrate
            , UniqueTest.uniqueMigrate
            , MaxLenTest.maxlenMigrate
            , MaybeFieldDefsTest.maybeFieldDefMigrate
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
            , LongIdentifierTest.migration
            ]
        PersistentTest.cleanDB
        ForeignKey.cleanDB


    hspec $ do
        describe "Database" $ describe "Persist" $ describe "Sqlite" $ do
            CompositeSpec.spec
        RenameTest.specsWith db
        DataTypeTest.specsWith
            db
            (Just (runMigrationSilent dataTypeMigrate))
            [ TestFn "text" dataTypeTableText
            , TestFn "textMaxLen" dataTypeTableTextMaxLen
            , TestFn "bytes" dataTypeTableBytes
            , TestFn "bytesTextTuple" dataTypeTableBytesTextTuple
            , TestFn "bytesMaxLen" dataTypeTableBytesMaxLen
            , TestFn "int" dataTypeTableInt
            , TestFn "intList" dataTypeTableIntList
            , TestFn "intMap" dataTypeTableIntMap
            , TestFn "bool" dataTypeTableBool
            , TestFn "day" dataTypeTableDay
            , TestFn "time" (DataTypeTest.roundTime . dataTypeTableTime)
            , TestFn "utc" (DataTypeTest.roundUTCTime . dataTypeTableUtc)
            ]
            [ ("pico", dataTypeTablePico) ]
            dataTypeTableDouble
        HtmlTest.specsWith
            db
            (Just (runMigrationSilent HtmlTest.htmlMigrate))
        EmbedTest.specsWith db
        EmbedOrderTest.specsWith db
        LargeNumberTest.specsWith db
        UniqueTest.specsWith db
        MaxLenTest.specsWith db
        MaybeFieldDefsTest.specsWith db
        Recursive.specsWith db
        SumTypeTest.specsWith db (Just (runMigrationSilent SumTypeTest.sumTypeMigrate))
        MigrationOnlyTest.specsWith db
            (Just
                $ runMigrationSilent MigrationOnlyTest.migrateAll1
                >> runMigrationSilent MigrationOnlyTest.migrateAll2
            )
        PersistentTest.specsWith db
        PersistentTest.filterOrSpecs db
        ReadWriteTest.specsWith db
        RawSqlTest.specsWith db
        UpsertTest.specsWith
            db
            UpsertTest.Don'tUpdateNull
            UpsertTest.UpsertPreserveOldKey

        MpsNoPrefixTest.specsWith db
        MpsCustomPrefixTest.specsWith db
        EmptyEntityTest.specsWith db (Just (runMigrationSilent EmptyEntityTest.migration))
        CompositeTest.specsWith db
        PersistUniqueTest.specsWith db
        PrimaryTest.specsWith db
        CustomPersistFieldTest.specsWith db
        CustomPrimaryKeyReferenceTest.specsWith db
        MigrationColumnLengthTest.specsWith db
        EquivalentTypeTest.specsWith db
        ForeignKey.specsWith db
        TransactionLevelTest.specsWith db
        MigrationTest.specsWith db
        LongIdentifierTest.specsWith db
        GeneratedColumnTestSQL.specsWith db

        it "issue #328" $ asIO $ runSqliteInfo (mkSqliteConnectionInfo ":memory:") $ do
            void $ runMigrationSilent migrateAll
            insert_ . Test $ read "2014-11-30 05:15:25.123Z"
            [Single x] <- rawSql "select strftime('%s%f',time) from test" []
            liftIO $ x `shouldBe` Just ("141732452525.123" :: String)
        it "issue #339" $ asIO $ runSqliteInfo (mkSqliteConnectionInfo ":memory:") $ do
            void $ runMigrationSilent migrateAll
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
            void $ runMigrationSilent migrateAll
            insertMany_ $ replicate 1000 (Test $ read "2014-11-30 05:15:25.123Z")

        it "afterException" $ asIO $ runSqliteInfo (mkSqliteConnectionInfo ":memory:") $ do
            void $ runMigrationSilent testMigrate
            let catcher :: forall m. Monad m => SomeException -> m ()
                catcher _ = return ()
            insert_ $ Person "A" 0 Nothing
            insert_ (Person "A" 1 Nothing) `catch` catcher
            insert_ $ Person "B" 0 Nothing
            return ()
