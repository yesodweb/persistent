{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import PgInit

import Data.Aeson
import qualified Data.ByteString as BS
import Data.IntMap (IntMap)
import Data.Fixed
import qualified Data.Text as T
import Data.Time
import Test.QuickCheck

import qualified ArrayAggTest
import qualified CompositeTest
import qualified ForeignKey
import qualified CustomPersistFieldTest
import qualified CustomPrimaryKeyReferenceTest
import qualified DataTypeTest
import qualified EmbedOrderTest
import qualified EmbedTest
import qualified EmptyEntityTest
import qualified EquivalentTypeTestPostgres
import qualified HtmlTest
import qualified JSONTest
import qualified LargeNumberTest
import qualified MaxLenTest
import qualified MigrationColumnLengthTest
import qualified MigrationOnlyTest
import qualified MpsNoPrefixTest
import qualified MpsCustomPrefixTest
import qualified PersistentTest
import qualified PersistUniqueTest
import qualified PrimaryTest
import qualified RawSqlTest
import qualified ReadWriteTest
import qualified Recursive
import qualified RenameTest
import qualified SumTypeTest
import qualified TransactionLevelTest
import qualified TreeTest
import qualified UniqueTest
import qualified UpsertTest
import qualified CustomConstraintTest
import qualified LongIdentifierTest
import qualified PgIntervalTest

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
    jsonb Value
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
     <*> (arbitrary) -- utc
     <*> (truncateUTCTime   =<< arbitrary) -- utc
     <*> arbitrary              -- value

setup :: MonadIO m => Migration -> ReaderT SqlBackend m ()
setup migration = do
  printMigration migration
  runMigrationUnsafe migration

main :: IO ()
main = do
  runConn $ do
    mapM_ setup
      [ PersistentTest.testMigrate
      , PersistentTest.noPrefixMigrate
      , PersistentTest.customPrefixMigrate
      , PersistentTest.treeMigrate
      , EmbedTest.embedMigrate
      , EmbedOrderTest.embedOrderMigrate
      , LargeNumberTest.numberMigrate
      , UniqueTest.uniqueMigrate
      , MaxLenTest.maxlenMigrate
      , Recursive.recursiveMigrate
      , CompositeTest.compositeMigrate
      , TreeTest.treeMigrate
      , PersistUniqueTest.migration
      , RenameTest.migration
      , CustomPersistFieldTest.customFieldMigrate
      , PrimaryTest.migration
      , CustomPrimaryKeyReferenceTest.migration
      , MigrationColumnLengthTest.migration
      , TransactionLevelTest.migration
      , LongIdentifierTest.migration
      , ForeignKey.compositeMigrate
      , PgIntervalTest.pgIntervalMigrate
      ]
    PersistentTest.cleanDB

  hspec $ do
    RenameTest.specsWith runConnAssert
    DataTypeTest.specsWith runConnAssert
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
        , TestFn "jsonb" dataTypeTableJsonb
        ]
        [ ("pico", dataTypeTablePico) ]
        dataTypeTableDouble
    HtmlTest.specsWith
        runConnAssert
        (Just (runMigrationSilent HtmlTest.htmlMigrate))
    EmbedTest.specsWith runConnAssert
    EmbedOrderTest.specsWith runConnAssert
    LargeNumberTest.specsWith runConnAssert
    ForeignKey.specsWith runConnAssert
    UniqueTest.specsWith runConnAssert
    MaxLenTest.specsWith runConnAssert
    Recursive.specsWith runConnAssert
    SumTypeTest.specsWith runConnAssert (Just (runMigrationSilent SumTypeTest.sumTypeMigrate))
    MigrationOnlyTest.specsWith runConnAssert
        (Just
            $ runMigrationSilent MigrationOnlyTest.migrateAll1
            >> runMigrationSilent MigrationOnlyTest.migrateAll2
        )
    PersistentTest.specsWith runConnAssert
    ReadWriteTest.specsWith runConnAssert
    PersistentTest.filterOrSpecs runConnAssert
    RawSqlTest.specsWith runConnAssert
    UpsertTest.specsWith
        runConnAssert
        UpsertTest.Don'tUpdateNull
        UpsertTest.UpsertPreserveOldKey

    MpsNoPrefixTest.specsWith runConnAssert
    MpsCustomPrefixTest.specsWith runConnAssert
    EmptyEntityTest.specsWith runConnAssert (Just (runMigrationSilent EmptyEntityTest.migration))
    CompositeTest.specsWith runConnAssert
    TreeTest.specsWith runConnAssert
    PersistUniqueTest.specsWith runConnAssert
    PrimaryTest.specsWith runConnAssert
    CustomPersistFieldTest.specsWith runConnAssert
    CustomPrimaryKeyReferenceTest.specsWith runConnAssert
    MigrationColumnLengthTest.specsWith runConnAssert
    EquivalentTypeTestPostgres.specs
    TransactionLevelTest.specsWith runConnAssert
    LongIdentifierTest.specsWith runConnAssert
    JSONTest.specs
    CustomConstraintTest.specs
    PgIntervalTest.specs
    ArrayAggTest.specs
