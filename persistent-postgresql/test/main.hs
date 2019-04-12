{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE UndecidableInstances #-}
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

import PgInit

import Data.IntMap (IntMap)
import Data.Aeson
import Data.Fixed
import Test.QuickCheck
import qualified Data.Text as T
import Data.Time
import Data.Int
import Data.Word
import Data.IntMap (IntMap)
import Control.Monad.Trans
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

import qualified CompositeTest
import qualified TreeTest
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
import qualified ReadWriteTest
import qualified RawSqlTest
import qualified MpsNoPrefixTest
import qualified PersistUniqueTest
import qualified PrimaryTest
import qualified Recursive
import qualified RenameTest
import qualified SumTypeTest
import qualified UniqueTest
import qualified MigrationColumnLengthTest
import qualified EquivalentTypeTestPostgres
import qualified TransactionLevelTest
import qualified JSONTest

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
      ]
    PersistentTest.cleanDB

  hspec $ do
    RenameTest.specsWith db
    DataTypeTest.specsWith db
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
        db
        (Just (runMigrationSilent HtmlTest.htmlMigrate))
    EmbedTest.specsWith db
    EmbedOrderTest.specsWith db
    LargeNumberTest.specsWith db
    UniqueTest.specsWith db
    MaxLenTest.specsWith db
    Recursive.specsWith db
    SumTypeTest.specsWith db (Just (runMigrationSilent SumTypeTest.sumTypeMigrate))
    MigrationOnlyTest.specsWith db
        (Just
            $ runMigrationSilent MigrationOnlyTest.migrateAll1
            >> runMigrationSilent MigrationOnlyTest.migrateAll2
        )
    PersistentTest.specsWith db
    ReadWriteTest.specsWith db
    PersistentTest.filterOrSpecs db
    RawSqlTest.specsWith db
    UpsertTest.specsWith
        db
        UpsertTest.Don'tUpdateNull
        UpsertTest.UpsertPreserveOldKey

    MpsNoPrefixTest.specsWith db
    EmptyEntityTest.specsWith db (Just (runMigrationSilent EmptyEntityTest.migration))
    CompositeTest.specsWith db
    TreeTest.specsWith db
    PersistUniqueTest.specsWith db
    PrimaryTest.specsWith db
    CustomPersistFieldTest.specsWith db
    CustomPrimaryKeyReferenceTest.specsWith db
    MigrationColumnLengthTest.specsWith db
    EquivalentTypeTestPostgres.specs
    TransactionLevelTest.specsWith db
    JSONTest.specs
