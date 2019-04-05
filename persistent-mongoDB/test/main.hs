{-# LANGUAGE ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import MongoInit

import Data.Time
import Data.IntMap (IntMap)
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.ByteString as BS

import CustomPersistField

-- These tests are noops with the NoSQL flags set.
-- import qualified CompositeTest
-- import qualified CustomPrimaryKeyReferenceTest
-- import qualified InsertDuplicateUpdate

import qualified CustomPersistFieldTest
import qualified DataTypeTest

import qualified EmbedOrderTest

import qualified EmbedTest
import qualified EmptyEntityTest
import qualified HtmlTest
import qualified LargeNumberTest
import qualified MaxLenTest
import qualified MigrationOnlyTest
import qualified PersistentTest
import qualified PersistUniqueTest
import qualified PrimaryTest
import qualified Recursive
import qualified RenameTest
import qualified SumTypeTest
import qualified UniqueTest
import qualified MigrationColumnLengthTest
import qualified EquivalentTypeTest
import qualified TransactionLevelTest

db :: Action IO () -> Assertion
db = db' (return ())

type Tuple = (,)

mkPersist persistSettings [persistUpperCase|
  BlogPost
    article Markdown
    deriving Show Eq
|]

mkPersist persistSettings [persistUpperCase|
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
    utc UTCTime
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
     <*> (truncateUTCTime   =<< arbitrary) -- utc

main :: IO ()
main = do
  hspec $ do
    RenameTest.specs
    DataTypeTest.specsWith
        db
        (pure ())
        Nothing
        [ TestFn "Text" dataTypeTableText
        , TestFn "Text" dataTypeTableTextMaxLen
        , TestFn "Bytes" dataTypeTableBytes
        , TestFn "Bytes" dataTypeTableBytesTextTuple
        , TestFn "Bytes" dataTypeTableBytesMaxLen
        , TestFn "Int" dataTypeTableInt
        , TestFn "Int" dataTypeTableIntList
        , TestFn "Int" dataTypeTableIntMap
        , TestFn "Double" dataTypeTableDouble
        , TestFn "Bool" dataTypeTableBool
        , TestFn "Day" dataTypeTableDay
        ]
        []
        dataTypeTableDouble
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
    EmptyEntityTest.specs
    -- CompositeTest.specs
    PersistUniqueTest.specs
    PrimaryTest.specs
    CustomPersistFieldTest.specsWith
        db
        BlogPost
    MigrationColumnLengthTest.specs
    EquivalentTypeTest.specs
    TransactionLevelTest.specs
