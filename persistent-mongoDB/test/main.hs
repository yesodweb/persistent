{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Int
import Data.Word
import Data.IntMap (IntMap)
import Control.Monad.Trans
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

import CustomPersistField

-- These tests are noops with the NoSQL flags set.
-- import qualified CompositeTest
-- import qualified CustomPrimaryKeyReferenceTest
-- import qualified InsertDuplicateUpdate

-- These modules were quite complicated. Instead of fully extracting the
-- relevant common functionality, I just copied and de-CPPed manually.
import qualified EmbedTestMongo

-- These are done.
import qualified CustomPersistFieldTest
import qualified DataTypeTest
import qualified EmbedOrderTest
import qualified EmptyEntityTest
import qualified HtmlTest
import qualified LargeNumberTest

-- This one is in progress!
import qualified MaxLenTest

-- These are TODO.
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

type Tuple = (,)

dbNoCleanup :: Action IO () -> Assertion
dbNoCleanup = db' (pure ())

share [mkPersist persistSettings, mkMigrate "htmlMigrate"] [persistLowerCase|
HtmlTable
    html Html
    deriving
|]
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

mkPersist persistSettings [persistUpperCase|
Foo sql=foo_embed_order
    bars [Bar]
    deriving Eq Show
Bar sql=bar_embed_order
    b String
    u String
    g String
    deriving Eq Show
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

mkPersist persistSettings [persistUpperCase|
EmptyEntity
|]

mkPersist persistSettings [persistUpperCase|
  Number
    intx Int
    int32 Int32
    word32 Word32
    int64 Int64
    word64 Word64
    deriving Show Eq
|]

main :: IO ()
main = do
  hspec $ do
    RenameTest.specs
    DataTypeTest.specsWith
        dbNoCleanup
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
    HtmlTest.specsWith
        (db' (deleteWhere @_ @_ @HtmlTable []))
        Nothing
        HtmlTable
        htmlTableHtml
    EmbedTestMongo.specs
    EmbedOrderTest.specsWith
        (db' (deleteWhere ([] :: [Filter Foo]) >> deleteWhere ([] :: [Filter Bar])))
        Foo
        Bar
    LargeNumberTest.specsWith
        (db' (deleteWhere ([] :: [Filter Number])))
        Number
    UniqueTest.specs
    MaxLenTest.specs
    Recursive.specs
    SumTypeTest.specs
    MigrationOnlyTest.specs
    PersistentTest.specs
    EmptyEntityTest.specsWith
        (lift . db' (deleteWhere @_ @_ @EmptyEntity []))
        Nothing
        EmptyEntity
    -- CompositeTest.specs
    PersistUniqueTest.specs
    PrimaryTest.specs
    CustomPersistFieldTest.specsWith
        dbNoCleanup
        BlogPost
    MigrationColumnLengthTest.specs
    EquivalentTypeTest.specs
    TransactionLevelTest.specs
