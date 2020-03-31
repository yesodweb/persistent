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

import MyInit

import Data.Time (Day, UTCTime (..), TimeOfDay, timeToTimeOfDay, timeOfDayToTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Fixed
import Test.QuickCheck
import qualified Data.Text as T
import Data.IntMap (IntMap)
import qualified Data.ByteString as BS

import qualified CompositeTest
import qualified CustomPersistFieldTest
import qualified CustomPrimaryKeyReferenceTest
import qualified DataTypeTest
import qualified EmbedOrderTest
import qualified EmbedTest
import qualified EmptyEntityTest
import qualified EquivalentTypeTest
import qualified HtmlTest
import qualified InsertDuplicateUpdate
import qualified LargeNumberTest
import qualified MaxLenTest
import qualified MigrationColumnLengthTest
import qualified MigrationIdempotencyTest
import qualified MigrationOnlyTest
import qualified MpsNoPrefixTest
import qualified MpsCustomPrefixTest
import qualified PersistentTest
import qualified PersistUniqueTest
-- FIXME: Not used... should it be?
-- import qualified PrimaryTest
import qualified RawSqlTest
import qualified ReadWriteTest
import qualified Recursive
import qualified RenameTest
import qualified SumTypeTest
import qualified TransactionLevelTest
import qualified UniqueTest
import qualified UpsertTest
import qualified CustomConstraintTest
import qualified LongIdentifierTest

type Tuple a b = (a, b)

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
    -- For MySQL, provide extra tests for time fields with fractional seconds,
    -- since the default (used above) is to have no fractional part.  This
    -- requires the server version to be at least 5.6.4, and should be switched
    -- off for older servers by defining OLD_MYSQL.
    timeFrac TimeOfDay sqltype=TIME(6)
    utcFrac UTCTime sqltype=DATETIME(6)
|]

instance Arbitrary (DataTypeTableGeneric backend) where
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
     <*> (truncateTimeOfDay =<< arbitrary) -- timeFrac
     <*> (truncateUTCTime   =<< arbitrary) -- utcFrac

setup :: MonadUnliftIO m => Migration -> ReaderT SqlBackend m ()
setup migration = do
  printMigration migration
  _ <- runMigrationSilent migration
  pure ()

main :: IO ()
main = do
  runConn $ do
    mapM_ setup
      [ PersistentTest.testMigrate
      , PersistentTest.noPrefixMigrate
      , PersistentTest.customPrefixMigrate
      , EmbedTest.embedMigrate
      , EmbedOrderTest.embedOrderMigrate
      , LargeNumberTest.numberMigrate
      , UniqueTest.uniqueMigrate
      , MaxLenTest.maxlenMigrate
      , Recursive.recursiveMigrate
      , CompositeTest.compositeMigrate
      , PersistUniqueTest.migration
      , RenameTest.migration
      , CustomPersistFieldTest.customFieldMigrate
      , InsertDuplicateUpdate.duplicateMigrate
      , MigrationIdempotencyTest.migration
      , CustomPrimaryKeyReferenceTest.migration
      , MigrationColumnLengthTest.migration
      , TransactionLevelTest.migration
      -- , LongIdentifierTest.migration
      ]
    PersistentTest.cleanDB

  hspec $ do
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
        , TestFn "time" (roundTime . dataTypeTableTime)
        , TestFn "utc" (roundUTCTime . dataTypeTableUtc)
        , TestFn "timeFrac" (dataTypeTableTimeFrac)
        , TestFn "utcFrac" (dataTypeTableUtcFrac)
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
    CustomPersistFieldTest.specsWith db
    CustomPrimaryKeyReferenceTest.specsWith db
    InsertDuplicateUpdate.specs
    MigrationColumnLengthTest.specsWith db
    EquivalentTypeTest.specsWith db
    TransactionLevelTest.specsWith db

    MigrationIdempotencyTest.specsWith db
    CustomConstraintTest.specs db
    -- TODO: implement automatic truncation for too long foreign keys, so we can run this test.
    xdescribe "The migration for this test currently fails because of MySQL's 64 character limit for identifiers. See https://github.com/yesodweb/persistent/issues/1000 for details" $
        LongIdentifierTest.specsWith db

roundFn :: RealFrac a => a -> Integer
roundFn = round

roundTime :: TimeOfDay -> TimeOfDay
roundTime t = timeToTimeOfDay $ fromIntegral $ roundFn $ timeOfDayToTime t

roundUTCTime :: UTCTime -> UTCTime
roundUTCTime t =
    posixSecondsToUTCTime $ fromIntegral $ roundFn $ utcTimeToPOSIXSeconds t
