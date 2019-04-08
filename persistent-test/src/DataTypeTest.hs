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

module DataTypeTest
    ( specs
    , specsWith
    , dataTypeMigrate
    , roundTime
    , roundUTCTime
    ) where

import Control.Applicative (liftA2)
import Database.Persist.TH
#ifdef WITH_POSTGRESQL
import Data.Aeson (Value(..))
import Database.Persist.Postgresql.JSON
import qualified Data.HashMap.Strict as HM
#endif
import Data.Char (generalCategory, GeneralCategory(..))
import qualified Data.ByteString as BS
import Data.Fixed (Pico,Micro)
import Data.IntMap (IntMap)
import Data.Foldable (for_)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..), fromGregorian, picosecondsToDiffTime,
                  TimeOfDay (TimeOfDay), timeToTimeOfDay, timeOfDayToTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen(..), frequency, listOf, sized, resize)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Random (newQCGen)

import Init

type Tuple a b = (a, b)

#ifdef WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
-- Test lower case names
share [mkPersist persistSettings, mkMigrate "dataTypeMigrate"] [persistLowerCase|
#endif
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
#ifndef WITH_NOSQL
    pico Pico
    time TimeOfDay
#endif
    utc UTCTime
#if defined(WITH_MYSQL) && !(defined(OLD_MYSQL))
    -- For MySQL, provide extra tests for time fields with fractional seconds,
    -- since the default (used above) is to have no fractional part.  This
    -- requires the server version to be at least 5.6.4, and should be switched
    -- off for older servers by defining OLD_MYSQL.
    timeFrac TimeOfDay sqltype=TIME(6)
    utcFrac UTCTime sqltype=DATETIME(6)
#endif
#ifdef WITH_POSTGRESQL
    jsonb Value
#endif
|]

cleanDB'
    ::
    ( MonadIO m, PersistStoreWrite (BaseBackend backend), PersistQuery backend) => ReaderT backend m ()
cleanDB' = deleteWhere ([] :: [Filter (DataTypeTableGeneric backend)])

specs :: Spec
specs =
    specsWith
        runConn
#ifdef WITH_NOSQL
        Nothing
#else
        (Just (runMigrationSilent dataTypeMigrate))
#endif
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
#ifndef WITH_NOSQL
        , TestFn "time" (roundTime . dataTypeTableTime)
#endif
#if !(defined(WITH_NOSQL)) || (defined(WITH_NOSQL) && defined(HIGH_PRECISION_DATE))
        , TestFn "utc" (roundUTCTime . dataTypeTableUtc)
#endif
#if defined(WITH_MYSQL) && !(defined(OLD_MYSQL))
        , TestFn "timeFrac" (dataTypeTableTimeFrac)
        , TestFn "utcFrac" (dataTypeTableUtcFrac)
#endif
#ifdef WITH_POSTGRESQL
        , TestFn "jsonb" dataTypeTableJsonb
#endif
        ]
#ifndef WITH_NOSQL
        [ ("pico", dataTypeTablePico) ]
#endif
        dataTypeTableDouble

roundFn :: RealFrac a => a -> Integer
#ifdef OLD_MYSQL
-- At version 5.6.4, MySQL changed the method used to round values for
-- date/time types - this is the same version which added support for
-- fractional seconds in the storage type.
roundFn = truncate
#else
roundFn = round
#endif

roundTime :: TimeOfDay -> TimeOfDay
-- #ifdef WITH_MYSQL
roundTime t = timeToTimeOfDay $ fromIntegral $ roundFn $ timeOfDayToTime t
-- #else
-- roundTime = id
-- #endif

roundUTCTime :: UTCTime -> UTCTime
#ifdef WITH_MYSQL
roundUTCTime t =
    posixSecondsToUTCTime $ fromIntegral $ roundFn $ utcTimeToPOSIXSeconds t
#else
roundUTCTime = id
#endif

randomValues :: Arbitrary a => Int -> IO [a]
randomValues i = do
  gs <- replicateM i newQCGen
  return $ zipWith (unGen arbitrary) gs [0..]

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
#ifndef WITH_NOSQL
     <*> arbitrary              -- pico
     <*> (truncateTimeOfDay =<< arbitrary) -- time
#endif
     <*> (truncateUTCTime   =<< arbitrary) -- utc
#if defined(WITH_MYSQL) && !(defined(OLD_MYSQL))
     <*> (truncateTimeOfDay =<< arbitrary) -- timeFrac
     <*> (truncateUTCTime   =<< arbitrary) -- utcFrac
#endif
#ifdef WITH_POSTGRESQL
     <*> arbitrary              -- value
#endif

#ifdef WITH_POSTGRESQL
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
#endif


specsWith
    :: forall db backend m entity.
    ( db ~ ReaderT backend m
    , PersistStoreRead backend
    , PersistEntity entity
    , PersistEntityBackend entity ~ BaseBackend backend
    , Arbitrary entity
    , PersistStoreWrite backend
    , PersistStoreWrite (BaseBackend backend)
    , PersistQueryWrite (BaseBackend backend)
    , PersistQueryWrite backend
    , MonadFail m
    , MonadIO m
    )
    => (db () -> IO ())
    -- ^ DB Runner
    -> Maybe (db [Text])
    -- ^ Optional migrations to run
    -> [TestFn entity]
    -- ^ List of entity fields to test
    -> [(String, entity -> Pico)]
    -- ^ List of pico fields to test
    -> (entity -> Double)
    -> Spec
specsWith runDb mmigration checks apprxChecks doubleFn = describe "data type specs" $
    it "handles all types" $ asIO $ runDb $ do

        _ <- sequence_ mmigration
        -- Ensure reading the data from the database works...
        _ <- sequence_ mmigration
        cleanDB'
        rvals <- liftIO $ randomValues 1000
        for_ rvals $ \x -> do
            key <- insert x
            Just y <- get key
            liftIO $ do
                let check :: (Eq a, Show a) => String -> (entity -> a) -> IO ()
                    check s f = (s, f x) @=? (s, f y)
                -- Check floating-point near equality
                let check' :: (Fractional p, Show p, Real p) => String -> (entity -> p) -> IO ()
                    check' s f
                        | abs (f x - f y) < 0.000001 = return ()
                        | otherwise = (s, f x) @=? (s, f y)
                -- Check individual fields for better error messages
                for_ checks $ \(TestFn msg f) -> check msg f
                for_ apprxChecks $ \(msg, f) -> check' msg f

                -- Do a special check for Double since it may
                -- lose precision when serialized.
                when (getDoubleDiff (doubleFn x) (doubleFn y) > 1e-14) $
                    check "double" doubleFn
    where
      normDouble :: Double -> Double
      normDouble x | abs x > 1 = x / 10 ^ (truncate (logBase 10 (abs x)) :: Integer)
                   | otherwise = x
      getDoubleDiff x y = abs (normDouble x - normDouble y)
