{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module DataTypeTest (specs) where

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen(..), choose)
import Test.QuickCheck.Instances ()
import Database.Persist.Sqlite
import Database.Persist.TH
#if defined(WITH_POSTGRESQL)
import Database.Persist.Postgresql
#elif defined(WITH_MYSQL)
import Database.Persist.MySQL
#endif
import Data.Char (generalCategory, GeneralCategory(..))
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Time (Day, UTCTime (..), ZonedTime (..), minutesToTimeZone, TimeOfDay)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.LocalTime
import System.Random (newStdGen)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, forM_)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Fixed (Pico,Micro)

import Init

#ifdef WITH_MONGODB
mkPersist persistSettings [persistUpperCase|
#else
-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "dataTypeMigrate"] [persistLowerCase|
#endif
DataTypeTable no-json
    text Text
    textMaxLen Text maxlen=100
    bytes ByteString
    bytesMaxLen ByteString maxlen=100
    int Int
    double Double
    bool Bool
    day Day
#ifndef WITH_MONGODB
    pico Pico
    time TimeOfDay
#endif
    utc UTCTime
    zonedTime ZonedTime
|]

cleanDB :: (PersistQuery m, PersistMonadBackend m ~ PersistEntityBackend DataTypeTable) => m ()
cleanDB = do
  deleteWhere ([] :: [Filter DataTypeTable])

specs :: Spec
specs = describe "data type specs" $ do
    it "handles all types" $ asIO $ runResourceT $ runConn $ do
#ifndef WITH_MONGODB
        _ <- runMigrationSilent dataTypeMigrate
        -- Ensure reading the data from the database works...
        _ <- runMigrationSilent dataTypeMigrate
#endif
        rvals <- liftIO randomValues
        forM_ (take 1000 rvals) $ \x -> do
            key <- insert x
            Just y <- get key
            liftIO $ do
                let check :: (Eq a, Show a) => String -> (DataTypeTable -> a) -> IO ()
                    check s f = (s, f x) @=? (s, f y)
                -- Check floating-point near equality
                let check' :: String -> (DataTypeTable -> Pico) -> IO ()
                    check' s f
                        | abs (f x - f y) < 0.000001 = return ()
                        | otherwise = (s, f x) @=? (s, f y)
                -- Check individual fields for better error messages
                check "text" dataTypeTableText
                check "textMaxLen" dataTypeTableTextMaxLen
                check "bytes" dataTypeTableBytes
                check "bytesMaxLen" dataTypeTableBytesMaxLen
                check "int" dataTypeTableInt
                check "bool" dataTypeTableBool
                check "day" dataTypeTableDay
#ifndef WITH_MONGODB
                check' "pico" dataTypeTablePico
                check "time" dataTypeTableTime
#endif
#if !(defined(WITH_MONGODB)) || (defined(WITH_MONGODB) && defined(HIGH_PRECISION_DATE))
                check "utc" dataTypeTableUtc
#endif
#ifndef WITH_POSTGRESQL
                -- postgres seems to 'convert' the time to the localtimezone
                -- http://www.postgresql.org/docs/9.2/static/datatype-datetime.html#AEN5739
                -- so, this test will never pass anyhow
                check "zoned" dataTypeTableZonedTime
#endif

                -- Do a special check for Double since it may
                -- lose precision when serialized.
                when (abs (dataTypeTableDouble x - dataTypeTableDouble y) > 1e-14) $
                  check "double" dataTypeTableDouble

randomValues :: IO [DataTypeTable]
randomValues = do
  g <- newStdGen
  return $ map ((unGen arbitrary) g) [0..]

instance Arbitrary (DataTypeTableGeneric g) where
  arbitrary = DataTypeTable
     <$> arbText                -- text
     <*> arbText                -- textManLen
     <*> arbitrary              -- bytes
     <*> arbitrary              -- bytesMaxLen
     <*> arbitrary              -- int
     <*> arbitrary              -- double
     <*> arbitrary              -- bool
     <*> arbitrary              -- day
#ifndef WITH_MONGODB
     <*> arbitrary              -- pico
     <*> (truncateTimeOfDay =<< arbitrary) -- time
#endif
     <*> (truncateUTCTime   =<< arbitrary) -- utc
     <*> (truncateToMicroZonedTime =<< arbitraryZT)  -- zonedTime

arbText :: Gen Text
arbText =
     T.pack
  .  filter ((`notElem` forbidden) . generalCategory)
  .  filter (<= '\xFFFF') -- only BMP
  .  filter (/= '\0')     -- no nulls
  <$> arbitrary
  where forbidden = [NotAssigned, PrivateUse]

arbitraryZT :: Gen ZonedTime
arbitraryZT = do
    tod <- arbitrary
    -- this avoids a crash in PostgreSQL, due to a limitation of
    -- Postgresql-simple. However, the test is still disabled on
    -- this DB because it 'adapts' the time and timezone.
    d   <- fmap (addDays 19000) arbitrary
    halfHours <- choose (-23, 23)
    let minutes = halfHours * 30
        tz = minutesToTimeZone minutes
    return $ ZonedTime (LocalTime d tod) tz

-- truncate less significant digits
truncateToMicro :: Pico -> Pico
truncateToMicro p = let
  p' = fromRational . toRational $ p  :: Micro
  in   fromRational . toRational $ p' :: Pico

truncateToMicroZonedTime :: ZonedTime  -> Gen ZonedTime
truncateToMicroZonedTime (ZonedTime (LocalTime d (TimeOfDay h m s)) tz) = do
  return $ ZonedTime (LocalTime d (TimeOfDay h m (truncateToMicro s))) tz

truncateTimeOfDay :: TimeOfDay -> Gen TimeOfDay
truncateTimeOfDay (TimeOfDay h m s) =
  return $ TimeOfDay h m $ truncateToMicro s

truncateUTCTime :: UTCTime -> Gen UTCTime
truncateUTCTime (UTCTime d dift) = do
  let pico = fromRational . toRational $ dift :: Pico
      picoi= truncate . (*1000000000000) . toRational $ truncateToMicro pico :: Integer
  return $ UTCTime d $ picosecondsToDiffTime $ picoi

asIO :: IO a -> IO a
asIO = id

instance Eq ZonedTime where
    a == b = show a == show b
