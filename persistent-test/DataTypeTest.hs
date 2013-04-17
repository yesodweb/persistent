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
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
import Data.Char (generalCategory, GeneralCategory(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.Time (Day, TimeOfDay (..), UTCTime (..), fromGregorian, ZonedTime (..), LocalTime (..), TimeZone (..), minutesToTimeZone)
import System.Random (randomIO, randomRIO, Random, newStdGen)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, forM_)
import Data.Word (Word8)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Fixed (Pico)

import Init

#ifdef WITH_MONGODB
mkPersist persistSettings [persistLowerCase|
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
#ifndef WITH_MONGODB
    pico Pico
    day Day
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
#ifndef WITH_MONGODB
                check' "pico" dataTypeTablePico
                check "day" dataTypeTableDay
                check "time" dataTypeTableTime
#endif
                check "utc" dataTypeTableUtc
                check "zoned" dataTypeTableZonedTime

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
#ifndef WITH_MONGODB
     <*> arbitrary              -- pico
     <*> arbitrary              -- day
     <*> arbitrary              -- time
#endif
     <*> arbitrary              -- utc
     <*> arbitraryZT            -- zonedTime

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
    lt <- arbitrary
    halfHours <- choose (-23, 23)
    let minutes = halfHours * 30
        tz = minutesToTimeZone minutes
    return $ ZonedTime lt tz

asIO :: IO a -> IO a
asIO = id

instance Eq ZonedTime where
    a == b = show a == show b
