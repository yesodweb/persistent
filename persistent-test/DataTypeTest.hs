{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module DataTypeTest (specs) where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
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
import Data.Time (Day, TimeOfDay (..), UTCTime (..), fromGregorian)
import System.Random (randomIO, randomRIO, Random)
import Control.Applicative ((<$>), (<*>))
import Data.Word (Word8)

import Init

#ifdef WITH_MONGODB
mkPersist persistSettings [persistLowerCase|
#else
-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "dataTypeMigrate"] [persistLowerCase|
#endif
DataTypeTable no-json
    text Text
    bytes ByteString
    intx Int
    doublex Double
    bool Bool
#ifndef WITH_MONGODB
    day Day
    time TimeOfDay
#endif
    utc UTCTime
|]

cleanDB :: PersistQuery b m => b m ()
cleanDB = do
  deleteWhere ([] :: [Filter DataTypeTable])

specs :: Specs
specs = describe "data type specs" $ do
    it "handles all types" $ asIO $ runConn $ do
#ifndef WITH_MONGODB
        _ <- runMigrationSilent dataTypeMigrate
        -- Ensure reading the data from the database works...
        _ <- runMigrationSilent dataTypeMigrate
#endif
        sequence_ $ replicate 1000 $ do
            x <- liftIO randomValue
            _ <- insert x
            return ()
            {-
            Just y <- get key
            liftIO $ do
                let check :: (Eq a, Show a) => String -> (DataTypeTable -> a) -> IO ()
                    check s f = (s, f x) @=? (s, f y)
                -- Check individual fields for better error messages
                check "text" dataTypeTableText
                check "bytes" dataTypeTableBytes
                check "int" dataTypeTableIntx
                check "bool" dataTypeTableBool
                check "day" dataTypeTableDay
                check "time" dataTypeTableTime
                check "utc" dataTypeTableUtc

                -- Do a special check for Double since it may
                -- lose precision when serialized.
                when (abs (dataTypeTableDoublex x - dataTypeTableDoublex y) > 1e-14) $
                  check "double" dataTypeTableDoublex
                  -}

randomValue :: IO DataTypeTable
randomValue = DataTypeTable
    <$> (T.pack
              . filter ((`notElem` forbidden) . generalCategory)
              . filter (<= '\xFFFF') -- only BMP
              . filter (/= '\0')     -- no nulls
         <$> randomIOs)
    <*> (S.pack . map intToWord8 <$> randomIOs)
    <*> randomIO
    <*> randomIO
    <*> randomIO
#ifndef WITH_MONGODB
    <*> randomDay
    <*> randomTime
#endif
    <*> randomUTC
    where forbidden = [NotAssigned, PrivateUse]

asIO :: IO a -> IO a
asIO = id

intToWord8 :: Int -> Word8
intToWord8 i = fromIntegral $ mod i 256

randomIOs :: Random a => IO [a]
randomIOs = do
    len <- randomRIO (0, 20)
    sequence $ replicate len randomIO

randomDay :: IO Day
randomDay = fromGregorian <$> randomRIO (1900, 9400) <*> randomIO <*> randomIO

randomUTC :: IO UTCTime
randomUTC = UTCTime <$> randomDay <*> return 0 -- precision issues

randomTime :: IO TimeOfDay
randomTime = TimeOfDay <$> randomRIO (0, 23)
                       <*> randomRIO (0, 59)
                       <*> return 0
