{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module DataTypeTest where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit
import Database.Persist.Sqlite
import Database.Persist.TH
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.Time (Day, TimeOfDay (..), UTCTime (..), fromGregorian)
import RenameTest (runConn2)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO, randomRIO, Random)
import Control.Applicative ((<$>), (<*>))
import Data.Word (Word8)

-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DataTypeTable
    text Text
    bytes ByteString
    int Int
    double Double
    bool Bool
    day Day
    time TimeOfDay
    utc UTCTime
|]

dataTypeSpecs :: Specs
dataTypeSpecs = describe "data type specs" $ do
    it "handles all types" $ asIO $ runConn2 $ do
        _ <- runMigrationSilent migrateAll

        -- Ensure reading the data from the database works...
        _ <- runMigrationSilent migrateAll
        sequence_ $ replicate 1000 $ do
            x <- liftIO randomValue
            key <- insert x
            Just y <- get key
            liftIO $ do
                let check :: (Eq a, Show a) => String -> (DataTypeTable -> a) -> IO ()
                    check s f = (s, f x) @=? (s, f y)
                -- Check individual fields for better error messages
                check "text" dataTypeTableText
                check "bytes" dataTypeTableBytes
                check "int" dataTypeTableInt
                check "bool" dataTypeTableBool
                check "day" dataTypeTableDay
                check "time" dataTypeTableTime
                check "utc" dataTypeTableUtc

                -- Do a special check for Double since it may
                -- lose precision when serialized.
                when (abs (dataTypeTableDouble x - dataTypeTableDouble y) > 1e-14) $
                  check "double" dataTypeTableDouble

randomValue :: IO DataTypeTable
randomValue = DataTypeTable
    <$> (T.pack . filter (/= '\0') <$> randomIOs)
    <*> (S.pack . map intToWord8 <$> randomIOs)
    <*> randomIO
    <*> randomIO
    <*> randomIO
    <*> randomDay
    <*> randomTime
    <*> randomUTC

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
