{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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

module DataTypeTestMongo (specs) where

import Control.Applicative (liftA2)
import Database.Persist.TH
import Data.Char (generalCategory, GeneralCategory(..))
import qualified Data.ByteString as BS
import Data.Fixed (Pico,Micro)
import Data.IntMap (IntMap)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..), fromGregorian, picosecondsToDiffTime,
                  TimeOfDay (TimeOfDay), timeToTimeOfDay, timeOfDayToTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen(..), frequency, listOf, sized, resize)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Random (newQCGen)

import InitMongo

type Tuple a b = (a, b)

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

cleanDB :: (MonadIO m, PersistQuery backend, backend ~ PersistEntityBackend DataTypeTable) => ReaderT backend m ()
cleanDB = deleteWhere ([] :: [Filter DataTypeTable])

specs :: Spec
specs = describe "data type specs" $
    it "handles all types" $ asIO $ runConn $ do

        cleanDB
        rvals <- liftIO $ randomValues 1000
        forM_ rvals $ \x -> do
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
                check "bytesTextTuple" dataTypeTableBytesTextTuple
                check "bytesMaxLen" dataTypeTableBytesMaxLen
                check "int" dataTypeTableInt
                check "intList" dataTypeTableIntList
                check "intMap" dataTypeTableIntMap
                check "bool" dataTypeTableBool
                check "day" dataTypeTableDay

                -- Do a special check for Double since it may
                -- lose precision when serialized.
                when (getDoubleDiff (dataTypeTableDouble x)(dataTypeTableDouble y) > 1e-14) $
                  check "double" dataTypeTableDouble
    where
      normDouble :: Double -> Double
      normDouble x | abs x > 1 = x / 10 ^ (truncate (logBase 10 (abs x)) :: Integer)
                   | otherwise = x
      getDoubleDiff x y = abs (normDouble x - normDouble y)

roundFn :: RealFrac a => a -> Integer
roundFn = round

roundTime :: TimeOfDay -> TimeOfDay
roundTime = id

roundUTCTime :: UTCTime -> UTCTime
roundUTCTime = id

randomValues :: Int -> IO [DataTypeTable]
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
     <*> (truncateUTCTime   =<< arbitrary) -- utc

arbText :: Gen Text
arbText =
     T.pack
  .  filter ((`notElem` forbidden) . generalCategory)
  .  filter (<= '\xFFFF') -- only BMP
  .  filter (/= '\0')     -- no nulls
  .  T.unpack
  <$> arbitrary
  where forbidden = [NotAssigned, PrivateUse]

-- truncate less significant digits
truncateToMicro :: Pico -> Pico
truncateToMicro p = let
  p' = fromRational . toRational $ p  :: Micro
  in   fromRational . toRational $ p' :: Pico

truncateTimeOfDay :: TimeOfDay -> Gen TimeOfDay
truncateTimeOfDay (TimeOfDay h m s) =
  return $ TimeOfDay h m $ truncateToMicro s

truncateUTCTime :: UTCTime -> Gen UTCTime
truncateUTCTime (UTCTime d dift) = do
  let pico = fromRational . toRational $ dift :: Pico
      picoi= truncate . (*1000000000000) . toRational $ truncateToMicro pico :: Integer
      -- https://github.com/lpsmith/postgresql-simple/issues/123
      d' = max d $ fromGregorian 1950 1 1
  return $ UTCTime d' $ picosecondsToDiffTime picoi

asIO :: IO a -> IO a
asIO = id
