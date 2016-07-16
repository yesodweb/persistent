{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module DataTypeTest (specs) where

import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Random (newQCGen)
import Database.Persist.TH
import Data.Char (generalCategory, GeneralCategory(..))
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Time (Day, UTCTime (..), fromGregorian)
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Data.IntMap (IntMap)
import Data.Fixed (Pico,Micro)

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
|]

cleanDB :: (MonadIO m, PersistQuery backend, backend ~ PersistEntityBackend DataTypeTable) => ReaderT backend m ()
cleanDB = deleteWhere ([] :: [Filter DataTypeTable])

specs :: Spec
specs = describe "data type specs" $
    it "handles all types" $ asIO $ runConn $ do
#ifndef WITH_NOSQL
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
                check "bytesTextTuple" dataTypeTableBytesTextTuple
                check "bytesMaxLen" dataTypeTableBytesMaxLen
                check "int" dataTypeTableInt
                check "intList" dataTypeTableIntList
                check "intMap" dataTypeTableIntMap
                check "bool" dataTypeTableBool
                check "day" dataTypeTableDay
#ifndef WITH_NOSQL
                check' "pico" dataTypeTablePico
                check "time" (roundTime . dataTypeTableTime)
#endif
#if !(defined(WITH_NOSQL)) || (defined(WITH_NOSQL) && defined(HIGH_PRECISION_DATE))
                check "utc" (roundUTCTime . dataTypeTableUtc)
#endif

                -- Do a special check for Double since it may
                -- lose precision when serialized.
                when (getDoubleDiff (dataTypeTableDouble x)(dataTypeTableDouble y) > 1e-14) $
                  check "double" dataTypeTableDouble
    where
      normDouble :: Double -> Double
      normDouble x | abs x > 1 = x / 10 ^ (truncate (logBase 10 (abs x)) :: Integer)
                   | otherwise = x
      getDoubleDiff x y = abs (normDouble x - normDouble y)

roundTime :: TimeOfDay -> TimeOfDay
#ifdef WITH_MYSQL
roundTime (TimeOfDay h m s) = TimeOfDay h m (fromIntegral $ truncate s)
#else
roundTime = id
#endif

roundUTCTime :: UTCTime -> UTCTime
#ifdef WITH_MYSQL
roundUTCTime (UTCTime day time) = UTCTime day (fromIntegral $ truncate time)
#else
roundUTCTime = id
#endif

randomValues :: IO [DataTypeTable]
randomValues = do
  g <- newQCGen
  return $ map (unGen arbitrary g) [0..]

instance Arbitrary DataTypeTable where
  arbitrary = DataTypeTable
     <$> arbText                -- text
     <*> (T.take 100 <$> arbText) -- textManLen
     <*> arbitrary              -- bytes
     <*> arbTuple arbitrary arbText -- bytesTextTuple
     <*> (BS.take 100 <$> arbitrary) -- bytesMaxLen
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

arbText :: Gen Text
arbText =
     T.pack
  .  filter ((`notElem` forbidden) . generalCategory)
  .  filter (<= '\xFFFF') -- only BMP
  .  filter (/= '\0')     -- no nulls
  <$> arbitrary
  where forbidden = [NotAssigned, PrivateUse]

arbTuple :: Gen a -> Gen b -> Gen (a, b)
arbTuple x y = (,) <$> x <*> y

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
