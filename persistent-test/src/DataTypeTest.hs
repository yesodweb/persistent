{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DataTypeTest
    ( specsWith
    , dataTypeMigrate
    , roundTime
    , roundUTCTime
    ) where

import Control.Applicative (liftA2)
import qualified Data.ByteString as BS
import Data.Fixed (Pico)
import Data.Foldable (for_)
import Data.IntMap (IntMap)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..), TimeOfDay, timeToTimeOfDay, timeOfDayToTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Random (newQCGen)

import Database.Persist.TH
import Init

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
    utc UTCTime
|]

cleanDB'
    ::
    ( MonadIO m, PersistStoreWrite (BaseBackend backend), PersistQuery backend) => ReaderT backend m ()
cleanDB' = deleteWhere ([] :: [Filter (DataTypeTableGeneric backend)])

roundFn :: RealFrac a => a -> Integer
roundFn = round

roundTime :: TimeOfDay -> TimeOfDay
roundTime t = timeToTimeOfDay $ fromIntegral $ roundFn $ timeOfDayToTime t

roundUTCTime :: UTCTime -> UTCTime
roundUTCTime t =
    posixSecondsToUTCTime $ fromIntegral $ roundFn $ utcTimeToPOSIXSeconds t

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
     <*> (truncateUTCTime   =<< arbitrary) -- utc

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
