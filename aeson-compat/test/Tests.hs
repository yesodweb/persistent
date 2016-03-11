{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Time (Day, LocalTime)
import           Data.Version (Version)
import           Numeric.Natural (Natural)

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit

import           Data.Aeson.Compat

import           Orphans ()

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ dotColonMark
  , testGroup "Roundtrip"
    [ testProperty "Day"       $ roundtripBroken10 (undefined :: Day)
    , testProperty "LocalTime" $ roundtripBroken10 (undefined :: LocalTime)
    , testProperty "Version"   $ roundtrip (undefined :: Version)
    , testProperty "Ordering"  $ roundtrip (undefined :: Ordering)
    , testProperty "Natural"   $ roundtrip (undefined :: Natural)
    ]
  ]

------------------------------------------------------------------------------
-- Comparison (.:?) and (.:!)
------------------------------------------------------------------------------

newtype T1 = T1 (Maybe Int) deriving (Eq, Show)
newtype T2 = T2 (Maybe Int) deriving (Eq, Show)
newtype T3 = T3 (Maybe Int) deriving (Eq, Show)

instance FromJSON T1 where parseJSON = fmap T1 . withObject "T1" (.: "value")
instance FromJSON T2 where parseJSON = fmap T2 . withObject "T2" (.:? "value")
instance FromJSON T3 where parseJSON = fmap T3 . withObject "T3" (.:! "value")

dotColonMark :: TestTree
dotColonMark = testGroup "Operators" $ fmap t [
    assertEqual ".:  not-present" Nothing               (decode ex1 :: Maybe T1)
  , assertEqual ".:  42"          (Just (T1 (Just 42))) (decode ex2 :: Maybe T1)
  , assertEqual ".:  null"        (Just (T1 Nothing))   (decode ex3 :: Maybe T1)

  , assertEqual ".:? not-present" (Just (T2 (Nothing))) (decode ex1 :: Maybe T2)
  , assertEqual ".:? 42"          (Just (T2 (Just 42))) (decode ex2 :: Maybe T2)
  , assertEqual ".:? null"        (Just (T2 Nothing))   (decode ex3 :: Maybe T2)

  , assertEqual ".:! not-present" (Just (T3 (Nothing))) (decode ex1 :: Maybe T3)
  , assertEqual ".:! 42"          (Just (T3 (Just 42))) (decode ex2 :: Maybe T3)
  , assertEqual ".:! null"        Nothing               (decode ex3 :: Maybe T3)
  ]
  where ex1 = "{}"
        ex2 = "{\"value\": 42 }"
        ex3 = "{\"value\": null }"
        t   = testCase "-"

roundtrip :: (Arbitrary a, Eq a, Show a, ToJSON a, FromJSON a) => a -> a -> Property
roundtrip _ x = Right x === (eitherDecode . encode $ x)

roundtripBroken10 :: (Arbitrary a, Eq a, Show a, ToJSON a, FromJSON a) => a -> a -> Property
#if MIN_VERSION_aeson(0,10,0) && !MIN_VERSION_aeson(0,11,0)
roundtripBroken10 _ x = property $ case eitherDecode . encode $ x of
  Right y -> False && x == y  -- x and y of the same type!
  Left _  -> True 
#else
roundtripBroken10 = roundtrip
#endif
