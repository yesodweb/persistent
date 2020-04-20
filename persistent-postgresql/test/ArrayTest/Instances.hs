{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ArrayTest.Instances
  ( RoundtripTextArray(..)
  , ListHackTextArray(..)
  , IntArray(..)
  , JSONArray(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.List (sort)
import qualified Data.Text as T
import Test.Hspec.Expectations ()

import PersistentTestModels
import PgInit
import qualified Data.ByteString.Lazy as BSL
-- import qualified Data.Text.Lazy.Encoding as DTLE
-- import qualified Data.Text.Lazy as DTL

newtype RoundtripTextArray = RoundtripTextArray [Text]
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance PersistField RoundtripTextArray where
  toPersistValue (RoundtripTextArray ts) = PersistArray $ toPersistValue <$> ts
  fromPersistValue (PersistArray as) = RoundtripTextArray <$> traverse fromPersistValue as
  -- Note: While we serialized to a PersistArray, we get a PersistList when deserializing. Bug?
  -- With this next line uncommented, deserializing this will fail
  -- fromPersistValue (PersistList as) = RoundtripTextArray <$> traverse fromPersistValue as
  fromPersistValue wat = Left . T.pack $ "RoundtripTextArray: When expecting PersistArray, received: " ++ show wat

instance PersistFieldSql RoundtripTextArray where
  sqlType _ = SqlOther "text[]"


newtype ListHackTextArray = ListHackTextArray [Text]
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance PersistField ListHackTextArray where
  toPersistValue (ListHackTextArray ts) = PersistArray $ toPersistValue <$> ts
  fromPersistValue (PersistArray as) = ListHackTextArray <$> traverse fromPersistValue as
  -- Note: While we serialized to a PersistArray, we get a PersistList when deserializing. Bug?
  fromPersistValue (PersistList as) = ListHackTextArray <$> traverse fromPersistValue as
  fromPersistValue wat = Left . T.pack $ "ListHackTextArray: When expecting PersistArray, received: " ++ show wat

instance PersistFieldSql ListHackTextArray where
  sqlType _ = SqlOther "text[]"


newtype IntArray = IntArray [Int]
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance PersistField IntArray where
  toPersistValue (IntArray ts) = PersistArray $ toPersistValue <$> ts
  fromPersistValue (PersistArray as) = IntArray <$> traverse fromPersistValue as
  -- Note: While we serialized to a PersistArray, we get a PersistList when deserializing. Bug?
  fromPersistValue (PersistList as) = IntArray <$> traverse fromPersistValue as
  fromPersistValue wat = Left . T.pack $ "IntArray: When expecting PersistArray, received: " ++ show wat

instance PersistFieldSql IntArray where
  sqlType _ = SqlOther "int[]"


newtype JSONArray a = JSONArray [a]
  deriving (Show, Eq)

instance (ToJSON a, FromJSON a) => PersistField (JSONArray a) where
  -- Note: You can also serialize to PersistByteString or PersistText, and get the same error
  toPersistValue (JSONArray xs) = PersistArray $ map (PersistDbSpecific . BSL.toStrict . encode) xs
  fromPersistValue = error "todo"

-- Started writing an implmentation for this but it was really ugly, realized I didn't need it to demonstrate the error.

--   fromPersistValue (PersistList xs) = 
--     let eithers :: [Either String a]
--         eithers = map (eitherDecodeStrict . persistValueToByteString) xs

--         result :: Either String [a]
--         result = foldl checkForDecodeError (Right []) eithers

--         checkForDecodeError :: Either String [a] -> Either String a -> Either String [a]
--         checkForDecodeError accum nextValue =
--            case accum of
--              Left oldErr -> Left oldErr
--              Right xs -> case nextValue of
--                Left newErr -> Left newErr
--                Right x -> Right (x : xs)
--     in case result of
--       Left s -> Left $ T.pack $ "JSONArray: When deserializing a value, got the error: " <> s
--       Right xs -> Right $ JSONArray xs

-- persistValueToByteString :: PersistValue -> ByteString
-- persistValueToByteString (PersistByteString bs) = bs
-- persistValueToByteString (PersistDbSpecific bs) = bs
-- persistValueToByteString other = error $ "expected bytestring or db specific; got other: " <> show other

instance (ToJSON a, FromJSON a) => PersistFieldSql (JSONArray a) where
  sqlType _ = SqlOther "jsonb[]"