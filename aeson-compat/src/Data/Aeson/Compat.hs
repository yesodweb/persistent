{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Compat
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Compatibility notices
--
--   * 'decode' etc. work as in @aeson >=0.9@
--   * but it is generalised to work in any 'MonadThrow' (that is extra)
--   * '.:?' works as in @aeson <0.10@
--   * '.:!' works as '.:?' in @aeson ==0.10@
--   * Orphan instances 'FromJSON' 'Day' and 'FromJSON' 'LocalTime' for @aeson <0.10@
--
module Data.Aeson.Compat (
  -- * Generic decoding functions
  decode,
  decode',
  decodeStrict,
  decodeStrict',
  AesonException(..),
  -- * Either decoding functions
  eitherDecode, eitherDecode', eitherDecodeStrict, eitherDecodeStrict',
  -- * Operators
  (.:?), (.:!),
  -- * Re-exports
  -- | Original 'Data.Aeson..:?' operator is not re-exported
  module Data.Aeson,
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

#if MIN_VERSION_aeson(0,10,0)
import           Data.Monoid
#endif

import           Data.Aeson hiding
  ((.:?), decode, decode', decodeStrict, decodeStrict'
#if !MIN_VERSION_aeson (0,9,0)
  , eitherDecode, eitherDecode', eitherDecodeStrict, eitherDecodeStrict'
#endif
  )

#if !MIN_VERSION_aeson (0,9,0)
import           Data.Aeson.Parser (value, value')
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (skipSpace)
import qualified Data.Attoparsec.Lazy as L
#endif

import           Control.Monad.Catch
import           Data.Aeson.Types hiding ((.:?))
import           Data.ByteString as B
import qualified Data.Scientific as Scientific
import           Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import           Data.Text as T
import           Data.Typeable (Typeable)

#if !MIN_VERSION_aeson(0,10,0)
import           Data.Time (Day, LocalTime, formatTime)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import qualified Data.Aeson.Compat.Time as CompatTime
#endif

#if !(MIN_VERSION_aeson(0,11,0) && MIN_VERSION_base(4,8,0))
import Numeric.Natural (Natural)
#endif

#if !MIN_VERSION_aeson(0,11,0)
import Data.Version (Version, showVersion, parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S)
#endif

-- | Exception thrown by 'decode' - family of functions in this module.
newtype AesonException = AesonException String
  deriving (Show, Typeable)

instance Exception AesonException

eitherAesonExc :: (MonadThrow m) => Either String a -> m a
eitherAesonExc (Left err) = throwM (AesonException err)
eitherAesonExc (Right x)  = return x

-- | Like original 'Data.Aeson.decode' but in arbitrary 'MonadThrow'.
--
-- Parse a top-level JSON value, i.e. also strings, numbers etc.
decode :: (FromJSON a, MonadThrow m) => L.ByteString -> m a
decode = eitherAesonExc . eitherDecode

-- | Like original 'Data.Aeson.decode'' but in arbitrary 'MonadThrow'.
decode' :: (FromJSON a, MonadThrow m) => L.ByteString -> m a
decode' = eitherAesonExc . eitherDecode'

-- | Like original 'Data.Aeson.decodeStrict' but in arbitrary 'MonadThrow'.
decodeStrict :: (FromJSON a, MonadThrow m) => B.ByteString -> m a
decodeStrict = eitherAesonExc . eitherDecodeStrict

-- | Like original 'Data.Aeson.decodeStrict'' but in arbitrary 'MonadThrow'.
decodeStrict' :: (FromJSON a, MonadThrow m) => B.ByteString -> m a
decodeStrict' = eitherAesonExc . eitherDecodeStrict'

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present, or 'empty' if
-- the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
--
-- This operator is consistent in @aeson >=0.7 && <0.11@
(.:?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:? key = case H.lookup key obj of
                Nothing -> pure Nothing
                Just v  ->
#if MIN_VERSION_aeson(0,10,0)
                  modifyFailure addKeyName $ parseJSON v -- <?> Key key
  where
    addKeyName = (("failed to parse field " <> T.unpack key <> ": ") <>)
#else
                  parseJSON v
#endif
{-# INLINE (.:?) #-}

#if !MIN_VERSION_aeson(0,11,0)
-- | Like '.:?', but the resulting parser will fail,
-- if the key is present but is 'Null'.
(.:!) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:! key = case H.lookup key obj of
                Nothing -> pure Nothing
                Just v  ->
#if MIN_VERSION_aeson(0,10,0)
                  modifyFailure addKeyName $ Just <$> parseJSON v -- <?> Key key
  where
    addKeyName = (("failed to parse field " <> T.unpack key <> ": ") <>)
#else
                  Just <$> parseJSON v
#endif
{-# INLINE (.:!) #-}
#endif

#if !MIN_VERSION_aeson(0,9,0)
-- From Parser.Internal

-- | Parse a top-level JSON value followed by optional whitespace and
-- end-of-input.  See also: 'json'.
jsonEOF :: A.Parser Value
jsonEOF = value <* A.skipSpace <* A.endOfInput

-- | Parse a top-level JSON value followed by optional whitespace and
-- end-of-input.  See also: 'json''.
jsonEOF' :: A.Parser Value
jsonEOF' = value' <* A.skipSpace <* A.endOfInput

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode = eitherDecodeWith jsonEOF fromJSON
{-# INLINE eitherDecode #-}

-- | Like 'decodeStrict' but returns an error message when decoding fails.
eitherDecodeStrict :: (FromJSON a) => B.ByteString -> Either String a
eitherDecodeStrict = eitherDecodeStrictWith jsonEOF fromJSON
{-# INLINE eitherDecodeStrict #-}

-- | Like 'decode'' but returns an error message when decoding fails.
eitherDecode' :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode' = eitherDecodeWith jsonEOF' fromJSON
{-# INLINE eitherDecode' #-}

-- | Like 'decodeStrict'' but returns an error message when decoding fails.
eitherDecodeStrict' :: (FromJSON a) => B.ByteString -> Either String a
eitherDecodeStrict' = eitherDecodeStrictWith jsonEOF' fromJSON
{-# INLINE eitherDecodeStrict' #-}

eitherDecodeWith :: L.Parser Value -> (Value -> Result a) -> L.ByteString
                 -> Either String a
eitherDecodeWith p to s =
    case L.parse p s of
      L.Done _ v -> case to v of
                      Success a -> Right a
                      Error msg -> Left msg
      L.Fail _ _ msg -> Left msg
{-# INLINE eitherDecodeWith #-}

eitherDecodeStrictWith :: A.Parser Value -> (Value -> Result a) -> B.ByteString
                       -> Either String a
eitherDecodeStrictWith p to s =
    case either Error to (A.parseOnly p s) of
      Success a -> Right a
      Error msg -> Left msg
{-# INLINE eitherDecodeStrictWith #-}

#endif

-----------------------------------------------------------------------
-- Instances in aeson-0.10
-----------------------------------------------------------------------

#if !MIN_VERSION_aeson(0,10,0)
instance FromJSON Day where
  parseJSON = withText "Day" (CompatTime.run CompatTime.day)

instance FromJSON LocalTime where
  parseJSON = withText "LocalTime" (CompatTime.run CompatTime.localTime)

instance ToJSON Day where
  toJSON = toJSON . T.pack . formatTime defaultTimeLocale "%F"

instance ToJSON LocalTime where
  toJSON = toJSON . T.pack . formatTime defaultTimeLocale "%FT%T%Q"
#endif

-----------------------------------------------------------------------
-- Instances in aeson-0.11
-----------------------------------------------------------------------

#if !(MIN_VERSION_aeson(0,11,1))
#if !(MIN_VERSION_aeson(0,11,0) && MIN_VERSION_base(4,8,0))
instance ToJSON Natural where
    toJSON = toJSON . toInteger
    {-# INLINE toJSON #-}

#if MIN_VERSION_aeson(0,10,0)
    toEncoding = toEncoding . toInteger
    {-# INLINE toEncoding #-}
#endif

instance FromJSON Natural where
    parseJSON = withScientific "Natural" $ \s ->
      if Scientific.coefficient s < 0
        then fail $ "Expected a Natural number but got the negative number: " ++ show s
        else pure $ truncate s
#endif
#endif

#if !MIN_VERSION_aeson(0,11,0)
instance ToJSON Version where
    toJSON = toJSON . showVersion
    {-# INLINE toJSON #-}

#if MIN_VERSION_aeson(0,10,0)
    toEncoding = toEncoding . showVersion
    {-# INLINE toEncoding #-}
#endif

instance FromJSON Version where
    {-# INLINE parseJSON #-}
    parseJSON = withText "Version" $ go . readP_to_S parseVersion . T.unpack
      where
        go [(v,[])] = return v
        go (_ : xs) = go xs
        go _        = fail $ "could not parse Version"

instance ToJSON Ordering where
  toJSON     = toJSON     . orderingToText
#if MIN_VERSION_aeson(0,10,0)
  toEncoding = toEncoding . orderingToText
#endif

orderingToText :: Ordering -> T.Text
orderingToText o = case o of
                     LT -> "LT"
                     EQ -> "EQ"
                     GT -> "GT"

instance FromJSON Ordering where
  parseJSON = withText "Ordering" $ \s ->
    case s of
      "LT" -> return LT
      "EQ" -> return EQ
      "GT" -> return GT
      _ -> fail "Parsing Ordering value failed: expected \"LT\", \"EQ\", or \"GT\""
#endif
