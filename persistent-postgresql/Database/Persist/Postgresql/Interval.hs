{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Postgresql.Interval where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Ascii
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Database.Persist as Persist
import qualified Database.Persist.Sql as Persist
import qualified Database.PostgreSQL.Simple.Interval.Unstable as Interval
import qualified Database.PostgreSQL.Simple.ToField as Postgres

instance Persist.PersistField Interval.Interval where
  fromPersistValue persistValue = case persistValue of
    Persist.PersistLiteral_ Persist.Unescaped byteString
      | Just withoutPrefix <- Ascii.stripPrefix "interval '" byteString,
        Just withoutSuffix <- Ascii.stripSuffix "'" withoutPrefix,
        Right interval <- A.parseOnly Interval.parse withoutSuffix -> Right interval
    _ -> Left $ "invalid interval: " <> Text.pack (show persistValue)

  toPersistValue =
    Persist.PersistLiteral_ Persist.Unescaped
      . LazyByteString.toStrict
      . Builder.toLazyByteString
      . ("interval " <>)
      . Postgres.inQuotes
      . Interval.render

instance Persist.PersistFieldSql Interval.Interval where
  sqlType = const $ Persist.SqlOther "interval"
