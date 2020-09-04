{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}

module ChronosPersistent where

import qualified Data.Text as T
import Data.String.Conversions (cs)
import qualified Data.Attoparsec.ByteString as APBS
import Database.Persist (PersistField(..), PersistValue(..))
import Database.Persist.Sql (PersistFieldSql(..), SqlType(..))
import Chronos (OffsetDatetime(..), parserUtf8_YmdHMSz, encode_YmdHMSz, OffsetFormat(..), DatetimeFormat(..), SubsecondPrecision(..), Datetime(..), Date(..), Year(..), Month(..), DayOfMonth(..), TimeOfDay(..), Offset(..), builder_Ymd)
import qualified Data.Binary.Builder as B
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import qualified Data.Text.Lazy.Builder as DTLB
import qualified Data.Aeson.Encoding as Encoding

instance PersistField OffsetDatetime where
  toPersistValue date = PersistText $ encode_YmdHMSz OffsetFormatColonAuto SubsecondPrecisionAuto datetimeFormat date
  fromPersistValue (PersistByteString bs) = case APBS.parseOnly (parserUtf8_YmdHMSz OffsetFormatColonAuto datetimeFormat) bs of
    Left err -> Left $ "When parsing a Chronos OffsetDatetime, got error: " <> T.pack err
    Right offsetDateTime -> Right offsetDateTime
  fromPersistValue bad = Left $ "When deserializing a Chronos OffsetDatetime, expected PersistByteString but got " <> (T.pack $ show bad)

instance PersistFieldSql OffsetDatetime where
  sqlType _ = SqlOther "timestamptz"


datetimeFormat :: DatetimeFormat
datetimeFormat = DatetimeFormat
  { datetimeFormatDateSeparator = Just '-'
  , datetimeFormatSeparator = Just ' '
  , datetimeFormatTimeSeparator = Just ':'
  }

placeholderOffsetDatetime :: OffsetDatetime
placeholderOffsetDatetime = 
  let zeroDate = Date (Year 0) (Month 0) (DayOfMonth 1)
      zeroTime = TimeOfDay 0 0 0
      zeroDatetime = Datetime zeroDate zeroTime
      utcOffset = Offset 0
  in OffsetDatetime zeroDatetime utcOffset

-- instance ToJSON Date where
--   toJSON date = String $ cs $ DTLB.toLazyText $ builder_Ymd (Just '-') date
--   toEncoding date = Encoding.lazyText $ DTLB.toLazyText $ builder_Ymd (Just '-') date