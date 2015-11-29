{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifndef NO_OVERLAP
{-# LANGUAGE OverlappingInstances #-}
#endif
module Database.Persist.Sql.Class
    ( RawSql (..)
    , PersistFieldSql (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Database.Persist
import Data.Monoid ((<>))
import Database.Persist.Sql.Types
import Data.Text (Text, intercalate, pack)
import Data.Maybe (fromMaybe)
import Data.Fixed
import Data.Proxy (Proxy)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.Time (UTCTime, TimeOfDay, Day)
import Data.Int
import Data.Word
import Data.ByteString (ByteString)
import Text.Blaze.Html (Html)
import Data.Bits (bitSize)
import qualified Data.Vector as V

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural (Natural)
#endif

-- | Class for data types that may be retrived from a 'rawSql'
-- query.
class RawSql a where
    -- | Number of columns that this data type needs and the list
    -- of substitutions for @SELECT@ placeholders @??@.
    rawSqlCols :: (DBName -> Text) -> a -> (Int, [Text])

    -- | A string telling the user why the column count is what
    -- it is.
    rawSqlColCountReason :: a -> String

    -- | Transform a row of the result into the data type.
    rawSqlProcessRow :: [PersistValue] -> Either Text a

instance PersistField a => RawSql (Single a) where
    rawSqlCols _ _         = (1, [])
    rawSqlColCountReason _ = "one column for a 'Single' data type"
    rawSqlProcessRow [pv]  = Single <$> fromPersistValue pv
    rawSqlProcessRow _     = Left $ pack "RawSql (Single a): wrong number of columns."

instance (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => RawSql (Key a) where
  rawSqlCols _ key         = (length $ keyToValues key, [])
  rawSqlColCountReason key = "The primary key is composed of "
                             ++ (show $ length $ keyToValues key)
                             ++ " columns"
  rawSqlProcessRow         = keyFromValues

instance (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
         => RawSql (Entity record) where
    rawSqlCols escape ent = (length sqlFields, [intercalate ", " sqlFields])
        where
          sqlFields = map (((name <> ".") <>) . escape)
              $ map fieldDB
              -- Hacky for a composite key because
              -- it selects the same field multiple times
              $ entityKeyFields entDef ++ entityFields entDef
          name = escape (entityDB entDef)
          entDef = entityDef (Nothing :: Maybe record)
    rawSqlColCountReason a =
        case fst (rawSqlCols (error "RawSql") a) of
          1 -> "one column for an 'Entity' data type without fields"
          n -> show n ++ " columns for an 'Entity' data type"
    rawSqlProcessRow row = case splitAt nKeyFields row of
      (rowKey, rowVal) -> Entity <$> keyFromValues rowKey
                                 <*> fromPersistValues rowVal
      where
        nKeyFields = length $ entityKeyFields entDef
        entDef = entityDef (Nothing :: Maybe record)

-- | Since 1.0.1.
instance RawSql a => RawSql (Maybe a) where
    rawSqlCols e = rawSqlCols e . extractMaybe
    rawSqlColCountReason = rawSqlColCountReason . extractMaybe
    rawSqlProcessRow cols
      | all isNull cols = return Nothing
      | otherwise       =
        case rawSqlProcessRow cols of
          Right v  -> Right (Just v)
          Left msg -> Left $ "RawSql (Maybe a): not all columns were Null " <>
                             "but the inner parser has failed.  Its message " <>
                             "was \"" <> msg <> "\".  Did you apply Maybe " <>
                             "to a tuple, perhaps?  The main use case for " <>
                             "Maybe is to allow OUTER JOINs to be written, " <>
                             "in which case 'Maybe (Entity v)' is used."
      where isNull PersistNull = True
            isNull _           = False

instance (RawSql a, RawSql b) => RawSql (a, b) where
    rawSqlCols e x = rawSqlCols e (fst x) # rawSqlCols e (snd x)
        where (cnta, lsta) # (cntb, lstb) = (cnta + cntb, lsta ++ lstb)
    rawSqlColCountReason x = rawSqlColCountReason (fst x) ++ ", " ++
                             rawSqlColCountReason (snd x)
    rawSqlProcessRow =
        let x = getType processRow
            getType :: (z -> Either y x) -> x
            getType = error "RawSql.getType"

            colCountFst = fst $ rawSqlCols (error "RawSql.getType2") (fst x)
            processRow row =
                let (rowFst, rowSnd) = splitAt colCountFst row
                in (,) <$> rawSqlProcessRow rowFst
                       <*> rawSqlProcessRow rowSnd

        in colCountFst `seq` processRow
           -- Avoids recalculating 'colCountFst'.

instance (RawSql a, RawSql b, RawSql c) => RawSql (a, b, c) where
    rawSqlCols e         = rawSqlCols e         . from3
    rawSqlColCountReason = rawSqlColCountReason . from3
    rawSqlProcessRow     = fmap to3 . rawSqlProcessRow

from3 :: (a,b,c) -> ((a,b),c)
from3 (a,b,c) = ((a,b),c)

to3 :: ((a,b),c) -> (a,b,c)
to3 ((a,b),c) = (a,b,c)

instance (RawSql a, RawSql b, RawSql c, RawSql d) => RawSql (a, b, c, d) where
    rawSqlCols e         = rawSqlCols e         . from4
    rawSqlColCountReason = rawSqlColCountReason . from4
    rawSqlProcessRow     = fmap to4 . rawSqlProcessRow

from4 :: (a,b,c,d) -> ((a,b),(c,d))
from4 (a,b,c,d) = ((a,b),(c,d))

to4 :: ((a,b),(c,d)) -> (a,b,c,d)
to4 ((a,b),(c,d)) = (a,b,c,d)

instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e)
       => RawSql (a, b, c, d, e) where
    rawSqlCols e         = rawSqlCols e         . from5
    rawSqlColCountReason = rawSqlColCountReason . from5
    rawSqlProcessRow     = fmap to5 . rawSqlProcessRow

from5 :: (a,b,c,d,e) -> ((a,b),(c,d),e)
from5 (a,b,c,d,e) = ((a,b),(c,d),e)

to5 :: ((a,b),(c,d),e) -> (a,b,c,d,e)
to5 ((a,b),(c,d),e) = (a,b,c,d,e)

instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e, RawSql f)
       => RawSql (a, b, c, d, e, f) where
    rawSqlCols e         = rawSqlCols e         . from6
    rawSqlColCountReason = rawSqlColCountReason . from6
    rawSqlProcessRow     = fmap to6 . rawSqlProcessRow

from6 :: (a,b,c,d,e,f) -> ((a,b),(c,d),(e,f))
from6 (a,b,c,d,e,f) = ((a,b),(c,d),(e,f))

to6 :: ((a,b),(c,d),(e,f)) -> (a,b,c,d,e,f)
to6 ((a,b),(c,d),(e,f)) = (a,b,c,d,e,f)

instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e, RawSql f,
          RawSql g)
       => RawSql (a, b, c, d, e, f, g) where
    rawSqlCols e         = rawSqlCols e         . from7
    rawSqlColCountReason = rawSqlColCountReason . from7
    rawSqlProcessRow     = fmap to7 . rawSqlProcessRow

from7 :: (a,b,c,d,e,f,g) -> ((a,b),(c,d),(e,f),g)
from7 (a,b,c,d,e,f,g) = ((a,b),(c,d),(e,f),g)

to7 :: ((a,b),(c,d),(e,f),g) -> (a,b,c,d,e,f,g)
to7 ((a,b),(c,d),(e,f),g) = (a,b,c,d,e,f,g)

instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e, RawSql f,
          RawSql g, RawSql h)
       => RawSql (a, b, c, d, e, f, g, h) where
    rawSqlCols e         = rawSqlCols e         . from8
    rawSqlColCountReason = rawSqlColCountReason . from8
    rawSqlProcessRow     = fmap to8 . rawSqlProcessRow

from8 :: (a,b,c,d,e,f,g,h) -> ((a,b),(c,d),(e,f),(g,h))
from8 (a,b,c,d,e,f,g,h) = ((a,b),(c,d),(e,f),(g,h))

to8 :: ((a,b),(c,d),(e,f),(g,h)) -> (a,b,c,d,e,f,g,h)
to8 ((a,b),(c,d),(e,f),(g,h)) = (a,b,c,d,e,f,g,h)

extractMaybe :: Maybe a -> a
extractMaybe = fromMaybe (error "Database.Persist.GenericSql.extractMaybe")

class PersistField a => PersistFieldSql a where
    sqlType :: Proxy a -> SqlType

#ifndef NO_OVERLAP
instance PersistFieldSql String where
    sqlType _ = SqlString
#endif

instance PersistFieldSql ByteString where
    sqlType _ = SqlBlob
instance PersistFieldSql T.Text where
    sqlType _ = SqlString
instance PersistFieldSql TL.Text where
    sqlType _ = SqlString
instance PersistFieldSql Html where
    sqlType _ = SqlString
instance PersistFieldSql Int where
    sqlType _
        | bitSize (0 :: Int) <= 32 = SqlInt32
        | otherwise = SqlInt64
instance PersistFieldSql Int8 where
    sqlType _ = SqlInt32
instance PersistFieldSql Int16 where
    sqlType _ = SqlInt32
instance PersistFieldSql Int32 where
    sqlType _ = SqlInt32
instance PersistFieldSql Int64 where
    sqlType _ = SqlInt64
instance PersistFieldSql Word where
    sqlType _ = SqlInt64
instance PersistFieldSql Word8 where
    sqlType _ = SqlInt32
instance PersistFieldSql Word16 where
    sqlType _ = SqlInt32
instance PersistFieldSql Word32 where
    sqlType _ = SqlInt64
instance PersistFieldSql Word64 where
    sqlType _ = SqlInt64
instance PersistFieldSql Double where
    sqlType _ = SqlReal
instance PersistFieldSql Bool where
    sqlType _ = SqlBool
instance PersistFieldSql Day where
    sqlType _ = SqlDay
instance PersistFieldSql TimeOfDay where
    sqlType _ = SqlTime
instance PersistFieldSql UTCTime where
    sqlType _ = SqlDayTime
instance PersistFieldSql a => PersistFieldSql [a] where
    sqlType _ = SqlString
instance PersistFieldSql a => PersistFieldSql (V.Vector a) where
  sqlType _ = SqlString
instance (Ord a, PersistFieldSql a) => PersistFieldSql (S.Set a) where
    sqlType _ = SqlString
instance (PersistFieldSql a, PersistFieldSql b) => PersistFieldSql (a,b) where
    sqlType _ = SqlString
instance PersistFieldSql v => PersistFieldSql (IM.IntMap v) where
    sqlType _ = SqlString
instance PersistFieldSql v => PersistFieldSql (M.Map T.Text v) where
    sqlType _ = SqlString
instance PersistFieldSql PersistValue where
    sqlType _ = SqlInt64 -- since PersistValue should only be used like this for keys, which in SQL are Int64
instance PersistFieldSql Checkmark where
    sqlType    _ = SqlBool
instance (HasResolution a) => PersistFieldSql (Fixed a) where
    sqlType a =
        SqlNumeric long prec
      where
        prec = round $ (log $ fromIntegral $ resolution n) / (log 10 :: Double) --  FIXME: May lead to problems with big numbers
        long = prec + 10                                                        --  FIXME: Is this enough ?
        n = 0
        _mn = return n `asTypeOf` a
instance PersistFieldSql Rational where
    sqlType _ = SqlNumeric 32 20   --  need to make this field big enough to handle Rational to Mumber string conversion for ODBC

#if MIN_VERSION_base(4,8,0)
instance PersistFieldSql Natural where
  sqlType _ = SqlInt64
#endif

-- An embedded Entity
instance (PersistField record, PersistEntity record) => PersistFieldSql (Entity record) where
    sqlType _ = SqlString
