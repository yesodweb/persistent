{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.Sql.Class
    ( PersistFieldSql (..)
    , EntityWithPrefix(..)
    , unPrefix
    ) where

import Data.Bits (bitSizeMaybe)
import Data.ByteString (ByteString)
import Data.Fixed
import Data.Foldable (toList)
import Data.Int
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import qualified Data.Set as S
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (Day, TimeOfDay, UTCTime)
import qualified Data.Vector as V
import Data.Word
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Text.Blaze.Html (Html)

import Database.Persist
import Database.Persist.Sql.TH
import Database.Persist.Sql.Types



instance PersistField a => RawSql (Single a) where
    rawSqlCols _ _         = (1, [])
    rawSqlColCountReason _ = "one column for a 'Single' data type"
    rawSqlProcessRow [pv]  = Single <$> fromPersistValue pv
    rawSqlProcessRow _     = Left $ pack "RawSql (Single a): wrong number of columns."

instance
    (PersistEntity a, PersistEntityBackend a ~ backend, IsPersistBackend backend) =>
    RawSql (Key a) where
    rawSqlCols _ key         = (length $ keyToValues key, [])
    rawSqlColCountReason key = "The primary key is composed of "
                               ++ (show $ length $ keyToValues key)
                               ++ " columns"
    rawSqlProcessRow         = keyFromValues

instance
    (PersistEntity record, PersistEntityBackend record ~ backend, IsPersistBackend backend)
  =>
    RawSql (Entity record)
  where
    rawSqlCols escape _ent = (length sqlFields, [intercalate ", " $ toList sqlFields])
      where
        sqlFields =
            fmap (((name <> ".") <>) . escapeWith escape)
            $ fmap fieldDB
            $ keyAndEntityFields entDef
        name =
            escapeWith escape (getEntityDBName entDef)
        entDef =
            entityDef (Nothing :: Maybe record)
    rawSqlColCountReason a =
        case fst (rawSqlCols (error "RawSql") a) of
          1 -> "one column for an 'Entity' data type without fields"
          n -> show n <> " columns for an 'Entity' data type"
    rawSqlProcessRow row =
        case keyFromRecordM of
            Just mkKey -> do
                val <- fromPersistValues row
                pure Entity
                    { entityKey =
                        mkKey val
                    , entityVal =
                        val
                    }
            Nothing ->
                case row of
                    (k : rest) ->
                        Entity
                            <$> keyFromValues [k]
                            <*> fromPersistValues rest
                    [] ->
                        Left "Row was empty"

-- | This newtype wrapper is useful when selecting an entity out of the
-- database and you want to provide a prefix to the table being selected.
--
-- Consider this raw SQL query:
--
-- > SELECT ??
-- > FROM my_long_table_name AS mltn
-- > INNER JOIN other_table AS ot
-- >    ON mltn.some_col = ot.other_col
-- > WHERE ...
--
-- We don't want to refer to @my_long_table_name@ every time, so we create
-- an alias. If we want to select it, we have to tell the raw SQL
-- quasi-quoter that we expect the entity to be prefixed with some other
-- name.
--
-- We can give the above query a type with this, like:
--
-- @
-- getStuff :: 'SqlPersistM' ['EntityWithPrefix' \"mltn\" MyLongTableName]
-- getStuff = rawSql queryText []
-- @
--
-- The 'EntityWithPrefix' bit is a boilerplate newtype wrapper, so you can
-- remove it with 'unPrefix', like this:
--
-- @
-- getStuff :: 'SqlPersistM' ['Entity' MyLongTableName]
-- getStuff = 'unPrefix' @\"mltn\" '<$>' 'rawSql' queryText []
-- @
--
-- The @ symbol is a "type application" and requires the @TypeApplications@
-- language extension.
--
-- @since 2.10.5
newtype EntityWithPrefix (prefix :: Symbol) record
    = EntityWithPrefix { unEntityWithPrefix :: Entity record }

-- | A helper function to tell GHC what the 'EntityWithPrefix' prefix
-- should be. This allows you to use a type application to specify the
-- prefix, instead of specifying the etype on the result.
--
-- As an example, here's code that uses this:
--
-- @
-- myQuery :: 'SqlPersistM' ['Entity' Person]
-- myQuery = fmap (unPrefix @\"p\") <$> rawSql query []
--   where
--     query = "SELECT ?? FROM person AS p"
-- @
--
-- @since 2.10.5
unPrefix :: forall prefix record. EntityWithPrefix prefix record -> Entity record
unPrefix = unEntityWithPrefix

instance
    ( PersistEntity record
    , KnownSymbol prefix
    , PersistEntityBackend record ~ backend
    , IsPersistBackend backend
    )
  =>
    RawSql (EntityWithPrefix prefix record)
  where
    rawSqlCols escape _ent = (length sqlFields, [intercalate ", " $ toList sqlFields])
      where
          sqlFields =
              fmap (((name <> ".") <>) . escapeWith escape)
              $ fmap fieldDB
              -- Hacky for a composite key because
              -- it selects the same field multiple times
              $ keyAndEntityFields entDef
          name =
              pack $ symbolVal (Proxy :: Proxy prefix)
          entDef =
              entityDef (Nothing :: Maybe record)
    rawSqlColCountReason a =
        case fst (rawSqlCols (error "RawSql") a) of
            1 -> "one column for an 'Entity' data type without fields"
            n -> show n ++ " columns for an 'Entity' data type"
    rawSqlProcessRow row =
        case splitAt nKeyFields row of
            (rowKey, rowVal) ->
                fmap EntityWithPrefix $
                    Entity
                        <$> keyFromValues rowKey
                        <*> fromPersistValues rowVal
      where
        nKeyFields = length $ getEntityKeyFields entDef
        entDef = entityDef (Nothing :: Maybe record)

-- | @since 1.0.1
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

extractMaybe :: Maybe a -> a
extractMaybe = fromMaybe (error "Database.Persist.GenericSql.extractMaybe")

$(generateRawSqlHelper "from" fromN)
$(generateRawSqlHelper "to" toN)
$(generateRawSqlInstances)

-- | Tells Persistent what database column type should be used to store a Haskell type.
--
-- ==== __Examples__
--
-- ===== Simple Boolean Alternative
--
-- @
-- data Switch = On | Off
--   deriving (Show, Eq)
--
-- instance 'PersistField' Switch where
--   'toPersistValue' s = case s of
--     On -> 'PersistBool' True
--     Off -> 'PersistBool' False
--   'fromPersistValue' ('PersistBool' b) = if b then 'Right' On else 'Right' Off
--   'fromPersistValue' x = Left $ "File.hs: When trying to deserialize a Switch: expected PersistBool, received: " <> T.pack (show x)
--
-- instance 'PersistFieldSql' Switch where
--   'sqlType' _ = 'SqlBool'
-- @
--
-- ===== Non-Standard Database Types
--
-- If your database supports non-standard types, such as Postgres' @uuid@, you can use 'SqlOther' to use them:
--
-- @
-- import qualified Data.UUID as UUID
-- instance 'PersistField' UUID where
--   'toPersistValue' = 'PersistLiteralEncoded' . toASCIIBytes
--   'fromPersistValue' ('PersistLiteralEncoded' uuid) =
--     case fromASCIIBytes uuid of
--       'Nothing' -> 'Left' $ "Model/CustomTypes.hs: Failed to deserialize a UUID; received: " <> T.pack (show uuid)
--       'Just' uuid' -> 'Right' uuid'
--   'fromPersistValue' x = Left $ "File.hs: When trying to deserialize a UUID: expected PersistLiteralEncoded, received: "-- >  <> T.pack (show x)
--
-- instance 'PersistFieldSql' UUID where
--   'sqlType' _ = 'SqlOther' "uuid"
-- @
--
-- ===== User Created Database Types
--
-- Similarly, some databases support creating custom types, e.g. Postgres' <https://www.postgresql.org/docs/current/static/sql-createdomain.html DOMAIN> and <https://www.postgresql.org/docs/current/static/datatype-enum.html ENUM> features. You can use 'SqlOther' to specify a custom type:
--
-- > CREATE DOMAIN ssn AS text
-- >       CHECK ( value ~ '^[0-9]{9}$');
--
-- @
-- instance 'PersistFieldSQL' SSN where
--   'sqlType' _ = 'SqlOther' "ssn"
-- @
--
-- > CREATE TYPE rainbow_color AS ENUM ('red', 'orange', 'yellow', 'green', 'blue', 'indigo', 'violet');
--
-- @
-- instance 'PersistFieldSQL' RainbowColor where
--   'sqlType' _ = 'SqlOther' "rainbow_color"
-- @
class PersistField a => PersistFieldSql a where
    sqlType :: Proxy a -> SqlType

#ifndef NO_OVERLAP
instance {-# OVERLAPPING #-} PersistFieldSql [Char] where
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
        | Just x <- bitSizeMaybe (0 :: Int), x <= 32 = SqlInt32
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
instance {-# OVERLAPPABLE #-} PersistFieldSql a => PersistFieldSql [a] where
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


-- | This type uses the 'SqlInt64' version, which will exhibit overflow and
-- underflow behavior. Additionally, it permits negative values in the
-- database, which isn't ideal.
--
-- @since 2.11.0
instance PersistFieldSql OverflowNatural where
  sqlType _ = SqlInt64

-- An embedded Entity
instance (PersistField record, PersistEntity record) => PersistFieldSql (Entity record) where
    sqlType _ = SqlString
