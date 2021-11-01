{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Persist.Postgresql.Internal
    ( P(..)
    , PgInterval(..)
    , getGetter
    ) where

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PGFF
import qualified Database.PostgreSQL.Simple.Internal as PG
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as PS
import qualified Database.PostgreSQL.Simple.Types as PG

import qualified Blaze.ByteString.Builder.Char8 as BBB
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord)
import Data.Data (Typeable)
import Data.Fixed (Fixed(..), Pico)
import Data.Int (Int64)
import qualified Data.IntMap as I
import Data.Maybe (fromMaybe)
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (NominalDiffTime, localTimeToUTC, utc)

import Database.Persist.Sql

-- | Newtype used to avoid orphan instances for @postgresql-simple@ classes.
--
-- @since 2.13.2.0
newtype P = P { unP :: PersistValue }

instance PGTF.ToField P where
    toField (P (PersistText t))        = PGTF.toField t
    toField (P (PersistByteString bs)) = PGTF.toField (PG.Binary bs)
    toField (P (PersistInt64 i))       = PGTF.toField i
    toField (P (PersistDouble d))      = PGTF.toField d
    toField (P (PersistRational r))    = PGTF.Plain $
                                         BBB.fromString $
                                         show (fromRational r :: Pico) --  FIXME: Too Ambigous, can not select precision without information about field
    toField (P (PersistBool b))        = PGTF.toField b
    toField (P (PersistDay d))         = PGTF.toField d
    toField (P (PersistTimeOfDay t))   = PGTF.toField t
    toField (P (PersistUTCTime t))     = PGTF.toField t
    toField (P PersistNull)            = PGTF.toField PG.Null
    toField (P (PersistList l))        = PGTF.toField $ listToJSON l
    toField (P (PersistMap m))         = PGTF.toField $ mapToJSON m
    toField (P (PersistLiteral_ DbSpecific s))  = PGTF.toField (Unknown s)
    toField (P (PersistLiteral_ Unescaped l))     = PGTF.toField (UnknownLiteral l)
    toField (P (PersistLiteral_ Escaped e)) = PGTF.toField (Unknown e)
    toField (P (PersistArray a))       = PGTF.toField $ PG.PGArray $ P <$> a
    toField (P (PersistObjectId _))    =
        error "Refusing to serialize a PersistObjectId to a PostgreSQL value"

instance PGFF.FromField P where
    fromField field mdata = fmap P $ case mdata of
      -- If we try to simply decode based on oid, we will hit unexpected null
      -- errors.
      Nothing -> pure PersistNull
      data' -> getGetter (PGFF.typeOid field) field data'

newtype Unknown = Unknown { unUnknown :: ByteString }
  deriving (Eq, Show, Read, Ord)

instance PGFF.FromField Unknown where
    fromField f mdata =
      case mdata of
        Nothing  -> PGFF.returnError PGFF.UnexpectedNull f "Database.Persist.Postgresql/PGFF.FromField Unknown"
        Just dat -> return (Unknown dat)

instance PGTF.ToField Unknown where
    toField (Unknown a) = PGTF.Escape a

newtype UnknownLiteral = UnknownLiteral { unUnknownLiteral :: ByteString }
  deriving (Eq, Show, Read, Ord, Typeable)

instance PGFF.FromField UnknownLiteral where
    fromField f mdata =
      case mdata of
        Nothing  -> PGFF.returnError PGFF.UnexpectedNull f "Database.Persist.Postgresql/PGFF.FromField UnknownLiteral"
        Just dat -> return (UnknownLiteral dat)

instance PGTF.ToField UnknownLiteral where
    toField (UnknownLiteral a) = PGTF.Plain $ BB.byteString a

type Getter a = PGFF.FieldParser a

convertPV :: PGFF.FromField a => (a -> b) -> Getter b
convertPV f = (fmap f .) . PGFF.fromField

builtinGetters :: I.IntMap (Getter PersistValue)
builtinGetters = I.fromList
    [ (k PS.bool,        convertPV PersistBool)
    , (k PS.bytea,       convertPV (PersistByteString . unBinary))
    , (k PS.char,        convertPV PersistText)
    , (k PS.name,        convertPV PersistText)
    , (k PS.int8,        convertPV PersistInt64)
    , (k PS.int2,        convertPV PersistInt64)
    , (k PS.int4,        convertPV PersistInt64)
    , (k PS.text,        convertPV PersistText)
    , (k PS.xml,         convertPV (PersistByteString . unUnknown))
    , (k PS.float4,      convertPV PersistDouble)
    , (k PS.float8,      convertPV PersistDouble)
    , (k PS.money,       convertPV PersistRational)
    , (k PS.bpchar,      convertPV PersistText)
    , (k PS.varchar,     convertPV PersistText)
    , (k PS.date,        convertPV PersistDay)
    , (k PS.time,        convertPV PersistTimeOfDay)
    , (k PS.timestamp,   convertPV (PersistUTCTime. localTimeToUTC utc))
    , (k PS.timestamptz, convertPV PersistUTCTime)
    , (k PS.interval,    convertPV (PersistLiteralEscaped . pgIntervalToBs))
    , (k PS.bit,         convertPV PersistInt64)
    , (k PS.varbit,      convertPV PersistInt64)
    , (k PS.numeric,     convertPV PersistRational)
    , (k PS.void,        \_ _ -> return PersistNull)
    , (k PS.json,        convertPV (PersistByteString . unUnknown))
    , (k PS.jsonb,       convertPV (PersistByteString . unUnknown))
    , (k PS.unknown,     convertPV (PersistByteString . unUnknown))

    -- Array types: same order as above.
    -- The OIDs were taken from pg_type.
    , (1000,             listOf PersistBool)
    , (1001,             listOf (PersistByteString . unBinary))
    , (1002,             listOf PersistText)
    , (1003,             listOf PersistText)
    , (1016,             listOf PersistInt64)
    , (1005,             listOf PersistInt64)
    , (1007,             listOf PersistInt64)
    , (1009,             listOf PersistText)
    , (143,              listOf (PersistByteString . unUnknown))
    , (1021,             listOf PersistDouble)
    , (1022,             listOf PersistDouble)
    , (1023,             listOf PersistUTCTime)
    , (1024,             listOf PersistUTCTime)
    , (791,              listOf PersistRational)
    , (1014,             listOf PersistText)
    , (1015,             listOf PersistText)
    , (1182,             listOf PersistDay)
    , (1183,             listOf PersistTimeOfDay)
    , (1115,             listOf PersistUTCTime)
    , (1185,             listOf PersistUTCTime)
    , (1187,             listOf (PersistLiteralEscaped . pgIntervalToBs))
    , (1561,             listOf PersistInt64)
    , (1563,             listOf PersistInt64)
    , (1231,             listOf PersistRational)
    -- no array(void) type
    , (2951,             listOf (PersistLiteralEscaped . unUnknown))
    , (199,              listOf (PersistByteString . unUnknown))
    , (3807,             listOf (PersistByteString . unUnknown))
    -- no array(unknown) either
    ]
    where
        k (PGFF.typoid -> i) = PG.oid2int i
        -- A @listOf f@ will use a @PGArray (Maybe T)@ to convert
        -- the values to Haskell-land.  The @Maybe@ is important
        -- because the usual way of checking NULLs
        -- (c.f. withStmt') won't check for NULL inside
        -- arrays---or any other compound structure for that matter.
        listOf f = convertPV (PersistList . map (nullable f) . PG.fromPGArray)
          where nullable = maybe PersistNull

-- | Get the field parser corresponding to the given 'PG.Oid'.
--
-- For example, pass in the 'PG.Oid' of 'PS.bool', and you will get back a
-- field parser which parses boolean values in the table into 'PersistBool's.
--
-- @since 2.13.2.0
getGetter :: PG.Oid -> Getter PersistValue
getGetter oid
  = fromMaybe defaultGetter $ I.lookup (PG.oid2int oid) builtinGetters
  where defaultGetter = convertPV (PersistLiteralEscaped . unUnknown)

unBinary :: PG.Binary a -> a
unBinary (PG.Binary x) = x

-- | Represent Postgres interval using NominalDiffTime
--
-- @since 2.11.0.0
newtype PgInterval = PgInterval { getPgInterval :: NominalDiffTime }
  deriving (Eq, Show)

pgIntervalToBs :: PgInterval -> ByteString
pgIntervalToBs = toStrictByteString . show . getPgInterval

instance PGTF.ToField PgInterval where
    toField (PgInterval t) = PGTF.toField t

instance PGFF.FromField PgInterval where
    fromField f mdata =
      if PGFF.typeOid f /= PS.typoid PS.interval
        then PGFF.returnError PGFF.Incompatible f ""
        else case mdata of
          Nothing  -> PGFF.returnError PGFF.UnexpectedNull f ""
          Just dat -> case P.parseOnly (nominalDiffTime <* P.endOfInput) dat of
            Left msg  ->  PGFF.returnError PGFF.ConversionFailed f msg
            Right t   -> return $ PgInterval t

      where
        toPico :: Integer -> Pico
        toPico = MkFixed

        -- Taken from Database.PostgreSQL.Simple.Time.Internal.Parser
        twoDigits :: P.Parser Int
        twoDigits = do
          a <- P.digit
          b <- P.digit
          let c2d c = ord c .&. 15
          return $! c2d a * 10 + c2d b

        -- Taken from Database.PostgreSQL.Simple.Time.Internal.Parser
        seconds :: P.Parser Pico
        seconds = do
          real <- twoDigits
          mc <- P.peekChar
          case mc of
            Just '.' -> do
              t <- P.anyChar *> P.takeWhile1 P.isDigit
              return $! parsePicos (fromIntegral real) t
            _ -> return $! fromIntegral real
         where
          parsePicos :: Int64 -> B8.ByteString -> Pico
          parsePicos a0 t = toPico (fromIntegral (t' * 10^n))
            where n  = max 0 (12 - B8.length t)
                  t' = B8.foldl' (\a c -> 10 * a + fromIntegral (ord c .&. 15)) a0
                                 (B8.take 12 t)

        parseSign :: P.Parser Bool
        parseSign = P.choice [P.char '-' >> return True, return False]

        -- Db stores it in [-]HHH:MM:SS.[SSSS]
        -- For example, nominalDay is stored as 24:00:00
        interval :: P.Parser (Bool, Int, Int, Pico)
        interval = do
            s  <- parseSign
            h  <- P.decimal <* P.char ':'
            m  <- twoDigits <* P.char ':'
            ss <- seconds
            if m < 60 && ss <= 60
                then return (s, h, m, ss)
                else fail "Invalid interval"

        nominalDiffTime :: P.Parser NominalDiffTime
        nominalDiffTime = do
          (s, h, m, ss) <- interval
          let pico   = ss + 60 * (fromIntegral m) + 60 * 60 * (fromIntegral (abs h))
          return . fromRational . toRational $ if s then (-pico) else pico

fromPersistValueError :: Text -- ^ Haskell type, should match Haskell name exactly, e.g. "Int64"
                      -> Text -- ^ Database type(s), should appear different from Haskell name, e.g. "integer" or "INT", not "Int".
                      -> PersistValue -- ^ Incorrect value
                      -> Text -- ^ Error message
fromPersistValueError haskellType databaseType received = T.concat
    [ "Failed to parse Haskell type `"
    , haskellType
    , "`; expected "
    , databaseType
    , " from database, but received: "
    , T.pack (show received)
    , ". Potential solution: Check that your database schema matches your Persistent model definitions."
    ]

instance PersistField PgInterval where
    toPersistValue = PersistLiteralEscaped . pgIntervalToBs
    fromPersistValue (PersistLiteral_ DbSpecific bs) =
        fromPersistValue (PersistLiteralEscaped bs)
    fromPersistValue x@(PersistLiteral_ Escaped bs) =
      case P.parseOnly (P.signed P.rational <* P.char 's' <* P.endOfInput) bs of
        Left _  -> Left $ fromPersistValueError "PgInterval" "Interval" x
        Right i -> Right $ PgInterval i
    fromPersistValue x = Left $ fromPersistValueError "PgInterval" "Interval" x

instance PersistFieldSql PgInterval where
  sqlType _ = SqlOther "interval"


