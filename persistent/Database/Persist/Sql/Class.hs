{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators, FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Database.Persist.Sql.Class
    ( RawSql (..)
    , PersistFieldSql (..)
    , EntityWithPrefix(..)
    , unPrefix
    ) where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Bits (bitSizeMaybe)
import Data.ByteString (ByteString)
import Data.Fixed
import Data.Int
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import qualified Data.Set as S
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime, TimeOfDay, Day)
import qualified Data.Vector as V
import Data.Word
import Numeric.Natural (Natural)
import Text.Blaze.Html (Html)
import GHC.TypeLits

import Database.Persist
import Database.Persist.Sql.Types


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

instance
    (PersistEntity a, PersistEntityBackend a ~ backend, IsPersistBackend backend) =>
    RawSql (Key a) where
    rawSqlCols _ key         = (length $ keyToValues key, [])
    rawSqlColCountReason key = "The primary key is composed of "
                               ++ (show $ length $ keyToValues key)
                               ++ " columns"
    rawSqlProcessRow         = keyFromValues

instance
    (PersistEntity record, PersistEntityBackend record ~ backend, IsPersistBackend backend) =>
    RawSql (Entity record) where
    rawSqlCols escape _ent = (length sqlFields, [intercalate ", " sqlFields])
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
-- myQuery = map (unPrefix @\"p\") <$> rawSql query []
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
  => RawSql (EntityWithPrefix prefix record) where
    rawSqlCols escape _ent = (length sqlFields, [intercalate ", " sqlFields])
        where
          sqlFields = map (((name <> ".") <>) . escape)
              $ map fieldDB
              -- Hacky for a composite key because
              -- it selects the same field multiple times
              $ entityKeyFields entDef ++ entityFields entDef
          name = pack $ symbolVal (Proxy :: Proxy prefix)
          entDef = entityDef (Nothing :: Maybe record)
    rawSqlColCountReason a =
        case fst (rawSqlCols (error "RawSql") a) of
          1 -> "one column for an 'Entity' data type without fields"
          n -> show n ++ " columns for an 'Entity' data type"
    rawSqlProcessRow row = case splitAt nKeyFields row of
      (rowKey, rowVal) -> fmap EntityWithPrefix $ Entity <$> keyFromValues rowKey
                                 <*> fromPersistValues rowVal
      where
        nKeyFields = length $ entityKeyFields entDef
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

-- | @since 2.10.2
instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e, RawSql f,
          RawSql g, RawSql h, RawSql i)
       => RawSql (a, b, c, d, e, f, g, h, i) where
    rawSqlCols e         = rawSqlCols e         . from9
    rawSqlColCountReason = rawSqlColCountReason . from9
    rawSqlProcessRow     = fmap to9 . rawSqlProcessRow

-- | @since 2.10.2
from9 :: (a,b,c,d,e,f,g,h,i) -> ((a,b),(c,d),(e,f),(g,h),i)
from9 (a,b,c,d,e,f,g,h,i) = ((a,b),(c,d),(e,f),(g,h),i)

-- | @since 2.10.2
to9 :: ((a,b),(c,d),(e,f),(g,h),i) -> (a,b,c,d,e,f,g,h,i)
to9 ((a,b),(c,d),(e,f),(g,h),i) = (a,b,c,d,e,f,g,h,i)

-- | @since 2.10.2
instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e, RawSql f,
          RawSql g, RawSql h, RawSql i,
          RawSql j)
       => RawSql (a, b, c, d, e, f, g, h, i, j) where
    rawSqlCols e         = rawSqlCols e         . from10
    rawSqlColCountReason = rawSqlColCountReason . from10
    rawSqlProcessRow     = fmap to10 . rawSqlProcessRow

-- | @since 2.10.2
from10 :: (a,b,c,d,e,f,g,h,i,j) -> ((a,b),(c,d),(e,f),(g,h),(i,j))
from10 (a,b,c,d,e,f,g,h,i,j) = ((a,b),(c,d),(e,f),(g,h),(i,j))

-- | @since 2.10.2
to10 :: ((a,b),(c,d),(e,f),(g,h),(i,j)) -> (a,b,c,d,e,f,g,h,i,j)
to10 ((a,b),(c,d),(e,f),(g,h),(i,j)) = (a,b,c,d,e,f,g,h,i,j)

-- | @since 2.10.2
instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e, RawSql f,
          RawSql g, RawSql h, RawSql i,
          RawSql j, RawSql k)
       => RawSql (a, b, c, d, e, f, g, h, i, j, k) where
    rawSqlCols e         = rawSqlCols e         . from11
    rawSqlColCountReason = rawSqlColCountReason . from11
    rawSqlProcessRow     = fmap to11 . rawSqlProcessRow

-- | @since 2.10.2
from11 :: (a,b,c,d,e,f,g,h,i,j,k) -> ((a,b),(c,d),(e,f),(g,h),(i,j),k)
from11 (a,b,c,d,e,f,g,h,i,j,k) = ((a,b),(c,d),(e,f),(g,h),(i,j),k)

-- | @since 2.10.2
to11 :: ((a,b),(c,d),(e,f),(g,h),(i,j),k) -> (a,b,c,d,e,f,g,h,i,j,k)
to11 ((a,b),(c,d),(e,f),(g,h),(i,j),k) = (a,b,c,d,e,f,g,h,i,j,k)

-- | @since 2.10.2
instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e, RawSql f,
          RawSql g, RawSql h, RawSql i,
          RawSql j, RawSql k, RawSql l)
       => RawSql (a, b, c, d, e, f, g, h, i, j, k, l) where
    rawSqlCols e         = rawSqlCols e         . from12
    rawSqlColCountReason = rawSqlColCountReason . from12
    rawSqlProcessRow     = fmap to12 . rawSqlProcessRow

-- | @since 2.10.2
from12 :: (a,b,c,d,e,f,g,h,i,j,k,l) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l))
from12 (a,b,c,d,e,f,g,h,i,j,k,l) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l))

-- | @since 2.10.2
to12 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l)) -> (a,b,c,d,e,f,g,h,i,j,k,l)
to12 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l)) = (a,b,c,d,e,f,g,h,i,j,k,l)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    rawSqlCols e         = rawSqlCols e         . from13
    rawSqlColCountReason = rawSqlColCountReason . from13
    rawSqlProcessRow     = fmap to13 . rawSqlProcessRow

-- | @since 2.11.0
from13 :: (a,b,c,d,e,f,g,h,i,j,k,l,m) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m)
from13 (a,b,c,d,e,f,g,h,i,j,k,l,m) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m)

-- | @since 2.11.0
to13 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m) -> (a,b,c,d,e,f,g,h,i,j,k,l,m)
to13 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),m) = (a,b,c,d,e,f,g,h,i,j,k,l,m)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    rawSqlCols e         = rawSqlCols e         . from14
    rawSqlColCountReason = rawSqlColCountReason . from14
    rawSqlProcessRow     = fmap to14 . rawSqlProcessRow

-- | @since 2.11.0
from14 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n))
from14 (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n))

-- | @since 2.11.0
to14 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
to14 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    rawSqlCols e         = rawSqlCols e         . from15
    rawSqlColCountReason = rawSqlColCountReason . from15
    rawSqlProcessRow     = fmap to15 . rawSqlProcessRow

-- | @since 2.11.0
from15 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o)
from15 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o)

-- | @since 2.11.0
to15 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
to15 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),o) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
    rawSqlCols e         = rawSqlCols e         . from16
    rawSqlColCountReason = rawSqlColCountReason . from16
    rawSqlProcessRow     = fmap to16 . rawSqlProcessRow

-- | @since 2.11.0
from16 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p))
from16 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p))

-- | @since 2.11.0
to16 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
to16 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where
    rawSqlCols e         = rawSqlCols e         . from17
    rawSqlColCountReason = rawSqlColCountReason . from17
    rawSqlProcessRow     = fmap to17 . rawSqlProcessRow

-- | @since 2.11.0
from17 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),q)
from17 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),q)

-- | @since 2.11.0
to17 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),q) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
to17 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),q) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where
    rawSqlCols e         = rawSqlCols e         . from18
    rawSqlColCountReason = rawSqlColCountReason . from18
    rawSqlProcessRow     = fmap to18 . rawSqlProcessRow

-- | @since 2.11.0
from18 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r))
from18 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r))

-- | @since 2.11.0
to18 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
to18 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where
    rawSqlCols e         = rawSqlCols e         . from19
    rawSqlColCountReason = rawSqlColCountReason . from19
    rawSqlProcessRow     = fmap to19 . rawSqlProcessRow

-- | @since 2.11.0
from19 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),s)
from19 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),s)

-- | @since 2.11.0
to19 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),s) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
to19 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),s) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where
    rawSqlCols e         = rawSqlCols e         . from20
    rawSqlColCountReason = rawSqlColCountReason . from20
    rawSqlProcessRow     = fmap to20 . rawSqlProcessRow

-- | @since 2.11.0
from20 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t))
from20 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t))

-- | @since 2.11.0
to20 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
to20 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where
    rawSqlCols e         = rawSqlCols e         . from21
    rawSqlColCountReason = rawSqlColCountReason . from21
    rawSqlProcessRow     = fmap to21 . rawSqlProcessRow

-- | @since 2.11.0
from21 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),u)
from21 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),u)

-- | @since 2.11.0
to21 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),u) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)
to21 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),u) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where
    rawSqlCols e         = rawSqlCols e         . from22
    rawSqlColCountReason = rawSqlColCountReason . from22
    rawSqlProcessRow     = fmap to22 . rawSqlProcessRow

-- | @since 2.11.0
from22 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v))
from22 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v))

-- | @since 2.11.0
to22 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
to22 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where
    rawSqlCols e         = rawSqlCols e         . from23
    rawSqlColCountReason = rawSqlColCountReason . from23
    rawSqlProcessRow     = fmap to23 . rawSqlProcessRow

-- | @since 2.11.0
from23 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),w)
from23 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),w)

-- | @since 2.11.0
to23 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),w) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w)
to23 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),w) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where
    rawSqlCols e         = rawSqlCols e         . from24
    rawSqlColCountReason = rawSqlColCountReason . from24
    rawSqlProcessRow     = fmap to24 . rawSqlProcessRow

-- | @since 2.11.0
from24 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x))
from24 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x))

-- | @since 2.11.0
to24 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
to24 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) where
    rawSqlCols e         = rawSqlCols e         . from25
    rawSqlColCountReason = rawSqlColCountReason . from25
    rawSqlProcessRow     = fmap to25 . rawSqlProcessRow

-- | @since 2.11.0
from25 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),y)
from25 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),y)

-- | @since 2.11.0
to25 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),y) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y)
to25 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),y) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) where
    rawSqlCols e         = rawSqlCols e         . from26
    rawSqlColCountReason = rawSqlColCountReason . from26
    rawSqlProcessRow     = fmap to26 . rawSqlProcessRow

-- | @since 2.11.0
from26 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z))
from26 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z))

-- | @since 2.11.0
to26 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)
to26 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2) where
    rawSqlCols e         = rawSqlCols e         . from27
    rawSqlColCountReason = rawSqlColCountReason . from27
    rawSqlProcessRow     = fmap to27 . rawSqlProcessRow

-- | @since 2.11.0
from27 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),a2)
from27 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),a2)

-- | @since 2.11.0
to27 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),a2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2)
to27 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),a2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2) where
    rawSqlCols e         = rawSqlCols e         . from28
    rawSqlColCountReason = rawSqlColCountReason . from28
    rawSqlProcessRow     = fmap to28 . rawSqlProcessRow

-- | @since 2.11.0
from28 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2))
from28 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2))

-- | @since 2.11.0
to28 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2)
to28 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2) where
    rawSqlCols e         = rawSqlCols e         . from29
    rawSqlColCountReason = rawSqlColCountReason . from29
    rawSqlProcessRow     = fmap to29 . rawSqlProcessRow

-- | @since 2.11.0
from29 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),c2)
from29 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),c2)

-- | @since 2.11.0
to29 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),c2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2)
to29 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),c2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2) where
    rawSqlCols e         = rawSqlCols e         . from30
    rawSqlColCountReason = rawSqlColCountReason . from30
    rawSqlProcessRow     = fmap to30 . rawSqlProcessRow

-- | @since 2.11.0
from30 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2))
from30 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2))

-- | @since 2.11.0
to30 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2)
to30 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2) where
    rawSqlCols e         = rawSqlCols e         . from31
    rawSqlColCountReason = rawSqlColCountReason . from31
    rawSqlProcessRow     = fmap to31 . rawSqlProcessRow

-- | @since 2.11.0
from31 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),e2)
from31 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),e2)

-- | @since 2.11.0
to31 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),e2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2)
to31 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),e2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2) where
    rawSqlCols e         = rawSqlCols e         . from32
    rawSqlColCountReason = rawSqlColCountReason . from32
    rawSqlProcessRow     = fmap to32 . rawSqlProcessRow

-- | @since 2.11.0
from32 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2))
from32 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2))

-- | @since 2.11.0
to32 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2)
to32 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2) where
    rawSqlCols e         = rawSqlCols e         . from33
    rawSqlColCountReason = rawSqlColCountReason . from33
    rawSqlProcessRow     = fmap to33 . rawSqlProcessRow

-- | @since 2.11.0
from33 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),g2)
from33 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),g2)

-- | @since 2.11.0
to33 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),g2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2)
to33 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),g2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2) where
    rawSqlCols e         = rawSqlCols e         . from34
    rawSqlColCountReason = rawSqlColCountReason . from34
    rawSqlProcessRow     = fmap to34 . rawSqlProcessRow

-- | @since 2.11.0
from34 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2))
from34 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2))

-- | @since 2.11.0
to34 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2)
to34 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2) where
    rawSqlCols e         = rawSqlCols e         . from35
    rawSqlColCountReason = rawSqlColCountReason . from35
    rawSqlProcessRow     = fmap to35 . rawSqlProcessRow

-- | @since 2.11.0
from35 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),i2)
from35 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),i2)

-- | @since 2.11.0
to35 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),i2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2)
to35 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),i2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2) where
    rawSqlCols e         = rawSqlCols e         . from36
    rawSqlColCountReason = rawSqlColCountReason . from36
    rawSqlProcessRow     = fmap to36 . rawSqlProcessRow

-- | @since 2.11.0
from36 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2))
from36 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2))

-- | @since 2.11.0
to36 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2)
to36 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2) where
    rawSqlCols e         = rawSqlCols e         . from37
    rawSqlColCountReason = rawSqlColCountReason . from37
    rawSqlProcessRow     = fmap to37 . rawSqlProcessRow

-- | @since 2.11.0
from37 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),k2)
from37 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),k2)

-- | @since 2.11.0
to37 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),k2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)
to37 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),k2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2) where
    rawSqlCols e         = rawSqlCols e         . from38
    rawSqlColCountReason = rawSqlColCountReason . from38
    rawSqlProcessRow     = fmap to38 . rawSqlProcessRow

-- | @since 2.11.0
from38 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2))
from38 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2))

-- | @since 2.11.0
to38 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2)
to38 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2) where
    rawSqlCols e         = rawSqlCols e         . from39
    rawSqlColCountReason = rawSqlColCountReason . from39
    rawSqlProcessRow     = fmap to39 . rawSqlProcessRow

-- | @since 2.11.0
from39 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),m2)
from39 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),m2)

-- | @since 2.11.0
to39 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),m2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2)
to39 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),m2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2) where
    rawSqlCols e         = rawSqlCols e         . from40
    rawSqlColCountReason = rawSqlColCountReason . from40
    rawSqlProcessRow     = fmap to40 . rawSqlProcessRow

-- | @since 2.11.0
from40 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2))
from40 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2))

-- | @since 2.11.0
to40 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2)
to40 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2) where
    rawSqlCols e         = rawSqlCols e         . from41
    rawSqlColCountReason = rawSqlColCountReason . from41
    rawSqlProcessRow     = fmap to41 . rawSqlProcessRow

-- | @since 2.11.0
from41 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),o2)
from41 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),o2)

-- | @since 2.11.0
to41 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),o2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2)
to41 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),o2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2) where
    rawSqlCols e         = rawSqlCols e         . from42
    rawSqlColCountReason = rawSqlColCountReason . from42
    rawSqlProcessRow     = fmap to42 . rawSqlProcessRow

-- | @since 2.11.0
from42 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2))
from42 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2))

-- | @since 2.11.0
to42 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2)
to42 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2) where
    rawSqlCols e         = rawSqlCols e         . from43
    rawSqlColCountReason = rawSqlColCountReason . from43
    rawSqlProcessRow     = fmap to43 . rawSqlProcessRow

-- | @since 2.11.0
from43 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),q2)
from43 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),q2)

-- | @since 2.11.0
to43 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),q2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2)
to43 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),q2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2) where
    rawSqlCols e         = rawSqlCols e         . from44
    rawSqlColCountReason = rawSqlColCountReason . from44
    rawSqlProcessRow     = fmap to44 . rawSqlProcessRow

-- | @since 2.11.0
from44 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2))
from44 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2))

-- | @since 2.11.0
to44 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2)
to44 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2) where
    rawSqlCols e         = rawSqlCols e         . from45
    rawSqlColCountReason = rawSqlColCountReason . from45
    rawSqlProcessRow     = fmap to45 . rawSqlProcessRow

-- | @since 2.11.0
from45 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),s2)
from45 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),s2)

-- | @since 2.11.0
to45 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),s2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2)
to45 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),s2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2) where
    rawSqlCols e         = rawSqlCols e         . from46
    rawSqlColCountReason = rawSqlColCountReason . from46
    rawSqlProcessRow     = fmap to46 . rawSqlProcessRow

-- | @since 2.11.0
from46 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2))
from46 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2))

-- | @since 2.11.0
to46 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2)
to46 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2) where
    rawSqlCols e         = rawSqlCols e         . from47
    rawSqlColCountReason = rawSqlColCountReason . from47
    rawSqlProcessRow     = fmap to47 . rawSqlProcessRow

-- | @since 2.11.0
from47 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),u2)
from47 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),u2)

-- | @since 2.11.0
to47 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),u2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2)
to47 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),u2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2) where
    rawSqlCols e         = rawSqlCols e         . from48
    rawSqlColCountReason = rawSqlColCountReason . from48
    rawSqlProcessRow     = fmap to48 . rawSqlProcessRow

-- | @since 2.11.0
from48 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2))
from48 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2))

-- | @since 2.11.0
to48 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2)
to48 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2) where
    rawSqlCols e         = rawSqlCols e         . from49
    rawSqlColCountReason = rawSqlColCountReason . from49
    rawSqlProcessRow     = fmap to49 . rawSqlProcessRow

-- | @since 2.11.0
from49 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),w2)
from49 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),w2)

-- | @since 2.11.0
to49 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),w2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2)
to49 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),w2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2) where
    rawSqlCols e         = rawSqlCols e         . from50
    rawSqlColCountReason = rawSqlColCountReason . from50
    rawSqlProcessRow     = fmap to50 . rawSqlProcessRow

-- | @since 2.11.0
from50 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2))
from50 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2))

-- | @since 2.11.0
to50 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2)
to50 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2) where
    rawSqlCols e         = rawSqlCols e         . from51
    rawSqlColCountReason = rawSqlColCountReason . from51
    rawSqlProcessRow     = fmap to51 . rawSqlProcessRow

-- | @since 2.11.0
from51 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),y2)
from51 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),y2)

-- | @since 2.11.0
to51 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),y2) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2)
to51 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),y2) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2) where
    rawSqlCols e         = rawSqlCols e         . from52
    rawSqlColCountReason = rawSqlColCountReason . from52
    rawSqlProcessRow     = fmap to52 . rawSqlProcessRow

-- | @since 2.11.0
from52 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2))
from52 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2))

-- | @since 2.11.0
to52 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2)
to52 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2, RawSql a3)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2, a3) where
    rawSqlCols e         = rawSqlCols e         . from53
    rawSqlColCountReason = rawSqlColCountReason . from53
    rawSqlProcessRow     = fmap to53 . rawSqlProcessRow

-- | @since 2.11.0
from53 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),a3)
from53 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),a3)

-- | @since 2.11.0
to53 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),a3) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3)
to53 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),a3) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2, RawSql a3, RawSql b3)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2, a3, b3) where
    rawSqlCols e         = rawSqlCols e         . from54
    rawSqlColCountReason = rawSqlColCountReason . from54
    rawSqlProcessRow     = fmap to54 . rawSqlProcessRow

-- | @since 2.11.0
from54 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3))
from54 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3))

-- | @since 2.11.0
to54 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3)
to54 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2, RawSql a3, RawSql b3, RawSql c3)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2, a3, b3, c3) where
    rawSqlCols e         = rawSqlCols e         . from55
    rawSqlColCountReason = rawSqlColCountReason . from55
    rawSqlProcessRow     = fmap to55 . rawSqlProcessRow

-- | @since 2.11.0
from55 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),c3)
from55 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),c3)

-- | @since 2.11.0
to55 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),c3) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3)
to55 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),c3) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2, RawSql a3, RawSql b3, RawSql c3, RawSql d3)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2, a3, b3, c3, d3) where
    rawSqlCols e         = rawSqlCols e         . from56
    rawSqlColCountReason = rawSqlColCountReason . from56
    rawSqlProcessRow     = fmap to56 . rawSqlProcessRow

-- | @since 2.11.0
from56 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3))
from56 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3))

-- | @since 2.11.0
to56 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3)
to56 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2, RawSql a3, RawSql b3, RawSql c3, RawSql d3, RawSql e3)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2, a3, b3, c3, d3, e3) where
    rawSqlCols e         = rawSqlCols e         . from57
    rawSqlColCountReason = rawSqlColCountReason . from57
    rawSqlProcessRow     = fmap to57 . rawSqlProcessRow

-- | @since 2.11.0
from57 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),e3)
from57 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),e3)

-- | @since 2.11.0
to57 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),e3) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3)
to57 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),e3) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2, RawSql a3, RawSql b3, RawSql c3, RawSql d3, RawSql e3, RawSql f3)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2, a3, b3, c3, d3, e3, f3) where
    rawSqlCols e         = rawSqlCols e         . from58
    rawSqlColCountReason = rawSqlColCountReason . from58
    rawSqlProcessRow     = fmap to58 . rawSqlProcessRow

-- | @since 2.11.0
from58 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3))
from58 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3))

-- | @since 2.11.0
to58 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3)
to58 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2, RawSql a3, RawSql b3, RawSql c3, RawSql d3, RawSql e3, RawSql f3, RawSql g3)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2, a3, b3, c3, d3, e3, f3, g3) where
    rawSqlCols e         = rawSqlCols e         . from59
    rawSqlColCountReason = rawSqlColCountReason . from59
    rawSqlProcessRow     = fmap to59 . rawSqlProcessRow

-- | @since 2.11.0
from59 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),g3)
from59 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),g3)

-- | @since 2.11.0
to59 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),g3) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3)
to59 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),g3) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2, RawSql a3, RawSql b3, RawSql c3, RawSql d3, RawSql e3, RawSql f3, RawSql g3, RawSql h3)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2, a3, b3, c3, d3, e3, f3, g3, h3) where
    rawSqlCols e         = rawSqlCols e         . from60
    rawSqlColCountReason = rawSqlColCountReason . from60
    rawSqlProcessRow     = fmap to60 . rawSqlProcessRow

-- | @since 2.11.0
from60 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3))
from60 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3))

-- | @since 2.11.0
to60 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3)
to60 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2, RawSql a3, RawSql b3, RawSql c3, RawSql d3, RawSql e3, RawSql f3, RawSql g3, RawSql h3, RawSql i3)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2, a3, b3, c3, d3, e3, f3, g3, h3, i3) where
    rawSqlCols e         = rawSqlCols e         . from61
    rawSqlColCountReason = rawSqlColCountReason . from61
    rawSqlProcessRow     = fmap to61 . rawSqlProcessRow

-- | @since 2.11.0
from61 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3,i3) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3),i3)
from61 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3,i3) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3),i3)

-- | @since 2.11.0
to61 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3),i3) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3,i3)
to61 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3),i3) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3,i3)

-- | @since 2.11.0
instance (RawSql a, RawSql b, RawSql c, RawSql d, RawSql e, RawSql f, RawSql g, RawSql h, RawSql i, RawSql j, RawSql k, RawSql l, RawSql m, RawSql n, RawSql o, RawSql p, RawSql q, RawSql r, RawSql s, RawSql t, RawSql u, RawSql v, RawSql w, RawSql x, RawSql y, RawSql z, RawSql a2, RawSql b2, RawSql c2, RawSql d2, RawSql e2, RawSql f2, RawSql g2, RawSql h2, RawSql i2, RawSql j2, RawSql k2, RawSql l2, RawSql m2, RawSql n2, RawSql o2, RawSql p2, RawSql q2, RawSql r2, RawSql s2, RawSql t2, RawSql u2, RawSql v2, RawSql w2, RawSql x2, RawSql y2, RawSql z2, RawSql a3, RawSql b3, RawSql c3, RawSql d3, RawSql e3, RawSql f3, RawSql g3, RawSql h3, RawSql i3, RawSql j3)
      => RawSql (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2, w2, x2, y2, z2, a3, b3, c3, d3, e3, f3, g3, h3, i3, j3) where
    rawSqlCols e         = rawSqlCols e         . from62
    rawSqlColCountReason = rawSqlColCountReason . from62
    rawSqlProcessRow     = fmap to62 . rawSqlProcessRow

-- | @since 2.11.0
from62 :: (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3,i3,j3) -> ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3),(i3,j3))
from62 (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3,i3,j3) = ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3),(i3,j3))

-- | @since 2.11.0
to62 :: ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3),(i3,j3)) -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3,i3,j3)
to62 ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n),(o,p),(q,r),(s,t),(u,v),(w,x),(y,z),(a2,b2),(c2,d2),(e2,f2),(g2,h2),(i2,j2),(k2,l2),(m2,n2),(o2,p2),(q2,r2),(s2,t2),(u2,v2),(w2,x2),(y2,z2),(a3,b3),(c3,d3),(e3,f3),(g3,h3),(i3,j3)) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2,a3,b3,c3,d3,e3,f3,g3,h3,i3,j3)

extractMaybe :: Maybe a -> a
extractMaybe = fromMaybe (error "Database.Persist.GenericSql.extractMaybe")

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
--   'toPersistValue' = 'PersistDbSpecific' . toASCIIBytes
--   'fromPersistValue' ('PersistDbSpecific' uuid) =
--     case fromASCIIBytes uuid of
--       'Nothing' -> 'Left' $ "Model/CustomTypes.hs: Failed to deserialize a UUID; received: " <> T.pack (show uuid)
--       'Just' uuid' -> 'Right' uuid'
--   'fromPersistValue' x = Left $ "File.hs: When trying to deserialize a UUID: expected PersistDbSpecific, received: "-- >  <> T.pack (show x)
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
