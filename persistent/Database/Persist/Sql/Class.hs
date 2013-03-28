{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
#ifndef NO_OVERLAP
{-# LANGUAGE OverlappingInstances #-}
#endif
module Database.Persist.Sql.Class
    ( MonadSqlPersist (..)
    , RawSql (..)
    , PersistFieldSql (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Database.Persist
import Data.Monoid ((<>))
import Database.Persist.Sql.Types
import Control.Arrow ((&&&))
import Data.Text (Text, intercalate, pack)
import Data.Maybe (fromMaybe)

import Data.Monoid (Monoid)
import Control.Monad.Trans.Class (lift)

import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import Control.Monad.Trans.Reader   ( ReaderT, ask  )
import Control.Monad.Trans.Resource ( ResourceT )
import Data.Conduit.Internal (Pipe, ConduitM)

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Logger (MonadLogger)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (ZonedTime, UTCTime, TimeOfDay, Day)
import Data.Int
import Data.Word
import Data.ByteString (ByteString)
import Text.Blaze.Html (Html)
import Data.Bits (bitSize)

class (MonadIO m, MonadLogger m) => MonadSqlPersist m where
    askSqlConn :: m Connection
    default askSqlConn :: (MonadSqlPersist m, MonadTrans t, MonadLogger (t m))
                       => t m Connection
    askSqlConn = lift askSqlConn

instance (MonadIO m, MonadLogger m) => MonadSqlPersist (SqlPersistT m) where
    askSqlConn = SqlPersistT ask

#define GO(T) instance (MonadSqlPersist m) => MonadSqlPersist (T m)
#define GOX(X, T) instance (X, MonadSqlPersist m) => MonadSqlPersist (T m)
GO(LoggingT)
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(ContT r)
GO(StateT s)
GO(ResourceT)
GO(Pipe l i o u)
GO(ConduitM i o)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
#undef GO
#undef GOX

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

instance PersistEntity a => RawSql (Entity a) where
    rawSqlCols escape = ((+1) . length . entityFields &&& process) . entityDef . Just . entityVal
        where
          process ed = (:[]) $
                       intercalate ", " $
                       map ((name ed <>) . escape) $
                       (entityID ed:) $
                       map fieldDB $
                       entityFields ed
          name ed = escape (entityDB ed) <> "."

    rawSqlColCountReason a =
        case fst (rawSqlCols (error "RawSql") a) of
          1 -> "one column for an 'Entity' data type without fields"
          n -> show n ++ " columns for an 'Entity' data type"
    rawSqlProcessRow (idCol:ent) = Entity <$> fromPersistValue idCol
                                          <*> fromPersistValues ent
    rawSqlProcessRow _ = Left "RawSql (Entity a): wrong number of columns."

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
    sqlType :: Monad m => m a -> SqlType

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
instance PersistFieldSql ZonedTime where
    sqlType _ = SqlDayTimeZoned
instance PersistFieldSql a => PersistFieldSql [a] where
    sqlType _ = SqlString
instance (Ord a, PersistFieldSql a) => PersistFieldSql (S.Set a) where
    sqlType _ = SqlString
instance (PersistFieldSql a, PersistFieldSql b) => PersistFieldSql (a,b) where
    sqlType _ = SqlString
instance PersistFieldSql v => PersistFieldSql (M.Map T.Text v) where
    sqlType _ = SqlString
instance PersistFieldSql PersistValue where
    sqlType _ = SqlInt64 -- since PersistValue should only be used like this for keys, which in SQL are Int64
instance PersistFieldSql Checkmark where
    sqlType    _ = SqlBool
