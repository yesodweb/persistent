{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This is a helper module for creating SQL backends. Regular users do not
-- need to use this module.
module Database.Persist.GenericSql
    ( SqlPersist (..)
    , Connection
    , ConnectionPool
    , Statement
    , runSqlConn
    , runSqlPool
    , Key

    -- * Useful data types
    , Checkmark(..)

    -- * Raw SQL queries
    -- $rawSql
    , rawSql
    , Entity(..)
    , Single(..)
    , RawSql

    -- * Migrations
    , Migration
    , parseMigration
    , parseMigration'
    , printMigration
    , getMigration
    , runMigration
    , runMigrationSilent
    , runMigrationUnsafe
    , migrate
    , commit
    , rollback
    ) where

import qualified Prelude as P
import Prelude hiding ((++), unlines, concat, show)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&))
import Database.Persist.Store
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Conduit.Pool
import Database.Persist.GenericSql.Internal
import Database.Persist.GenericSql.Migration
import qualified Database.Persist.GenericSql.Raw as R
import Database.Persist.GenericSql.Raw (SqlPersist (..))
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Control.Exception as E
import Control.Exception (throw)
import Data.Text (Text, pack, unpack, concat)
import qualified Data.Text as T
import Web.PathPieces (PathPiece (..))
import qualified Data.Text.Read
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mappend)
import Database.Persist.EntityDef
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Base (liftBase)

type ConnectionPool = Pool Connection

instance PathPiece (KeyBackend R.SqlBackend entity) where
    toPathPiece (Key (PersistInt64 i)) = toPathPiece i
    toPathPiece k = throw $ PersistInvalidField $ "Invalid Key: " ++ show k
    fromPathPiece t =
        case Data.Text.Read.signed Data.Text.Read.decimal t of
            Right (i, "") -> Just $ Key $ PersistInt64 i
            _ -> Nothing

execute' :: (MonadIO m, MonadLogger m) => Text -> [PersistValue] -> SqlPersist m ()
execute' = R.execute

-- | Get a connection from the pool, run the given action, and then return the
-- connection to the pool.
runSqlPool :: MonadBaseControl IO m => SqlPersist m a -> Pool Connection -> m a
runSqlPool r pconn = withResource pconn $ runSqlConn r

runSqlConn :: MonadBaseControl IO m => SqlPersist m a -> Connection -> m a
runSqlConn (SqlPersist r) conn = do
    let getter = R.getStmt' conn
    liftBase $ begin conn getter
    x <- onException
            (runReaderT r conn)
            (liftBase $ rollbackC conn getter)
    liftBase $ commitC conn getter
    return x

instance (C.MonadResource m, MonadLogger m) => PersistStore (SqlPersist m) where
    type PersistMonadBackend (SqlPersist m) = R.SqlBackend
    insert val = do
        conn <- SqlPersist ask
        let esql = insertSql conn (entityDB t) (map fieldDB $ entityFields t) (entityID t)
        i <-
            case esql of
                ISRSingle sql -> R.withStmt sql vals C.$$ do
                    x <- CL.head
                    case x of
                        Just [PersistInt64 i] -> return i
                        Nothing -> error $ "SQL insert did not return a result giving the generated ID"
                        Just vals' -> error $ "Invalid result from a SQL insert, got: " P.++ P.show vals'
                ISRInsertGet sql1 sql2 -> do
                    execute' sql1 vals
                    R.withStmt sql2 [] C.$$ do
                        Just [PersistInt64 i] <- CL.head
                        return i
        return $ Key $ PersistInt64 i
      where
        t = entityDef val
        vals = map toPersistValue $ toPersistFields val

    replace k val = do
        conn <- SqlPersist ask
        let t = entityDef val
        let sql = concat
                [ "UPDATE "
                , escapeName conn (entityDB t)
                , " SET "
                , T.intercalate "," (map (go conn . fieldDB) $ entityFields t)
                , " WHERE "
                , escapeName conn $ entityID t
                , "=?"
                ]
            vals = map toPersistValue (toPersistFields val) `mappend` [unKey k]
        execute' sql vals
      where
        go conn x = escapeName conn x ++ "=?"

    insertKey = insrepHelper "INSERT"

    repsert key value = do
        -- FIXME use this for sqlite insrepHelper "REPLACE"
        delete key
        insertKey key value

    get k = do
        conn <- SqlPersist ask
        let t = entityDef $ dummyFromKey k
        let cols = T.intercalate ","
                 $ map (escapeName conn . fieldDB) $ entityFields t
        let sql = concat
                [ "SELECT "
                , cols
                , " FROM "
                , escapeName conn $ entityDB t
                , " WHERE "
                , escapeName conn $ entityID t
                , "=?"
                ]
            vals' = [unKey k]
        R.withStmt sql vals' C.$$ do
            res <- CL.head
            case res of
                Nothing -> return Nothing
                Just vals ->
                    case fromPersistValues vals of
                        Left e -> error $ unpack $ "get " ++ show (unKey k) ++ ": " ++ e
                        Right v -> return $ Just v

    delete k = do
        conn <- SqlPersist ask
        execute' (sql conn) [unKey k]
      where
        t = entityDef $ dummyFromKey k
        sql conn = concat
            [ "DELETE FROM "
            , escapeName conn $ entityDB t
            , " WHERE "
            , escapeName conn $ entityID t
            , "=?"
            ]

insrepHelper :: (MonadIO m, PersistEntity val, MonadLogger m)
             => Text
             -> Key val
             -> val
             -> SqlPersist m ()
insrepHelper command (Key k) val = do
    conn <- SqlPersist ask
    execute' (sql conn) vals
  where
    t = entityDef val
    sql conn = concat
        [ command
        , " INTO "
        , escapeName conn (entityDB t)
        , "("
        , T.intercalate ","
            $ map (escapeName conn)
            $ entityID t : map fieldDB (entityFields t)
        , ") VALUES("
        , T.intercalate "," ("?" : map (const "?") (entityFields t))
        , ")"
        ]
    vals = k : map toPersistValue (toPersistFields val)

instance (C.MonadResource m, MonadLogger m) => PersistUnique (SqlPersist m) where
    deleteBy uniq = do
        conn <- SqlPersist ask
        let sql' = sql conn
            vals = persistUniqueToValues uniq
        execute' sql' vals
      where
        t = entityDef $ dummyFromUnique uniq
        go = map snd . persistUniqueToFieldNames
        go' conn x = escapeName conn x ++ "=?"
        sql conn = concat
            [ "DELETE FROM "
            , escapeName conn $ entityDB t
            , " WHERE "
            , T.intercalate " AND " $ map (go' conn) $ go uniq
            ]

    getBy uniq = do
        conn <- SqlPersist ask
        let cols = T.intercalate "," $ (escapeName conn $ entityID t)
                 : map (escapeName conn . fieldDB) (entityFields t)
        let sql = concat
                [ "SELECT "
                , cols
                , " FROM "
                , escapeName conn $ entityDB t
                , " WHERE "
                , sqlClause conn
                ]
            vals' = persistUniqueToValues uniq
        R.withStmt sql vals' C.$$ do
            row <- CL.head
            case row of
                Nothing -> return Nothing
                Just (PersistInt64 k:vals) ->
                    case fromPersistValues vals of
                        Left s -> error $ unpack s
                        Right x -> return $ Just (Entity (Key $ PersistInt64 k) x)
                Just _ -> error "Database.Persist.GenericSql: Bad list in getBy"
      where
        sqlClause conn =
            T.intercalate " AND " $ map (go conn) $ toFieldNames' uniq
        go conn x = escapeName conn x ++ "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map snd . persistUniqueToFieldNames

dummyFromKey :: KeyBackend R.SqlBackend v -> v
dummyFromKey _ = error "dummyFromKey"

dummyFromUnique :: Unique v -> v
dummyFromUnique _ = error "dummyFromUnique"

#if MIN_VERSION_monad_control(0, 3, 0)
onException :: MonadBaseControl IO m => m α -> m β -> m α
onException m what = control $ \runInIO ->
                       E.onException (runInIO m)
                                     (runInIO what)
#endif

infixr 5 ++
(++) :: Text -> Text -> Text
(++) = mappend

show :: Show a => a -> Text
show = pack . P.show


-- | A 'Checkmark' should be used as a field type whenever a
-- uniqueness constraint should guarantee that a certain kind of
-- record may appear at most once, but other kinds of records may
-- appear any number of times.
--
-- /NOTE:/ You need to mark any @Checkmark@ fields as @nullable@
-- (see the following example).
--
-- For example, suppose there's a @Location@ entity that
-- represents where a user has lived:
--
-- @
-- Location
--     user    UserId
--     name    Text
--     current Checkmark nullable
--
--     UniqueLocation user current
-- @
--
-- The @UniqueLocation@ constraint allows any number of
-- 'Inactive' @Location@s to be @current@.  However, there may be
-- at most one @current@ @Location@ per user (i.e., either zero
-- or one per user).
--
-- This data type works because of the way that SQL treats
-- @NULL@able fields within uniqueness constraints.  The SQL
-- standard says that @NULL@ values should be considered
-- different, so we represent 'Inactive' as SQL @NULL@, thus
-- allowing any number of 'Inactive' records.  On the other hand,
-- we represent 'Active' as @TRUE@, so the uniqueness constraint
-- will disallow more than one 'Active' record.
--
-- /Note:/ There may be DBMSs that do not respect the SQL
-- standard's treatment of @NULL@ values on uniqueness
-- constraints, please check if this data type works before
-- relying on it.
--
-- The SQL @BOOLEAN@ type is used because it's the smallest data
-- type available.  Note that we never use @FALSE@, just @TRUE@
-- and @NULL@.  Provides the same behavior @Maybe ()@ would if
-- @()@ was a valid 'PersistField'.
data Checkmark = Active
                 -- ^ When used on a uniqueness constraint, there
                 -- may be at most one 'Active' record.
               | Inactive
                 -- ^ When used on a uniqueness constraint, there
                 -- may be any number of 'Inactive' records.
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance PersistField Checkmark where
    toPersistValue Active   = PersistBool True
    toPersistValue Inactive = PersistNull
    fromPersistValue PersistNull         = Right Inactive
    fromPersistValue (PersistBool True)  = Right Active
    fromPersistValue (PersistBool False) =
      Left "PersistField Checkmark: found unexpected FALSE value"
    fromPersistValue other =
      Left $ "PersistField Checkmark: unknown value " ++ show other
    sqlType    _ = SqlBool
    isNullable _ = True

instance PathPiece Checkmark where
    toPathPiece = show
    fromPathPiece txt =
      case reads (T.unpack txt) of
        [(a, "")] -> Just a
        _         -> Nothing


-- $rawSql
--
-- Although it covers most of the useful cases, @persistent@'s
-- API may not be enough for some of your tasks.  May be you need
-- some complex @JOIN@ query, or a database-specific command
-- needs to be issued.
--
-- To issue raw SQL queries you could use 'R.withStmt', which
-- allows you to do anything you need.  However, its API is
-- /low-level/ and you need to parse each row yourself.  However,
-- most of your complex queries will have simple results -- some
-- of your entities and maybe a couple of derived columns.
--
-- This is where 'rawSql' comes in.  Like 'R.withStmt', you may
-- issue /any/ SQL query.  However, it does all the hard work for
-- you and automatically parses the rows of the result.  It may
-- return:
--
--   * An 'Entity', that which 'selectList' returns.
--     All of your entity's fields are
--     automatically parsed.
--
--   * A @'Single' a@, which is a single, raw column of type @a@.
--     You may use a Haskell type (such as in your entity
--     definitions), for example @Single Text@ or @Single Int@,
--     or you may get the raw column value with @Single
--     'PersistValue'@.
--
--   * A tuple combining any of these (including other tuples).
--     Using tuples allows you to return many entities in one
--     query.
--
-- The only difference between issuing SQL queries with 'rawSql'
-- and using other means is that we have an /entity selection/
-- /placeholder/, the double question mark @??@.  It /must/ be
-- used whenever you want to @SELECT@ an 'Entity' from your
-- query.  Here's a sample SQL query @sampleStmt@ that may be
-- issued:
--
-- @
-- SELECT ??, ??
-- FROM \"Person\", \"Likes\", \"Object\"
-- WHERE \"Person\".id = \"Likes\".\"personId\"
-- AND \"Object\".id = \"Likes\".\"objectId\"
-- AND \"Person\".name LIKE ?
-- @
--
-- To use that query, you could say
--
-- @
-- do results <- 'rawSql' sampleStmt [\"%Luke%\"]
--    forM_ results $
--      \\( Entity personKey person
--       , Entity objectKey object
--       ) -> do ...
-- @
--
-- Note that 'rawSql' knows how to replace the double question
-- marks @??@ because of the type of the @results@.


-- | A single column (see 'rawSql').  Any 'PersistField' may be
-- used here, including 'PersistValue' (which does not do any
-- processing).
newtype Single a = Single {unSingle :: a}
    deriving (Eq, Ord, Show, Read)


-- | Execute a raw SQL statement and return its results as a
-- list.
--
-- If you're using 'Entity'@s@ (which is quite likely), then you
-- /must/ use entity selection placeholders (double question
-- mark, @??@).  These @??@ placeholders are then replaced for
-- the names of the columns that we need for your entities.
-- You'll receive an error if you don't use the placeholders.
-- Please see the 'Entity'@s@ documentation for more details.
--
-- You may put value placeholders (question marks, @?@) in your
-- SQL query.  These placeholders are then replaced by the values
-- you pass on the second parameter, already correctly escaped.
-- You may want to use 'toPersistValue' to help you constructing
-- the placeholder values.
--
-- Since you're giving a raw SQL statement, you don't get any
-- guarantees regarding safety.  If 'rawSql' is not able to parse
-- the results of your query back, then an exception is raised.
-- However, most common problems are mitigated by using the
-- entity selection placeholder @??@, and you shouldn't see any
-- error at all if you're not using 'Single'.
rawSql :: (RawSql a, C.MonadResource m, MonadLogger m)
       => Text             -- ^ SQL statement, possibly with placeholders.
       -> [PersistValue]   -- ^ Values to fill the placeholders.
       -> SqlPersist m [a]
rawSql stmt = run
    where
      getType :: (x -> SqlPersist m [a]) -> a
      getType = undefined

      x = getType run
      process = rawSqlProcessRow

      withStmt' colSubsts params = do
            R.withStmt sql params
          where
            sql = T.concat $ makeSubsts colSubsts $ T.splitOn placeholder stmt
            placeholder = "??"
            makeSubsts (s:ss) (t:ts) = t : s : makeSubsts ss ts
            makeSubsts []     []     = []
            makeSubsts []     ts     = [T.intercalate placeholder ts]
            makeSubsts ss     []     = error (P.concat err)
                where
                  err = [ "rawsql: there are still ", P.show (length ss)
                        , "'??' placeholder substitutions to be made "
                        , "but all '??' placeholders have already been "
                        , "consumed.  Please read 'rawSql's documentation "
                        , "on how '??' placeholders work."
                        ]

      run params = do
        conn <- SqlPersist ask
        let (colCount, colSubsts) = rawSqlCols (escapeName conn) x
        withStmt' colSubsts params C.$$ firstRow colCount

      firstRow colCount = do
        mrow <- CL.head
        case mrow of
          Nothing -> return []
          Just row
              | colCount == length row -> getter mrow
              | otherwise              -> fail $ P.concat
                  [ "rawSql: wrong number of columns, got "
                  , P.show (length row), " but expected ", P.show colCount
                  , " (", rawSqlColCountReason x, ")." ]

      getter = go id
          where
            go acc Nothing = return (acc [])
            go acc (Just row) =
              case process row of
                Left err -> fail (T.unpack err)
                Right r  -> CL.head >>= go (acc . (r:))


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
    rawSqlProcessRow _     = Left "RawSql (Single a): wrong number of columns."

instance PersistEntity a => RawSql (Entity a) where
    rawSqlCols escape = ((+1) . length . entityFields &&& process) . entityDef . entityVal
        where
          process ed = (:[]) $
                       T.intercalate ", " $
                       map ((name ed ++) . escape) $
                       (entityID ed:) $
                       map fieldDB $
                       entityFields ed
          name ed = escape (entityDB ed) ++ "."

    rawSqlColCountReason a =
        case fst (rawSqlCols undefined a) of
          1 -> "one column for an 'Entity' data type without fields"
          n -> P.show n P.++ " columns for an 'Entity' data type"
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
          Left msg -> Left $ "RawSql (Maybe a): not all columns were Null " ++
                             "but the inner parser has failed.  Its message " ++
                             "was \"" ++ msg ++ "\".  Did you apply Maybe " ++
                             "to a tuple, perhaps?  The main use case for " ++
                             "Maybe is to allow OUTER JOINs to be written, " ++
                             "in which case 'Maybe (Entity v)' is used."
      where isNull PersistNull = True
            isNull _           = False

extractMaybe :: Maybe a -> a
extractMaybe = fromMaybe (error "Database.Persist.GenericSql.extractMaybe")

instance (RawSql a, RawSql b) => RawSql (a, b) where
    rawSqlCols e x = rawSqlCols e (fst x) # rawSqlCols e (snd x)
        where (cnta, lsta) # (cntb, lstb) = (cnta + cntb, lsta P.++ lstb)
    rawSqlColCountReason x = rawSqlColCountReason (fst x) P.++ ", " P.++
                             rawSqlColCountReason (snd x)
    rawSqlProcessRow =
        let x = getType processRow
            getType :: (z -> Either y x) -> x
            getType = undefined

            colCountFst = fst $ rawSqlCols undefined (fst x)
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
