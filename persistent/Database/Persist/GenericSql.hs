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
    , Key (..)

    -- * Raw SQL queries
    -- $rawSql
    , rawSql
    , Entity(..)
    , Single(..)

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
import Database.Persist.Store
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Pool
import Database.Persist.GenericSql.Internal
import Database.Persist.GenericSql.Migration
import qualified Database.Persist.GenericSql.Raw as R
import Database.Persist.GenericSql.Raw (SqlPersist (..))
#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import qualified Control.Exception as E
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception.Control (onException)

#define MBCIO MonadControlIO
#endif
import Control.Exception (throw)
import Data.Text (Text, pack, unpack, concat)
import qualified Data.Text as T
import Web.PathPieces (PathPiece (..))
import qualified Data.Text.Read
import Data.Monoid (Monoid, mappend)
import Database.Persist.EntityDef
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

type ConnectionPool = Pool Connection

instance PathPiece (Key SqlPersist entity) where
    toPathPiece (Key (PersistInt64 i)) = toPathPiece i
    toPathPiece k = throw $ PersistInvalidField $ "Invalid Key: " ++ show k
    fromPathPiece t =
        case Data.Text.Read.signed Data.Text.Read.decimal t of
            Right (i, "") -> Just $ Key $ PersistInt64 i
            _ -> Nothing

execute' :: MonadIO m => Text -> [PersistValue] -> SqlPersist m ()
execute' = R.execute

runSqlPool :: (MBCIO m, MonadIO m) => SqlPersist m a -> Pool Connection -> m a
runSqlPool r pconn = withPool' pconn $ runSqlConn r

runSqlConn :: (MBCIO m, MonadIO m) => SqlPersist m a -> Connection -> m a
runSqlConn (SqlPersist r) conn = do
    let getter = R.getStmt' conn
    liftIO $ begin conn getter
    x <- onException
            (runReaderT r conn)
            (liftIO $ rollbackC conn getter)
    liftIO $ commitC conn getter
    return x

instance C.ResourceIO m => PersistStore SqlPersist m where
    insert val = do
        conn <- SqlPersist ask
        let esql = insertSql conn (entityDB t) (map fieldDB $ entityFields t)
        i <-
            case esql of
                Left sql -> C.runResourceT $ R.withStmt sql vals C.$$ do
                    Just [PersistInt64 i] <- CL.head
                    return i
                Right (sql1, sql2) -> do
                    execute' sql1 vals
                    C.runResourceT $ R.withStmt sql2 [] C.$$ do
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
                , " WHERE id=?"
                ]
        execute' sql $ map toPersistValue (toPersistFields val)
                       `mappend` [unKey k]
      where
        go conn x = escapeName conn x ++ "=?"

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
                , " WHERE id=?"
                ]
        C.runResourceT $ R.withStmt sql [unKey k] C.$$ do
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
            , " WHERE id=?"
            ]

instance C.ResourceIO m => PersistUnique SqlPersist m where
    deleteBy uniq = do
        conn <- SqlPersist ask
        execute' (sql conn) $ persistUniqueToValues uniq
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
        C.runResourceT $ R.withStmt sql (persistUniqueToValues uniq) C.$$ do
            row <- CL.head
            case row of
                Nothing -> return Nothing
                Just (PersistInt64 k:vals) ->
                    case fromPersistValues vals of
                        Left s -> error $ unpack s
                        Right x -> return $ Just (Key $ PersistInt64 k, x)
                Just _ -> error "Database.Persist.GenericSql: Bad list in getBy"
      where
        sqlClause conn =
            T.intercalate " AND " $ map (go conn) $ toFieldNames' uniq
        go conn x = escapeName conn x ++ "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map snd . persistUniqueToFieldNames

dummyFromKey :: Key SqlPersist v -> v
dummyFromKey _ = error "dummyFromKey"

{- FIXME
<<<<<<< HEAD

type Sql = Text

-- Bool indicates if the Sql is safe
type CautiousMigration = [(Bool, Sql)]
allSql :: CautiousMigration -> [Sql]
allSql = map snd
unsafeSql :: CautiousMigration -> [Sql]
unsafeSql = allSql . filter fst
safeSql :: CautiousMigration -> [Sql]
safeSql = allSql . filter (not . fst)

type Migration m = WriterT [Text] (WriterT CautiousMigration m) ()

parseMigration :: Monad m => Migration m -> m (Either [Text] CautiousMigration)
parseMigration =
    liftM go . runWriterT . execWriterT
  where
    go ([], sql) = Right sql
    go (errs, _) = Left errs

-- like parseMigration, but call error or return the CautiousMigration
parseMigration' :: Monad m => Migration m -> m (CautiousMigration)
parseMigration' m = do
  x <- parseMigration m
  case x of
      Left errs -> error $ unpack $ unlines errs
      Right sql -> return sql

printMigration :: (MBCIO m, MonadIO m) => Migration (SqlPersist m) -> SqlPersist m ()
printMigration m = do
  mig <- parseMigration' m
  mapM_ (liftIO . Data.Text.IO.putStrLn . flip snoc ';') (allSql mig)

getMigration :: (MBCIO m, MonadIO m) => Migration (SqlPersist m) -> SqlPersist m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

runMigration :: (MonadIO m, MBCIO m)
             => Migration (SqlPersist m)
             -> SqlPersist m ()
runMigration m = runMigration' m False >> return ()

-- | Same as 'runMigration', but returns a list of the SQL commands executed
-- instead of printing them to stderr.
runMigrationSilent :: (MBCIO m, MonadIO m)
                   => Migration (SqlPersist m)
                   -> SqlPersist m [Text]
runMigrationSilent m = runMigration' m True

runMigration'
    :: (MBCIO m, MonadIO m)
    => Migration (SqlPersist m)
    -> Bool -- ^ is silent?
    -> SqlPersist m [Text]
runMigration' m silent = do
    mig <- parseMigration' m
    case unsafeSql mig of
        []   -> mapM (executeMigrate silent) $ safeSql mig
        errs -> error $ unpack $ concat
            [ "\n\nDatabase migration: manual intervention required.\n"
            , "The following actions are considered unsafe:\n\n"
            , unlines $ map (\s -> "    " ++ s ++ ";") $ errs
            ]

runMigrationUnsafe :: (MBCIO m, MonadIO m)
                   => Migration (SqlPersist m)
                   -> SqlPersist m ()
runMigrationUnsafe m = do
    mig <- parseMigration' m
    mapM_ (executeMigrate False) $ allSql mig

executeMigrate :: MonadIO m => Bool -> Text -> SqlPersist m Text
executeMigrate silent s = do
    unless silent $ liftIO $ hPutStrLn stderr $ "Migrating: " ++ s
    execute' s []
    return s

migrate :: (MonadIO m, MBCIO m, PersistEntity val)
        => [EntityDef]
        -> val
        -> Migration (SqlPersist m)
migrate allDefs val = do
    conn <- lift $ lift $ SqlPersist ask
    let getter = R.getStmt' conn
    res <- liftIO $ migrateSql conn allDefs getter val
    either tell (lift . tell) res

updatePersistValue :: Update v -> PersistValue
updatePersistValue (Update _ v _) = toPersistValue v

-- | Perform a database commit.
commit :: MonadIO m => SqlPersist m ()
commit = do
    conn <- SqlPersist ask
    let getter = R.getStmt' conn
    liftIO $ commitC conn getter >> begin conn getter

-- | Perform a database rollback.
rollback :: MonadIO m => SqlPersist m ()
rollback = do
    conn <- SqlPersist ask
    let getter = R.getStmt' conn
    liftIO $ rollbackC conn getter >> begin conn getter
=======
-}

dummyFromUnique :: Unique v b -> v
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
-- issue /any/ SQL query.  However, it does all the hardwork for
-- you and automatically parses the rows of the result.  It may
-- return:
--
--   * An 'Entity', which is analogous to the tuples that
--     'selectList' returns.  All of your entity's fields are
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


-- | A single column (see 'rawSql').
newtype Single a = Single {unSingle :: a}
    deriving (Eq, Ord, Show, Read)


-- | An entity taking up possibly many columns.  In your SQL
-- @SELECT@ query you should select @Entity.*@.
data Entity backend entity =
    Entity { entityKey :: Key backend entity
           , entityVal :: entity }
    deriving (Eq, Ord, Show, Read)


-- | Execute a raw SQL statement and return its results as a
-- list.
--
-- You may put placeholders (question marks, @?@) in you SQL
-- query.  These placeholders are then replaced by the values you
-- pass by argument, already correctly escaped.  You may want to
-- use 'toPersistValue' to help you constructing the placeholder
-- values.
--
-- Since you're giving a raw SQL statement, you don't get any
-- guarantees regarding safety.  If 'rawSql' is not able to parse
-- the results of your query back, then an exception is raised.
-- Common problems include swapping the order of entities and
-- swapping the order of fields of an entity.
rawSql :: (RawSql a, C.ResourceIO m) =>
          Text             -- ^ SQL statement, possibly with placeholders.
       -> [PersistValue]   -- ^ Values to fill the placeholders.
       -> SqlPersist m [a]
rawSql stmt params = C.runResourceT $ R.withStmt stmt params C.$$ firstRow
    where
      getType :: C.Sink x y [z] -> z
      getType = undefined

      firstRow = do
        mrow <- CL.head
        case mrow of
          Nothing  -> return []
          Just row ->
            let x = getType firstRow
                colCount = rawSqlColCount x
                process  = rawSqlProcessRow
            in if colCount == length row
               then getter process mrow
               else fail $ P.concat
                      [ "rawSql: wrong number of columns, got "
                      , P.show (length row), " but expected ", P.show colCount
                      , " (", rawSqlColCountReason x, ")" ]

      getter process = go id
          where
            go acc Nothing = return (acc [])
            go acc (Just row) =
              case process row of
                Left err -> fail (T.unpack err)
                Right x  -> CL.head >>= go (acc . (x:))


-- | Class for data types that may be retrived from a 'rawSql'
-- query.
class RawSql a where
    -- | Number of columns that this data type needs.
    rawSqlColCount :: a -> Int

    -- | A string telling the user why the column count is what
    -- it is.
    rawSqlColCountReason :: a -> String

    -- | Transform a row of the result into the data type.
    rawSqlProcessRow :: [PersistValue] -> Either Text a

instance PersistField a => RawSql (Single a) where
    rawSqlColCount _       = 1
    rawSqlColCountReason _ = "one column for a 'Single' data type"
    rawSqlProcessRow [pv]  = Single <$> fromPersistValue pv
    rawSqlProcessRow _     = Left "RawSql (Single a): wrong number of columns."

instance PersistEntity a => RawSql (Entity backend a) where
    rawSqlColCount = (+1) . length . entityFields . entityDef . entityVal
    rawSqlColCountReason a =
        case rawSqlColCount a of
          1 -> "one column for an 'Entity' data type without fields"
          n -> P.show n P.++ " columns for an 'Entity' data type"
    rawSqlProcessRow (idCol:ent) = Entity <$> fromPersistValue idCol
                                          <*> fromPersistValues ent
    rawSqlProcessRow _ = Left "RawSql (Entity a): wrong number of columns."

instance (RawSql a, RawSql b) => RawSql (a, b) where
    rawSqlColCount x = rawSqlColCount (fst x) + rawSqlColCount (snd x)
    rawSqlColCountReason x = rawSqlColCountReason (fst x) P.++ ", " P.++
                             rawSqlColCountReason (snd x)
    rawSqlProcessRow =
        let x = getType processRow
            getType :: (z -> Either y x) -> x
            getType = undefined

            colCountFst = rawSqlColCount (fst x)
            processRow row =
                let (rowFst, rowSnd) = splitAt colCountFst row
                in (,) <$> rawSqlProcessRow rowFst
                       <*> rawSqlProcessRow rowSnd

        in colCountFst `seq` processRow
           -- Avoids recalculating 'colCountFst'.

instance (RawSql a, RawSql b, RawSql c) => RawSql (a, b, c) where
    rawSqlColCount       = rawSqlColCount       . from3
    rawSqlColCountReason = rawSqlColCountReason . from3
    rawSqlProcessRow     = fmap to3 . rawSqlProcessRow

from3 :: (a,b,c) -> ((a,b),c)
from3 (a,b,c) = ((a,b),c)

to3 :: ((a,b),c) -> (a,b,c)
to3 ((a,b),c) = (a,b,c)

instance (RawSql a, RawSql b, RawSql c, RawSql d) => RawSql (a, b, c, d) where
    rawSqlColCount       = rawSqlColCount       . from4
    rawSqlColCountReason = rawSqlColCountReason . from4
    rawSqlProcessRow     = fmap to4 . rawSqlProcessRow

from4 :: (a,b,c,d) -> ((a,b),(c,d))
from4 (a,b,c,d) = ((a,b),(c,d))

to4 :: ((a,b),(c,d)) -> (a,b,c,d)
to4 ((a,b),(c,d)) = (a,b,c,d)

instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e)
       => RawSql (a, b, c, d, e) where
    rawSqlColCount       = rawSqlColCount       . from5
    rawSqlColCountReason = rawSqlColCountReason . from5
    rawSqlProcessRow     = fmap to5 . rawSqlProcessRow

from5 :: (a,b,c,d,e) -> ((a,b),(c,d),e)
from5 (a,b,c,d,e) = ((a,b),(c,d),e)

to5 :: ((a,b),(c,d),e) -> (a,b,c,d,e)
to5 ((a,b),(c,d),e) = (a,b,c,d,e)

instance (RawSql a, RawSql b, RawSql c,
          RawSql d, RawSql e, RawSql f)
       => RawSql (a, b, c, d, e, f) where
    rawSqlColCount       = rawSqlColCount       . from6
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
    rawSqlColCount       = rawSqlColCount       . from7
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
    rawSqlColCount       = rawSqlColCount       . from8
    rawSqlColCountReason = rawSqlColCountReason . from8
    rawSqlProcessRow     = fmap to8 . rawSqlProcessRow

from8 :: (a,b,c,d,e,f,g,h) -> ((a,b),(c,d),(e,f),(g,h))
from8 (a,b,c,d,e,f,g,h) = ((a,b),(c,d),(e,f),(g,h))

to8 :: ((a,b),(c,d),(e,f),(g,h)) -> (a,b,c,d,e,f,g,h)
to8 ((a,b),(c,d),(e,f),(g,h)) = (a,b,c,d,e,f,g,h)
