{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A postgresql backend for persistent.
module Database.Persist.Postgresql
    ( withPostgresqlPool
    , withPostgresqlConn
    , createPostgresqlPool
    , module Database.Persist
    , module Database.Persist.GenericSql
    , ConnectionString
    , PostgresConf (..)
    , openSimpleConn
    ) where

import Database.Persist hiding (Entity (..))
import Database.Persist.Store
import Database.Persist.GenericSql hiding (Key)
import Database.Persist.GenericSql.Internal
import Database.Persist.EntityDef
import Data.Maybe (mapMaybe)
import Data.Fixed (Pico)

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.BuiltinTypes as PG
import qualified Database.PostgreSQL.Simple.Internal as PG
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.FromField as PGFF
import qualified Database.PostgreSQL.Simple.Types as PG
import Database.PostgreSQL.Simple.Ok (Ok (..))

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as Map
import Data.Either (partitionEithers)
import Control.Arrow
import Data.List (sort, groupBy)
import Data.Function (on)
import Data.Conduit
import qualified Data.Conduit.List as CL

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Blaze.ByteString.Builder.Char8 as BBB
import Data.Time.LocalTime (localTimeToUTC, utc)
import Data.Text (Text, pack)
import Data.Aeson
import Control.Monad (forM, mzero)
import System.Environment (getEnvironment)


-- | A @libpq@ connection string.  A simple example of connection
-- string would be @\"host=localhost port=5432 user=test
-- dbname=test password=test\"@.  Please read libpq's
-- documentation at
-- <http://www.postgresql.org/docs/9.1/static/libpq-connect.html>
-- for more details on how to create such strings.
type ConnectionString = ByteString


-- | Create a PostgreSQL connection pool and run the given
-- action.  The pool is properly released after the action
-- finishes using it.  Note that you should not use the given
-- 'ConnectionPool' outside the action since it may be already
-- been released.
withPostgresqlPool :: MonadIO m
                   => ConnectionString
                   -- ^ Connection string to the database.
                   -> Int
                   -- ^ Number of connections to be kept open in
                   -- the pool.
                   -> (ConnectionPool -> m a)
                   -- ^ Action to be executed that uses the
                   -- connection pool.
                   -> m a
withPostgresqlPool ci = withSqlPool $ open' ci


-- | Create a PostgreSQL connection pool.  Note that it's your
-- responsability to properly close the connection pool when
-- unneeded.  Use 'withPostgresqlPool' for an automatic resource
-- control.
createPostgresqlPool :: MonadIO m
                     => ConnectionString
                     -- ^ Connection string to the database.
                     -> Int
                     -- ^ Number of connections to be kept open
                     -- in the pool.
                     -> m ConnectionPool
createPostgresqlPool ci = createSqlPool $ open' ci


-- | Same as 'withPostgresqlPool', but instead of opening a pool
-- of connections, only one connection is opened.
withPostgresqlConn :: (MonadIO m, MonadBaseControl IO m)
                   => ConnectionString -> (Connection -> m a) -> m a
withPostgresqlConn = withSqlConn . open'

open' :: ConnectionString -> IO Connection
open' cstr = do
    PG.connectPostgreSQL cstr >>= openSimpleConn

-- | Generate a 'Connection' from a 'PG.Connection'
openSimpleConn :: PG.Connection -> IO Connection
openSimpleConn conn = do
    smap <- newIORef $ Map.empty
    return Connection
        { prepare    = prepare' conn
        , stmtMap    = smap
        , insertSql  = insertSql'
        , close      = PG.close conn
        , migrateSql = migrate'
        , begin      = const $ PG.begin    conn
        , commitC    = const $ PG.commit   conn
        , rollbackC  = const $ PG.rollback conn
        , escapeName = escape
        , noLimit    = "LIMIT ALL"
        }

prepare' :: PG.Connection -> Text -> IO Statement
prepare' conn sql = do
    let query = PG.Query (T.encodeUtf8 sql)
    return Statement
        { finalize = return ()
        , reset = return ()
        , execute = execute' conn query
        , withStmt = withStmt' conn query
        }

insertSql' :: DBName -> [DBName] -> DBName -> InsertSqlResult
insertSql' t cols id' = ISRSingle $ pack $ concat
    [ "INSERT INTO "
    , T.unpack $ escape t
    , "("
    , intercalate "," $ map (T.unpack . escape) cols
    , ") VALUES("
    , intercalate "," (map (const "?") cols)
    , ") RETURNING "
    , T.unpack $ escape id'
    ]

execute' :: PG.Connection -> PG.Query -> [PersistValue] -> IO ()
execute' conn query vals = do
    _ <- PG.execute conn query (map P vals)
    return ()

withStmt' :: MonadResource m
          => PG.Connection
          -> PG.Query
          -> [PersistValue]
          -> Source m [PersistValue]
withStmt' conn query vals =
    bracketP openS closeS pull
  where
    openS = do
      -- Construct raw query
      rawquery <- PG.formatQuery conn query (map P vals)

      -- Take raw connection
      PG.withConnection conn $ \rawconn -> do
            -- Execute query
            mret <- LibPQ.exec rawconn rawquery
            case mret of
              Nothing -> do
                merr <- LibPQ.errorMessage rawconn
                fail $ case merr of
                         Nothing -> "Postgresql.withStmt': unknown error"
                         Just e  -> "Postgresql.withStmt': " ++ B8.unpack e
              Just ret -> do
                -- Check result status
                status <- LibPQ.resultStatus ret
                case status of
                  LibPQ.TuplesOk -> return ()
                  _ -> do
                    msg <- LibPQ.resStatus status
                    mmsg <- LibPQ.resultErrorMessage ret
                    fail $ "Postgresql.withStmt': bad result status " ++
                           show status ++ " (" ++ (maybe (show msg) (show . (,) msg) mmsg) ++ ")"

                -- Get number and type of columns
                cols <- LibPQ.nfields ret
                getters <- forM [0..cols-1] $ \col -> do
                  oid <- LibPQ.ftype ret col
                  case PG.oid2builtin oid of
                    Nothing -> fail $ "Postgresql.withStmt': could not " ++
                                      "recognize Oid of column " ++
                                      show (let LibPQ.Col i = col in i) ++
                                      " (counting from zero)"
                    Just bt -> return $ getGetter bt $
                               PG.Field ret col $
                               PG.builtin2typname bt
                -- Ready to go!
                rowRef   <- newIORef (LibPQ.Row 0)
                rowCount <- LibPQ.ntuples ret
                return (ret, rowRef, rowCount, getters)

    closeS (ret, _, _, _) = LibPQ.unsafeFreeResult ret

    pull x = do
        y <- liftIO $ pullS x
        case y of
            Nothing -> return ()
            Just z -> yield z >> pull x

    pullS (ret, rowRef, rowCount, getters) = do
        row <- atomicModifyIORef rowRef (\r -> (r+1, r))
        if row == rowCount
           then return Nothing
           else fmap Just $ forM (zip getters [0..]) $ \(getter, col) -> do
                                mbs <- LibPQ.getvalue' ret row col
                                case mbs of
                                  Nothing -> return PersistNull
                                  Just bs -> bs `seq` case getter mbs of
                                                        Errors (exc:_) -> throw exc
                                                        Errors [] -> error "Got an Errors, but no exceptions"
                                                        Ok v  -> return v

-- | Avoid orphan instances.
newtype P = P PersistValue


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
    toField (P (PersistZonedTime (ZT t))) = PGTF.toField t
    toField (P PersistNull)            = PGTF.toField PG.Null
    toField (P (PersistList l))        = PGTF.toField $ listToJSON l
    toField (P (PersistMap m))         = PGTF.toField $ mapToJSON m
    toField (P (PersistObjectId _))    =
        error "Refusing to serialize a PersistObjectId to a PostgreSQL value"

type Getter a = PG.Field -> Maybe ByteString -> Ok a

convertPV :: PGFF.FromField a => (a -> b) -> Getter b
convertPV f = (fmap f .) . PGFF.fromField

-- FIXME: check if those are correct and complete.
getGetter :: PG.BuiltinType -> Getter PersistValue
getGetter PG.Bool                  = convertPV PersistBool
getGetter PG.ByteA                 = convertPV (PersistByteString . unBinary)
getGetter PG.Char                  = convertPV PersistText
getGetter PG.Name                  = convertPV PersistText
getGetter PG.Int8                  = convertPV PersistInt64
getGetter PG.Int2                  = convertPV PersistInt64
getGetter PG.Int4                  = convertPV PersistInt64
getGetter PG.Text                  = convertPV PersistText
getGetter PG.Xml                   = convertPV PersistText
getGetter PG.Float4                = convertPV PersistDouble
getGetter PG.Float8                = convertPV PersistDouble
getGetter PG.AbsTime               = convertPV PersistUTCTime
getGetter PG.RelTime               = convertPV PersistUTCTime
getGetter PG.Money                 = convertPV PersistDouble
getGetter PG.BpChar                = convertPV PersistText
getGetter PG.VarChar               = convertPV PersistText
getGetter PG.Date                  = convertPV PersistDay
getGetter PG.Time                  = convertPV PersistTimeOfDay
getGetter PG.Timestamp             = convertPV (PersistUTCTime . localTimeToUTC utc)
getGetter PG.TimestampTZ           = convertPV (PersistZonedTime . ZT)
getGetter PG.Bit                   = convertPV PersistInt64
getGetter PG.VarBit                = convertPV PersistInt64
getGetter PG.Numeric               = convertPV PersistRational
getGetter PG.Void                  = \_ _ -> Ok PersistNull
getGetter other   = error $ "Postgresql.getGetter: type " ++
                            show other ++ " not supported."

unBinary :: PG.Binary a -> a
unBinary (PG.Binary x) = x

migrate' :: PersistEntity val
         => [EntityDef]
         -> (Text -> IO Statement)
         -> val
         -> IO (Either [Text] [(Bool, Text)])
migrate' allDefs getter val = fmap (fmap $ map showAlterDb) $ do
    let name = entityDB $ entityDef val
    old <- getColumns getter $ entityDef val
    case partitionEithers old of
        ([], old'') -> do
            let old' = partitionEithers old''
            let new = second (map udToPair) $ mkColumns allDefs val
            if null old
                then do
                    let addTable = AddTable $ concat
                            -- Lower case e: see Database.Persistent.GenericSql.Migration
                            [ "CREATe TABLE "
                            , T.unpack $ escape name
                            , "("
                            , T.unpack $ escape $ entityID $ entityDef val
                            , " SERIAL PRIMARY KEY UNIQUE"
                            , concatMap (\x -> ',' : showColumn x) $ fst new
                            , ")"
                            ]
                    let uniques = flip concatMap (snd new) $ \(uname, ucols) ->
                            [AlterTable name $ AddUniqueConstraint uname ucols]
                        references = mapMaybe (getAddReference name) $ fst new
                    return $ Right $ addTable : uniques ++ references
                else do
                    let (acs, ats) = getAlters new old'
                    let acs' = map (AlterColumn name) acs
                    let ats' = map (AlterTable name) ats
                    return $ Right $ acs' ++ ats'
        (errs, _) -> return $ Left errs

data AlterColumn = Type SqlType | IsNull | NotNull | Add Column | Drop
                 | Default String | NoDefault | Update String
                 | AddReference DBName | DropReference DBName
type AlterColumn' = (DBName, AlterColumn)

data AlterTable = AddUniqueConstraint DBName [DBName]
                | DropConstraint DBName

data AlterDB = AddTable String
             | AlterColumn DBName AlterColumn'
             | AlterTable DBName AlterTable

-- | Returns all of the columns in the given table currently in the database.
getColumns :: (Text -> IO Statement)
           -> EntityDef
           -> IO [Either Text (Either Column (DBName, [DBName]))]
getColumns getter def = do
    stmt <- getter "SELECT column_name,is_nullable,udt_name,column_default,numeric_precision,numeric_scale FROM information_schema.columns WHERE table_name=? AND column_name <> ?"
    let vals =
            [ PersistText $ unDBName $ entityDB def
            , PersistText $ unDBName $ entityID def
            ]
    cs <- runResourceT $ withStmt stmt vals $$ helper
    stmt' <- getter
        "SELECT constraint_name, column_name FROM information_schema.constraint_column_usage WHERE table_name=? AND column_name <> ? ORDER BY constraint_name, column_name"
    us <- runResourceT $ withStmt stmt' vals $$ helperU
    return $ cs ++ us
  where
    getAll front = do
        x <- CL.head
        case x of
            Nothing -> return $ front []
            Just [PersistText con, PersistText col] ->
                getAll (front . (:) (con, col))
            Just _ -> getAll front -- FIXME error message?
    helperU = do
        rows <- getAll id
        return $ map (Right . Right . (DBName . fst . head &&& map (DBName . snd)))
               $ groupBy ((==) `on` fst) rows
    helper = do
        x <- CL.head
        case x of
            Nothing -> return []
            Just x' -> do
                col <- liftIO $ getColumn getter (entityDB def) x'
                let col' = case col of
                            Left e -> Left e
                            Right c -> Right $ Left c
                cols <- helper
                return $ col' : cols

getAlters :: ([Column], [(DBName, [DBName])])
          -> ([Column], [(DBName, [DBName])])
          -> ([AlterColumn'], [AlterTable])
getAlters (c1, u1) (c2, u2) =
    (getAltersC c1 c2, getAltersU u1 u2)
  where
    getAltersC [] old = map (\x -> (cName x, Drop)) old
    getAltersC (new:news) old =
        let (alters, old') = findAlters new old
         in alters ++ getAltersC news old'

    getAltersU :: [(DBName, [DBName])]
               -> [(DBName, [DBName])]
               -> [AlterTable]
    getAltersU [] old = map DropConstraint $ filter (not . isManual) $ map fst old
    getAltersU ((name, cols):news) old =
        case lookup name old of
            Nothing -> AddUniqueConstraint name cols : getAltersU news old
            Just ocols ->
                let old' = filter (\(x, _) -> x /= name) old
                 in if sort cols == ocols
                        then getAltersU news old'
                        else  DropConstraint name
                            : AddUniqueConstraint name cols
                            : getAltersU news old'

    -- Don't drop constraints which were manually added.
    isManual (DBName x) = "__manual_" `T.isPrefixOf` x

getColumn :: (Text -> IO Statement)
          -> DBName -> [PersistValue]
          -> IO (Either Text Column)
getColumn getter tname [PersistText x, PersistText y, PersistText z, d, npre, nscl] =
    case d' of
        Left s -> return $ Left s
        Right d'' ->
            case getType z of
                Left s -> return $ Left s
                Right t -> do
                    let cname = DBName x
                    ref <- getRef cname
                    return $ Right $ Column cname (y == "YES") t d'' Nothing ref
  where
    getRef cname = do
        let sql = pack $ concat
                [ "SELECT COUNT(*) FROM "
                , "information_schema.table_constraints "
                , "WHERE table_name=? "
                , "AND constraint_type='FOREIGN KEY' "
                , "AND constraint_name=?"
                ]
        let ref = refName tname cname
        stmt <- getter sql
        runResourceT $ withStmt stmt
                     [ PersistText $ unDBName tname
                     , PersistText $ unDBName ref
                     ] $$ do
            Just [PersistInt64 i] <- CL.head
            return $ if i == 0 then Nothing else Just (DBName "", ref)
    d' = case d of
            PersistNull   -> Right Nothing
            PersistText t -> Right $ Just t
            _ -> Left $ pack $ "Invalid default column: " ++ show d
    getType "int4"        = Right $ SqlInt32
    getType "int8"        = Right $ SqlInt64
    getType "varchar"     = Right $ SqlString
    getType "date"        = Right $ SqlDay
    getType "bool"        = Right $ SqlBool
    getType "timestamp"   = Right $ SqlDayTime
    getType "timestamptz" = Right $ SqlDayTimeZoned
    getType "float4"      = Right $ SqlReal
    getType "float8"      = Right $ SqlReal
    getType "bytea"       = Right $ SqlBlob
    getType "time"        = Right $ SqlTime
    getType "numeric"     = getNumeric npre nscl
    getType a             = Right $ SqlOther a

    getNumeric (PersistInt64 a) (PersistInt64 b) = Right $ SqlNumeric (fromIntegral a) (fromIntegral b)
    getNumeric a b = Left $ pack $ "Can not get numeric field precision, got: " ++ show a ++ " and " ++ show b ++ " as precision and scale"
getColumn _ _ x =
    return $ Left $ pack $ "Invalid result from information_schema: " ++ show x

findAlters :: Column -> [Column] -> ([AlterColumn'], [Column])
findAlters col@(Column name isNull type_ def _maxLen ref) cols =
    case filter (\c -> cName c == name) cols of
        [] -> ([(name, Add col)], cols)
        Column _ isNull' type_' def' _maxLen' ref':_ ->
            let refDrop Nothing = []
                refDrop (Just (_, cname)) = [(name, DropReference cname)]
                refAdd Nothing = []
                refAdd (Just (tname, _)) = [(name, AddReference tname)]
                modRef =
                    if fmap snd ref == fmap snd ref'
                        then []
                        else refDrop ref' ++ refAdd ref
                modNull = case (isNull, isNull') of
                            (True, False) -> [(name, IsNull)]
                            (False, True) ->
                                let up = case def of
                                            Nothing -> id
                                            Just s -> (:) (name, Update $ T.unpack s)
                                 in up [(name, NotNull)]
                            _ -> []
                modType = if type_ == type_' then [] else [(name, Type type_)]
                modDef =
                    if def == def'
                        then []
                        else case def of
                                Nothing -> [(name, NoDefault)]
                                Just s -> [(name, Default $ T.unpack s)]
             in (modRef ++ modDef ++ modNull ++ modType,
                 filter (\c -> cName c /= name) cols)

-- | Get the references to be added to a table for the given column.
getAddReference :: DBName -> Column -> Maybe AlterDB
getAddReference table (Column n _nu _t _def _maxLen ref) =
    case ref of
        Nothing -> Nothing
        Just (s, _) -> Just $ AlterColumn table (n, AddReference s)

showColumn :: Column -> String
showColumn (Column n nu t def _maxLen _) = concat
    [ T.unpack $ escape n
    , " "
    , showSqlType t
    , " "
    , if nu then "NULL" else "NOT NULL"
    , case def of
        Nothing -> ""
        Just s -> " DEFAULT " ++ T.unpack s
    ]

showSqlType :: SqlType -> String
showSqlType SqlString = "VARCHAR"
showSqlType SqlInt32 = "INT4"
showSqlType SqlInt64 = "INT8"
showSqlType SqlReal = "DOUBLE PRECISION"
showSqlType (SqlNumeric s prec) = "NUMERIC(" ++ show s ++ "," ++ show prec ++ ")"
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTime = "TIMESTAMP"
showSqlType SqlDayTimeZoned = "TIMESTAMP WITH TIME ZONE"
showSqlType SqlBlob = "BYTEA"
showSqlType SqlBool = "BOOLEAN"
showSqlType (SqlOther t) = T.unpack t

showAlterDb :: AlterDB -> (Bool, Text)
showAlterDb (AddTable s) = (False, pack s)
showAlterDb (AlterColumn t (c, ac)) =
    (isUnsafe ac, pack $ showAlter t (c, ac))
  where
    isUnsafe Drop = True
    isUnsafe _ = False
showAlterDb (AlterTable t at) = (False, pack $ showAlterTable t at)

showAlterTable :: DBName -> AlterTable -> String
showAlterTable table (AddUniqueConstraint cname cols) = concat
    [ "ALTER TABLE "
    , T.unpack $ escape table
    , " ADD CONSTRAINT "
    , T.unpack $ escape cname
    , " UNIQUE("
    , intercalate "," $ map (T.unpack . escape) cols
    , ")"
    ]
showAlterTable table (DropConstraint cname) = concat
    [ "ALTER TABLE "
    , T.unpack $ escape table
    , " DROP CONSTRAINT "
    , T.unpack $ escape cname
    ]

showAlter :: DBName -> AlterColumn' -> String
showAlter table (n, Type t) =
    concat
        [ "ALTER TABLE "
        , T.unpack $ escape table
        , " ALTER COLUMN "
        , T.unpack $ escape n
        , " TYPE "
        , showSqlType t
        ]
showAlter table (n, IsNull) =
    concat
        [ "ALTER TABLE "
        , T.unpack $ escape table
        , " ALTER COLUMN "
        , T.unpack $ escape n
        , " DROP NOT NULL"
        ]
showAlter table (n, NotNull) =
    concat
        [ "ALTER TABLE "
        , T.unpack $ escape table
        , " ALTER COLUMN "
        , T.unpack $ escape n
        , " SET NOT NULL"
        ]
showAlter table (_, Add col) =
    concat
        [ "ALTER TABLE "
        , T.unpack $ escape table
        , " ADD COLUMN "
        , showColumn col
        ]
showAlter table (n, Drop) =
    concat
        [ "ALTER TABLE "
        , T.unpack $ escape table
        , " DROP COLUMN "
        , T.unpack $ escape n
        ]
showAlter table (n, Default s) =
    concat
        [ "ALTER TABLE "
        , T.unpack $ escape table
        , " ALTER COLUMN "
        , T.unpack $ escape n
        , " SET DEFAULT "
        , s
        ]
showAlter table (n, NoDefault) = concat
    [ "ALTER TABLE "
    , T.unpack $ escape table
    , " ALTER COLUMN "
    , T.unpack $ escape n
    , " DROP DEFAULT"
    ]
showAlter table (n, Update s) = concat
    [ "UPDATE "
    , T.unpack $ escape table
    , " SET "
    , T.unpack $ escape n
    , "="
    , s
    , " WHERE "
    , T.unpack $ escape n
    , " IS NULL"
    ]
showAlter table (n, AddReference t2) = concat
    [ "ALTER TABLE "
    , T.unpack $ escape table
    , " ADD CONSTRAINT "
    , T.unpack $ escape $ refName table n
    , " FOREIGN KEY("
    , T.unpack $ escape n
    , ") REFERENCES "
    , T.unpack $ escape t2
    ]
showAlter table (_, DropReference cname) = concat
    [ "ALTER TABLE "
    , T.unpack (escape table)
    , " DROP CONSTRAINT "
    , T.unpack $ escape cname
    ]

escape :: DBName -> Text
escape (DBName s) =
    T.pack $ '"' : go (T.unpack s) ++ "\""
  where
    go "" = ""
    go ('"':xs) = "\"\"" ++ go xs
    go (x:xs) = x : go xs

-- | Information required to connect to a PostgreSQL database
-- using @persistent@'s generic facilities.  These values are the
-- same that are given to 'withPostgresqlPool'.
data PostgresConf = PostgresConf
    { pgConnStr  :: ConnectionString
      -- ^ The connection string.
    , pgPoolSize :: Int
      -- ^ How many connections should be held on the connection pool.
    }

instance PersistConfig PostgresConf where
    type PersistConfigBackend PostgresConf = SqlPersist
    type PersistConfigPool PostgresConf = ConnectionPool
    createPoolConfig (PostgresConf cs size) = createPostgresqlPool cs size
    runPool _ = runSqlPool
    loadConfig (Object o) = do
        database <- o .: "database"
        host     <- o .: "host"
        port     <- o .:? "port" .!= 5432
        user     <- o .: "user"
        password <- o .: "password"
        pool     <- o .: "poolsize"
        let ci = PG.ConnectInfo
                   { PG.connectHost     = host
                   , PG.connectPort     = port
                   , PG.connectUser     = user
                   , PG.connectPassword = password
                   , PG.connectDatabase = database
                   }
            cstr = PG.postgreSQLConnectionString ci
        return $ PostgresConf cstr pool
    loadConfig _ = mzero

    applyEnv c0 = do
        env <- getEnvironment
        return $ addUser env
               $ addPass env
               $ addDatabase env
               $ addPort env
               $ addHost env c0
      where
        addParam param val c =
            c { pgConnStr = B8.concat [pgConnStr c, " ", param, "='", pgescape val, "'"] }

        pgescape = B8.pack . go
            where
              go ('\'':rest) = '\\' : '\'' : go rest
              go ('\\':rest) = '\\' : '\\' : go rest
              go ( x  :rest) =      x      : go rest
              go []          = []

        maybeAddParam param envvar env =
            maybe id (addParam param) $
            lookup envvar env

        addHost     = maybeAddParam "host"     "PGHOST"
        addPort     = maybeAddParam "port"     "PGPORT"
        addUser     = maybeAddParam "user"     "PGUSER"
        addPass     = maybeAddParam "password" "PGPASS"
        addDatabase = maybeAddParam "dbname"   "PGDATABASE"

refName :: DBName -> DBName -> DBName
refName (DBName table) (DBName column) =
    DBName $ T.concat [table, "_", column, "_fkey"]

udToPair :: UniqueDef -> (DBName, [DBName])
udToPair ud = (uniqueDBName ud, map snd $ uniqueFields ud)
