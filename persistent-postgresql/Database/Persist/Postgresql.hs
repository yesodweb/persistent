{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A postgresql backend for persistent.
module Database.Persist.Postgresql
    ( withPostgresqlPool
    , withPostgresqlConn
    , createPostgresqlPool
    , module Database.Persist.Sql
    , ConnectionString
    , PostgresConf (..)
    , openSimpleConn
    ) where

import Database.Persist.Sql
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
import Data.Typeable
import Data.IORef
import qualified Data.Map as Map
import Data.Either (partitionEithers)
import Control.Arrow
import Data.List (find, intercalate, sort, groupBy, nub)
import Data.Function (on)
import Data.Conduit
import qualified Data.Conduit.List as CL

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Blaze.ByteString.Builder.Char8 as BBB
import qualified Blaze.ByteString.Builder.ByteString as BBS

import Data.Time.LocalTime (localTimeToUTC, utc)
import Data.Text (Text, pack)
import Data.Aeson
import Control.Monad (forM, mzero)
import System.Environment (getEnvironment)
import Data.Int (Int64)
import Data.Maybe (mapMaybe, fromJust, isJust)
import Data.Monoid ((<>))
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
        { connPrepare    = prepare' conn
        , connStmtMap    = smap
        , connInsertSql  = insertSql'
        , connClose      = PG.close conn
        , connMigrateSql = migrate'
        , connBegin      = const $ PG.begin    conn
        , connCommit     = const $ PG.commit   conn
        , connRollback   = const $ PG.rollback conn
        , connEscapeName = escape
        , connNoLimit    = "LIMIT ALL"
        , connRDBMS      = "postgresql"
        , connLimitOffset = decorateSQLWithLimitOffset "LIMIT ALL"
        }

prepare' :: PG.Connection -> Text -> IO Statement
prepare' conn sql = do
    let query = PG.Query (T.encodeUtf8 sql)
    return Statement
        { stmtFinalize = return ()
        , stmtReset = return ()
        , stmtExecute = execute' conn query
        , stmtQuery = withStmt' conn query
        }
        
insertSql' :: EntityDef SqlType -> [PersistValue] -> InsertSqlResult
insertSql' ent vals =
  let sql = T.concat
                [ "INSERT INTO "
                , escape $ entityDB ent
                , "("
                , T.intercalate "," $ map (escape . fieldDB) $ entityFields ent
                , ") VALUES("
                , T.intercalate "," (map (const "?") $ entityFields ent)
                , ")"
                ]
  in case entityPrimary ent of
       Just pdef -> ISRManyKeys sql vals
       Nothing -> ISRSingle (sql <> " RETURNING " <> escape (entityID ent))

execute' :: PG.Connection -> PG.Query -> [PersistValue] -> IO Int64
execute' conn query vals = PG.execute conn query (map P vals)

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
                    Nothing -> return $ \bs->
                      case bs of
                        Nothing -> fail $ "Unexpected null value in backend specific value"
                        Just a  -> return $ PersistDbSpecific a
                    Just bt -> return $ getGetter bt $
                               PG.Field ret col oid
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
                                  Just bs -> do
                                    ok <- PGFF.runConversion (getter mbs) conn
                                    bs `seq` case ok of
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
    toField (P (PersistDbSpecific s))  = PGTF.toField (Unknown s)
    toField (P (PersistObjectId _))    =
        error "Refusing to serialize a PersistObjectId to a PostgreSQL value"

newtype Unknown = Unknown { unUnknown :: ByteString }
  deriving (Eq, Show, Read, Ord, Typeable)

instance PGFF.FromField Unknown where
    fromField f mdata =
      case mdata of
        Nothing  -> PGFF.returnError PGFF.UnexpectedNull f ""
        Just dat -> return (Unknown dat)

instance PGTF.ToField Unknown where
    toField (Unknown a) = PGTF.Escape a

type Getter a = PGFF.FieldParser a

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
getGetter PG.Void                  = \_ _ -> return PersistNull
getGetter PG.UUID                  = convertPV (PersistDbSpecific . unUnknown)
getGetter other   = error $ "Postgresql.getGetter: type " ++
                            show other ++ " not supported."

unBinary :: PG.Binary a -> a
unBinary (PG.Binary x) = x

doesTableExist :: (Text -> IO Statement)
               -> DBName -- ^ table name
               -> IO Bool
doesTableExist getter (DBName name) = do
    stmt <- getter sql
    runResourceT $ stmtQuery stmt vals $$ start
  where
    sql = "SELECT COUNT(*) FROM information_schema.tables WHERE table_name=?"
    vals = [PersistText name]

    start = await >>= maybe (error "No results when checking doesTableExist") start'
    start' [PersistInt64 0] = finish False
    start' [PersistInt64 1] = finish True
    start' res = error $ "doesTableExist returned unexpected result: " ++ show res
    finish x = await >>= maybe (return x) (error "Too many rows returned in doesTableExist")

migrate' :: [EntityDef a]
         -> (Text -> IO Statement)
         -> EntityDef SqlType
         -> IO (Either [Text] [(Bool, Text)])
                             -- We nub because there might be duplicate statements generated
                             -- such as AlterColumn DropReference and AlterTable DropConstraint
                             -- for the same reference constraint.
migrate' allDefs getter val = fmap (fmap $ nub . map showAlterDb) $ do
    let name = entityDB val
    old <- getColumns getter val
    case partitionEithers old of
        ([], old'') -> do
            let old' = partitionEithers old''
            let (newcols', udefs, fdefs) = mkColumns allDefs val
            let newcols = filter (not . safeToRemove val . cName) newcols'
            let udspair = map udToPair udefs
            -- Check for table existence if there are no columns, workaround
            -- for https://github.com/yesodweb/persistent/issues/152
            exists <-
                if null old
                    then doesTableExist getter name
                    else return True

            if not exists
                then do
                    let idtxt = case entityPrimary val of
                                  Just pdef -> concat [" PRIMARY KEY (", intercalate "," $ map (T.unpack . escape . snd) $ primaryFields pdef, ")"]
                                  Nothing   -> concat [T.unpack $ escape $ entityID val
                                        , " SERIAL PRIMARY KEY UNIQUE"]
                    let addTable = AddTable $ concat
                            -- Lower case e: see Database.Persist.Sql.Migration
                            [ "CREATe TABLE "
                            , T.unpack $ escape name
                            , "("
                            , idtxt
                            , if null newcols then [] else ","
                            , intercalate "," $ map showColumn newcols
                            , ")"
                            ]
                    let uniques = flip concatMap udspair $ \(uname, ucols) ->
                            [AlterTable name $ AddUniqueConstraint uname ucols]
                        references = mapMaybe (\c@Column { cName=cname, cReference=Just (refTblName, _) } -> getAddReference allDefs name refTblName cname (cReference c)) $ filter (\c -> cReference c /= Nothing) newcols
                        foreignsAlt = map (\fdef -> let (childfields, parentfields) = unzip (map (\(_,b,_,d) -> (b,d)) (foreignFields fdef)) 
                                                    in AlterColumn name (foreignRefTableDBName fdef, AddReference (foreignConstraintNameDBName fdef) childfields parentfields)) fdefs
                    return $ Right $ addTable : uniques ++ references ++ foreignsAlt
                else do
                    let (acs, ats) = getAlters allDefs val (newcols, udspair) old'
                    let acs' = map (AlterColumn name) acs
                    let ats' = map (AlterTable name) ats
                    return $ Right $ acs' ++ ats'
        (errs, _) -> return $ Left errs

type SafeToRemove = Bool

data AlterColumn = Type SqlType | IsNull | NotNull | Add' Column | Drop SafeToRemove
                 | Default String | NoDefault | Update' String
                 | AddReference DBName [DBName] [DBName] | DropReference DBName
type AlterColumn' = (DBName, AlterColumn)

data AlterTable = AddUniqueConstraint DBName [DBName]
                | DropConstraint DBName

data AlterDB = AddTable String
             | AlterColumn DBName AlterColumn'
             | AlterTable DBName AlterTable

-- | Returns all of the columns in the given table currently in the database.
getColumns :: (Text -> IO Statement)
           -> EntityDef a
           -> IO [Either Text (Either Column (DBName, [DBName]))]
getColumns getter def = do
    let sqlv=T.concat ["SELECT "
                          ,"column_name "
                          ,",is_nullable "
                          ,",udt_name "
                          ,",column_default "
                          ,",numeric_precision "
                          ,",numeric_scale "
                          ,"FROM information_schema.columns "
                          ,"WHERE table_catalog=current_database() "
                          ,"AND table_schema=current_schema() "
                          ,"AND table_name=? "
                          ,"AND column_name <> ?"]
  
    stmt <- getter sqlv
    let vals =
            [ PersistText $ unDBName $ entityDB def
            , PersistText $ unDBName $ entityID def
            ]
    cs <- runResourceT $ stmtQuery stmt vals $$ helper
    let sqlc=concat ["SELECT "
                          ,"c.constraint_name, "
                          ,"c.column_name "
                          ,"FROM information_schema.key_column_usage c, "
                          ,"information_schema.table_constraints k "
                          ,"WHERE c.table_catalog=current_database() "
                          ,"AND c.table_catalog=k.table_catalog "
                          ,"AND c.table_schema=current_schema() "
                          ,"AND c.table_schema=k.table_schema "
                          ,"AND c.table_name=? "
                          ,"AND c.table_name=k.table_name "
                          ,"AND c.column_name <> ? "
                          ,"AND c.ordinal_position=1 "
                          ,"AND c.constraint_name=k.constraint_name "
                          ,"AND k.constraint_type <> 'PRIMARY KEY' "
                          ,"ORDER BY c.constraint_name, c.column_name"]

    stmt' <- getter $ pack sqlc
        
    us <- runResourceT $ stmtQuery stmt' vals $$ helperU
    return $ cs ++ us
  where
    getAll front = do
        x <- CL.head
        case x of
            Nothing -> return $ front []
            Just [PersistText con, PersistText col] -> getAll (front . (:) (con, col))
            Just [PersistByteString con, PersistByteString col] -> getAll (front . (:) (T.decodeUtf8 con, T.decodeUtf8 col)) 
            Just o -> error $ "unexpected datatype returned for postgres o="++show o
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

-- | Check if a column name is listed as the "safe to remove" in the entity
-- list.
safeToRemove :: EntityDef a -> DBName -> Bool
safeToRemove def (DBName colName)
    = any (elem "SafeToRemove" . fieldAttrs)
    $ filter ((== (DBName colName)) . fieldDB)
    $ entityFields def

getAlters :: [EntityDef a]
          -> EntityDef SqlType
          -> ([Column], [(DBName, [DBName])])
          -> ([Column], [(DBName, [DBName])])
          -> ([AlterColumn'], [AlterTable])
getAlters defs def (c1, u1) (c2, u2) =
    (getAltersC c1 c2, getAltersU u1 u2)
  where
    getAltersC [] old = map (\x -> (cName x, Drop $ safeToRemove def $ cName x)) old
    getAltersC (new:news) old =
        let (alters, old') = findAlters defs (entityDB def) new old
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
                 in if sort cols == sort ocols
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
                    return $ Right Column
                        { cName = cname
                        , cNull = y == "YES"
                        , cSqlType = t
                        , cDefault = d''
                        , cDefaultConstraintName = Nothing
                        , cMaxLen = Nothing
                        , cReference = ref
                        }
  where
    getRef cname = do
        let sql = pack $ concat
                [ "SELECT COUNT(*) FROM "
                , "information_schema.table_constraints "
                , "WHERE table_catalog=current_database() "
                , "AND table_schema=current_schema() "
                , "AND table_name=? "
                , "AND constraint_type='FOREIGN KEY' "
                , "AND constraint_name=?"
                ]
        let ref = refName tname cname
        stmt <- getter sql
        runResourceT $ stmtQuery stmt
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

findAlters :: [EntityDef a] -> DBName -> Column -> [Column] -> ([AlterColumn'], [Column])
findAlters defs tablename col@(Column name isNull sqltype def defConstraintName _maxLen ref) cols =
    case filter (\c -> cName c == name) cols of
        [] -> ([(name, Add' col)], cols)
        Column _ isNull' sqltype' def' defConstraintName' _maxLen' ref':_ ->
            let refDrop Nothing = []
                refDrop (Just (_, cname)) = [(name, DropReference cname)]
                refAdd Nothing = []
                refAdd (Just (tname, a)) = case find ((==tname) . entityDB) defs of
                                                Just refdef -> [(tname, AddReference a [name] [entityID refdef])]
                                                Nothing -> error $ "could not find the entityDef for reftable[" ++ show tname ++ "]"
                modRef = 
                    if fmap snd ref == fmap snd ref'
                        then []
                        else refDrop ref' ++ refAdd ref
                modNull = case (isNull, isNull') of
                            (True, False) -> [(name, IsNull)]
                            (False, True) ->
                                let up = case def of
                                            Nothing -> id
                                            Just s -> (:) (name, Update' $ T.unpack s)
                                 in up [(name, NotNull)]
                            _ -> []
                modType = if sqltype == sqltype' then [] else [(name, Type sqltype)]
                modDef =
                    if def == def'
                        then []
                        else case def of
                                Nothing -> [(name, NoDefault)]
                                Just s -> [(name, Default $ T.unpack s)]
             in (modRef ++ modDef ++ modNull ++ modType,
                 filter (\c -> cName c /= name) cols)

-- | Get the references to be added to a table for the given column.
getAddReference :: [EntityDef a] -> DBName -> DBName -> DBName -> Maybe (DBName, DBName) -> Maybe AlterDB
getAddReference allDefs table reftable cname ref =
    case ref of
        Nothing -> Nothing
        Just (s, z) -> Just $ AlterColumn table (s, AddReference (refName table cname) [cname] [id_])
                          where
                            id_ = maybe (error $ "Could not find ID of entity " ++ show reftable)
                                        id $ do
                                          entDef <- find ((== reftable) . entityDB) allDefs
                                          return (entityID entDef)
                          

showColumn :: Column -> String
showColumn c@(Column n nu sqlType def defConstraintName _maxLen _ref) = concat
    [ T.unpack $ escape n
    , " "
    , showSqlType sqlType
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
    isUnsafe (Drop safeToRemove) = not safeToRemove
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
showAlter table (_, Add' col) =
    concat
        [ "ALTER TABLE "
        , T.unpack $ escape table
        , " ADD COLUMN "
        , showColumn col
        ]
showAlter table (n, Drop _) =
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
showAlter table (n, Update' s) = concat
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
showAlter table (reftable, AddReference fkeyname t2 id2) = concat
    [ "ALTER TABLE "
    , T.unpack $ escape table
    , " ADD CONSTRAINT "
    , T.unpack $ escape fkeyname
    , " FOREIGN KEY("
    , T.unpack $ T.intercalate "," $ map escape t2
    , ") REFERENCES "
    , T.unpack $ escape reftable
    , "("
    , T.unpack $ T.intercalate "," $ map escape id2
    , ")"
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
    type PersistConfigBackend PostgresConf = SqlPersistT
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
