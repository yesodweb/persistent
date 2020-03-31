{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
-- | A MySQL backend for @persistent@.
module Database.Persist.MySQL
    ( withMySQLPool
    , withMySQLConn
    , createMySQLPool
    , module Database.Persist.Sql
    , MySQL.ConnectInfo(..)
    , MySQLBase.SSLInfo(..)
    , MySQL.defaultConnectInfo
    , MySQLBase.defaultSSLInfo
    , MySQLConf(..)
    , mockMigration
     -- * @ON DUPLICATE KEY UPDATE@ Functionality
    , insertOnDuplicateKeyUpdate
    , insertManyOnDuplicateKeyUpdate
    , HandleUpdateCollision
    , pattern SomeField
    , SomeField
    , copyField
    , copyUnlessNull
    , copyUnlessEmpty
    , copyUnlessEq
    ) where

import qualified Blaze.ByteString.Builder.Char8 as BBB
import qualified Blaze.ByteString.Builder.ByteString as BBS

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLogger, runNoLoggingT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (runReaderT, ReaderT)
import Control.Monad.Trans.Writer (runWriterT)

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Acquire (Acquire, mkAcquire, with)
import Data.Aeson
import Data.Aeson.Types (modifyFailure)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.Fixed (Pico)
import Data.Function (on)
import Data.Int (Int64)
import Data.IORef
import Data.List (find, intercalate, sort, groupBy)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Monoid as Monoid
import Data.Pool (Pool)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Text.Read (readMaybe)
import System.Environment (getEnvironment)

import Database.Persist.Sql
import Database.Persist.Sql.Types.Internal (makeIsolationLevelStatement)
import qualified Database.Persist.Sql.Util as Util

import qualified Database.MySQL.Base          as MySQLBase
import qualified Database.MySQL.Base.Types    as MySQLBase
import qualified Database.MySQL.Simple        as MySQL
import qualified Database.MySQL.Simple.Param  as MySQL
import qualified Database.MySQL.Simple.Result as MySQL
import qualified Database.MySQL.Simple.Types  as MySQL

-- | Create a MySQL connection pool and run the given action.
-- The pool is properly released after the action finishes using
-- it.  Note that you should not use the given 'ConnectionPool'
-- outside the action since it may be already been released.
withMySQLPool :: (MonadLogger m, MonadUnliftIO m)
              => MySQL.ConnectInfo
              -- ^ Connection information.
              -> Int
              -- ^ Number of connections to be kept open in the pool.
              -> (Pool SqlBackend -> m a)
              -- ^ Action to be executed that uses the connection pool.
              -> m a
withMySQLPool ci = withSqlPool $ open' ci


-- | Create a MySQL connection pool.  Note that it's your
-- responsibility to properly close the connection pool when
-- unneeded.  Use 'withMySQLPool' for automatic resource control.
createMySQLPool :: (MonadUnliftIO m, MonadLogger m)
                => MySQL.ConnectInfo
                -- ^ Connection information.
                -> Int
                -- ^ Number of connections to be kept open in the pool.
                -> m (Pool SqlBackend)
createMySQLPool ci = createSqlPool $ open' ci


-- | Same as 'withMySQLPool', but instead of opening a pool
-- of connections, only one connection is opened.
withMySQLConn :: (MonadUnliftIO m, MonadLogger m)
              => MySQL.ConnectInfo
              -- ^ Connection information.
              -> (SqlBackend -> m a)
              -- ^ Action to be executed that uses the connection.
              -> m a
withMySQLConn = withSqlConn . open'


-- | Internal function that opens a connection to the MySQL
-- server.
open' :: MySQL.ConnectInfo -> LogFunc -> IO SqlBackend
open' ci logFunc = do
    conn <- MySQL.connect ci
    MySQLBase.autocommit conn False -- disable autocommit!
    smap <- newIORef $ Map.empty
    return $ SqlBackend
        { connPrepare    = prepare' conn
        , connStmtMap    = smap
        , connInsertSql  = insertSql'
        , connInsertManySql = Nothing
        , connUpsertSql = Nothing
        , connPutManySql = Just putManySql
        , connClose      = MySQL.close conn
        , connMigrateSql = migrate' ci
        , connBegin      = \_ mIsolation -> do
            forM_ mIsolation $ \iso -> MySQL.execute_ conn (makeIsolationLevelStatement iso)
            MySQL.execute_ conn "start transaction" >> return ()
        , connCommit     = const $ MySQL.commit   conn
        , connRollback   = const $ MySQL.rollback conn
        , connEscapeName = pack . escapeDBName
        , connNoLimit    = "LIMIT 18446744073709551615"
        -- This noLimit is suggested by MySQL's own docs, see
        -- <http://dev.mysql.com/doc/refman/5.5/en/select.html>
        , connRDBMS      = "mysql"
        , connLimitOffset = decorateSQLWithLimitOffset "LIMIT 18446744073709551615"
        , connLogFunc    = logFunc
        , connMaxParams = Nothing
        , connRepsertManySql = Just repsertManySql
        }

-- | Prepare a query.  We don't support prepared statements, but
-- we'll do some client-side preprocessing here.
prepare' :: MySQL.Connection -> Text -> IO Statement
prepare' conn sql = do
    let query = MySQL.Query (T.encodeUtf8 sql)
    return Statement
        { stmtFinalize = return ()
        , stmtReset    = return ()
        , stmtExecute  = execute' conn query
        , stmtQuery    = withStmt' conn query
        }


-- | SQL code to be executed when inserting an entity.
insertSql' :: EntityDef -> [PersistValue] -> InsertSqlResult
insertSql' ent vals =
  let sql = pack $ concat
                [ "INSERT INTO "
                , escapeDBName $ entityDB ent
                , "("
                , intercalate "," $ map (escapeDBName . fieldDB) $ entityFields ent
                , ") VALUES("
                , intercalate "," (map (const "?") $ entityFields ent)
                , ")"
                ]
  in case entityPrimary ent of
       Just _ -> ISRManyKeys sql vals
       Nothing -> ISRInsertGet sql "SELECT LAST_INSERT_ID()"

-- | Execute an statement that doesn't return any results.
execute' :: MySQL.Connection -> MySQL.Query -> [PersistValue] -> IO Int64
execute' conn query vals = MySQL.execute conn query (map P vals)


-- | Execute an statement that does return results.  The results
-- are fetched all at once and stored into memory.
withStmt' :: MonadIO m
          => MySQL.Connection
          -> MySQL.Query
          -> [PersistValue]
          -> Acquire (ConduitM () [PersistValue] m ())
withStmt' conn query vals = do
    result <- mkAcquire createResult MySQLBase.freeResult
    return $ fetchRows result >>= CL.sourceList
  where
    createResult = do
      -- Execute the query
      formatted <- MySQL.formatQuery conn query (map P vals)
      MySQLBase.query conn formatted
      MySQLBase.storeResult conn

    fetchRows result = liftIO $ do
      -- Find out the type of the columns
      fields <- MySQLBase.fetchFields result
      let getters = [ maybe PersistNull (getGetter f f . Just) | f <- fields]
          convert = use getters
            where use (g:gs) (col:cols) =
                    let v  = g col
                        vs = use gs cols
                    in v `seq` vs `seq` (v:vs)
                  use _ _ = []

      -- Ready to go!
      let go acc = do
            row <- MySQLBase.fetchRow result
            case row of
              [] -> return (acc [])
              _  -> let converted = convert row
                    in converted `seq` go (acc . (converted:))
      go id


-- | @newtype@ around 'PersistValue' that supports the
-- 'MySQL.Param' type class.
newtype P = P PersistValue

instance MySQL.Param P where
    render (P (PersistText t))        = MySQL.render t
    render (P (PersistByteString bs)) = MySQL.render bs
    render (P (PersistInt64 i))       = MySQL.render i
    render (P (PersistDouble d))      = MySQL.render d
    render (P (PersistBool b))        = MySQL.render b
    render (P (PersistDay d))         = MySQL.render d
    render (P (PersistTimeOfDay t))   = MySQL.render t
    render (P (PersistUTCTime t))     = MySQL.render t
    render (P PersistNull)            = MySQL.render MySQL.Null
    render (P (PersistList l))        = MySQL.render $ listToJSON l
    render (P (PersistMap m))         = MySQL.render $ mapToJSON m
    render (P (PersistRational r))    =
      MySQL.Plain $ BBB.fromString $ show (fromRational r :: Pico)
      -- FIXME: Too Ambiguous, can not select precision without information about field
    render (P (PersistDbSpecific s))    = MySQL.Plain $ BBS.fromByteString s
    render (P (PersistArray a))       = MySQL.render (P (PersistList a))
    render (P (PersistObjectId _))    =
        error "Refusing to serialize a PersistObjectId to a MySQL value"


-- | @Getter a@ is a function that converts an incoming value
-- into a data type @a@.
type Getter a = MySQLBase.Field -> Maybe ByteString -> a

-- | Helper to construct 'Getter'@s@ using 'MySQL.Result'.
convertPV :: MySQL.Result a => (a -> b) -> Getter b
convertPV f = (f .) . MySQL.convert

-- | Get the corresponding @'Getter' 'PersistValue'@ depending on
-- the type of the column.
getGetter :: MySQLBase.Field -> Getter PersistValue
getGetter field = go (MySQLBase.fieldType field)
                        (MySQLBase.fieldLength field)
                            (MySQLBase.fieldCharSet field)
  where
    -- Bool
    go MySQLBase.Tiny       1 _ = convertPV PersistBool
    go MySQLBase.Tiny       _ _ = convertPV PersistInt64
    -- Int64
    go MySQLBase.Int24      _ _ = convertPV PersistInt64
    go MySQLBase.Short      _ _ = convertPV PersistInt64
    go MySQLBase.Long       _ _ = convertPV PersistInt64
    go MySQLBase.LongLong   _ _ = convertPV PersistInt64
    -- Double
    go MySQLBase.Float      _ _ = convertPV PersistDouble
    go MySQLBase.Double     _ _ = convertPV PersistDouble
    go MySQLBase.Decimal    _ _ = convertPV PersistDouble
    go MySQLBase.NewDecimal _ _ = convertPV PersistDouble

    -- ByteString and Text

    -- The MySQL C client (and by extension the Haskell mysql package) doesn't distinguish between binary and non-binary string data at the type level.
    -- (e.g. both BLOB and TEXT have the MySQLBase.Blob type).
    -- Instead, the character set distinguishes them. Binary data uses character set number 63.
    -- See https://dev.mysql.com/doc/refman/5.6/en/c-api-data-structures.html (Search for "63")
    go MySQLBase.VarChar    _ 63 = convertPV PersistByteString
    go MySQLBase.VarString  _ 63 = convertPV PersistByteString
    go MySQLBase.String     _ 63 = convertPV PersistByteString

    go MySQLBase.VarChar    _ _  = convertPV PersistText
    go MySQLBase.VarString  _ _  = convertPV PersistText
    go MySQLBase.String     _ _  = convertPV PersistText

    go MySQLBase.Blob       _ 63 = convertPV PersistByteString
    go MySQLBase.TinyBlob   _ 63 = convertPV PersistByteString
    go MySQLBase.MediumBlob _ 63 = convertPV PersistByteString
    go MySQLBase.LongBlob   _ 63 = convertPV PersistByteString

    go MySQLBase.Blob       _ _  = convertPV PersistText
    go MySQLBase.TinyBlob   _ _  = convertPV PersistText
    go MySQLBase.MediumBlob _ _  = convertPV PersistText
    go MySQLBase.LongBlob   _ _  = convertPV PersistText

    -- Time-related
    go MySQLBase.Time       _ _  = convertPV PersistTimeOfDay
    go MySQLBase.DateTime   _ _  = convertPV PersistUTCTime
    go MySQLBase.Timestamp  _ _  = convertPV PersistUTCTime
    go MySQLBase.Date       _ _  = convertPV PersistDay
    go MySQLBase.NewDate    _ _  = convertPV PersistDay
    go MySQLBase.Year       _ _  = convertPV PersistDay
    -- Null
    go MySQLBase.Null       _ _  = \_ _ -> PersistNull
    -- Controversial conversions
    go MySQLBase.Set        _ _  = convertPV PersistText
    go MySQLBase.Enum       _ _  = convertPV PersistText
    -- Conversion using PersistDbSpecific
    go MySQLBase.Geometry   _ _  = \_ m ->
      case m of
        Just g -> PersistDbSpecific g
        Nothing -> error "Unexpected null in database specific value"
    -- Unsupported
    go other _ _ = error $ "MySQL.getGetter: type " ++
                      show other ++ " not supported."



----------------------------------------------------------------------


-- | Create the migration plan for the given 'PersistEntity'
-- @val@.
migrate' :: MySQL.ConnectInfo
         -> [EntityDef]
         -> (Text -> IO Statement)
         -> EntityDef
         -> IO (Either [Text] [(Bool, Text)])
migrate' connectInfo allDefs getter val = do
    let name = entityDB val
    let (newcols, udefs, fdefs) = mysqlMkColumns allDefs val
    (idClmn, old) <- getColumns connectInfo getter val newcols
    let udspair = map udToPair udefs
    case (idClmn, old, partitionEithers old) of
      -- Nothing found, create everything
      ([], [], _) -> do
        let uniques = flip concatMap udspair $ \(uname, ucols) ->
                      [ AlterTable name $
                        AddUniqueConstraint uname $
                        map (findTypeAndMaxLen name) ucols ]
        let foreigns = do
              Column { cName=cname, cReference=Just (refTblName, refConstraintName) } <- newcols
              return $ AlterColumn name (refTblName, addReference allDefs refConstraintName refTblName cname)

        let foreignsAlt = map (\fdef -> let (childfields, parentfields) = unzip (map (\((_,b),(_,d)) -> (b,d)) (foreignFields fdef))
                                        in AlterColumn name (foreignRefTableDBName fdef, AddReference (foreignRefTableDBName fdef) (foreignConstraintNameDBName fdef) childfields parentfields)) fdefs

        return $ Right $ map showAlterDb $ (addTable newcols val): uniques ++ foreigns ++ foreignsAlt
      -- No errors and something found, migrate
      (_, _, ([], old')) -> do
        let excludeForeignKeys (xs,ys) = (map (\c -> case cReference c of
                                                    Just (_,fk) -> case find (\f -> fk == foreignConstraintNameDBName f) fdefs of
                                                                     Just _ -> c { cReference = Nothing }
                                                                     Nothing -> c
                                                    Nothing -> c) xs,ys)
            (acs, ats) = getAlters allDefs name (newcols, udspair) $ excludeForeignKeys $ partitionEithers old'
            acs' = map (AlterColumn name) acs
            ats' = map (AlterTable  name) ats
        return $ Right $ map showAlterDb $ acs' ++ ats'
      -- Errors
      (_, _, (errs, _)) -> return $ Left errs

      where
        findTypeAndMaxLen tblName col = let (col', ty) = findTypeOfColumn allDefs tblName col
                                            (_, ml) = findMaxLenOfColumn allDefs tblName col
                                         in (col', ty, ml)

addTable :: [Column] -> EntityDef -> AlterDB
addTable cols entity = AddTable $ concat
           -- Lower case e: see Database.Persist.Sql.Migration
           [ "CREATe TABLE "
           , escapeDBName name
           , "("
           , idtxt
           , if null cols then [] else ","
           , intercalate "," $ map showColumn cols
           , ")"
           ]
    where
      name = entityDB entity
      idtxt = case entityPrimary entity of
                Just pdef -> concat [" PRIMARY KEY (", intercalate "," $ map (escapeDBName . fieldDB) $ compositeFields pdef, ")"]
                Nothing ->
                  let defText = defaultAttribute $ fieldAttrs $ entityId entity
                      sType = fieldSqlType $ entityId entity
                      autoIncrementText = case (sType, defText) of
                        (SqlInt64, Nothing) -> " AUTO_INCREMENT"
                        _ -> ""
                      maxlen = findMaxLenOfField (entityId entity)
                  in concat
                         [ escapeDBName $ fieldDB $ entityId entity
                         , " " <> showSqlType sType maxlen False
                         , " NOT NULL"
                         , autoIncrementText
                         , " PRIMARY KEY"
                         ]

-- | Find out the type of a column.
findTypeOfColumn :: [EntityDef] -> DBName -> DBName -> (DBName, FieldType)
findTypeOfColumn allDefs name col =
    maybe (error $ "Could not find type of column " ++
                   show col ++ " on table " ++ show name ++
                   " (allDefs = " ++ show allDefs ++ ")")
          ((,) col) $ do
            entDef   <- find ((== name) . entityDB) allDefs
            fieldDef <- find ((== col)  . fieldDB) (entityFields entDef)
            return (fieldType fieldDef)

-- | Find out the maxlen of a column (default to 200)
findMaxLenOfColumn :: [EntityDef] -> DBName -> DBName -> (DBName, Integer)
findMaxLenOfColumn allDefs name col =
   maybe (col, 200)
         ((,) col) $ do
           entDef     <- find ((== name) . entityDB) allDefs
           fieldDef   <- find ((== col) . fieldDB) (entityFields entDef)
           findMaxLenOfField fieldDef

-- | Find out the maxlen of a field
findMaxLenOfField :: FieldDef -> Maybe Integer
findMaxLenOfField fieldDef = do
    maxLenAttr <- find ((T.isPrefixOf "maxlen=") . T.toLower) (fieldAttrs fieldDef)
    readMaybe . T.unpack . T.drop 7 $ maxLenAttr

-- | Helper for 'AddReference' that finds out the which primary key columns to reference.
addReference :: [EntityDef] -> DBName -> DBName -> DBName -> AlterColumn
addReference allDefs fkeyname reftable cname = AddReference reftable fkeyname [cname] referencedColumns
    where
      referencedColumns = maybe (error $ "Could not find ID of entity " ++ show reftable
                                  ++ " (allDefs = " ++ show allDefs ++ ")")
                                id $ do
                                  entDef <- find ((== reftable) . entityDB) allDefs
                                  return $ map fieldDB $ entityKeyFields entDef

data AlterColumn = Change Column
                 | Add' Column
                 | Drop
                 | Default String
                 | NoDefault
                 | Update' String
                 -- | See the definition of the 'showAlter' function to see how these fields are used.
                 | AddReference
                    DBName -- Referenced table
                    DBName -- Foreign key name
                    [DBName] -- Referencing columns
                    [DBName] -- Referenced columns
                 | DropReference DBName

type AlterColumn' = (DBName, AlterColumn)

data AlterTable = AddUniqueConstraint DBName [(DBName, FieldType, Integer)]
                | DropUniqueConstraint DBName

data AlterDB = AddTable String
             | AlterColumn DBName AlterColumn'
             | AlterTable DBName AlterTable


udToPair :: UniqueDef -> (DBName, [DBName])
udToPair ud = (uniqueDBName ud, map snd $ uniqueFields ud)

----------------------------------------------------------------------


-- | Returns all of the 'Column'@s@ in the given table currently
-- in the database.
getColumns :: MySQL.ConnectInfo
           -> (Text -> IO Statement)
           -> EntityDef -> [Column]
           -> IO ( [Either Text (Either Column (DBName, [DBName]))] -- ID column
                 , [Either Text (Either Column (DBName, [DBName]))] -- everything else
                 )
getColumns connectInfo getter def cols = do
    -- Find out ID column.
    stmtIdClmn <- getter $ T.concat
      [ "SELECT COLUMN_NAME, "
      ,   "IS_NULLABLE, "
      ,   "DATA_TYPE, "
      ,   "COLUMN_DEFAULT "
      , "FROM INFORMATION_SCHEMA.COLUMNS "
      , "WHERE TABLE_SCHEMA = ? "
      ,   "AND TABLE_NAME   = ? "
      ,   "AND COLUMN_NAME  = ?"
      ]
    inter1 <- with (stmtQuery stmtIdClmn vals) (\src -> runConduit $ src .| CL.consume)
    ids <- runConduitRes $ CL.sourceList inter1 .| helperClmns -- avoid nested queries

    -- Find out all columns.
    stmtClmns <- getter $ T.concat
      [ "SELECT COLUMN_NAME, "
      ,   "IS_NULLABLE, "
      ,   "DATA_TYPE, "
      ,   "COLUMN_TYPE, "
      ,   "CHARACTER_MAXIMUM_LENGTH, "
      ,   "NUMERIC_PRECISION, "
      ,   "NUMERIC_SCALE, "
      ,   "COLUMN_DEFAULT "
      , "FROM INFORMATION_SCHEMA.COLUMNS "
      , "WHERE TABLE_SCHEMA = ? "
      ,   "AND TABLE_NAME   = ? "
      ,   "AND COLUMN_NAME <> ?"
      ]
    inter2 <- with (stmtQuery stmtClmns vals) (\src -> runConduitRes $ src .| CL.consume)
    cs <- runConduitRes $ CL.sourceList inter2 .| helperClmns -- avoid nested queries

    -- Find out the constraints.
    stmtCntrs <- getter $ T.concat
      [ "SELECT CONSTRAINT_NAME, "
      ,   "COLUMN_NAME "
      , "FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE "
      , "WHERE TABLE_SCHEMA = ? "
      ,   "AND TABLE_NAME   = ? "
      ,   "AND COLUMN_NAME <> ? "
      ,   "AND CONSTRAINT_NAME <> 'PRIMARY' "
      ,   "AND REFERENCED_TABLE_SCHEMA IS NULL "
      , "ORDER BY CONSTRAINT_NAME, "
      ,   "COLUMN_NAME"
      ]
    us <- with (stmtQuery stmtCntrs vals) (\src -> runConduitRes $ src .| helperCntrs)

    -- Return both
    return (ids, cs ++ us)
  where
    refMap = Map.fromList $ foldl ref [] cols
      where ref rs c = case cReference c of
                Nothing -> rs
                (Just r) -> (unDBName $ cName c, r) : rs
    vals = [ PersistText $ pack $ MySQL.connectDatabase connectInfo
           , PersistText $ unDBName $ entityDB def
           , PersistText $ unDBName $ fieldDB $ entityId def ]

    helperClmns = CL.mapM getIt .| CL.consume
        where
          getIt row = fmap (either Left (Right . Left)) .
                      liftIO .
                      getColumn connectInfo getter (entityDB def) row $ ref
            where ref = case row of
                    (PersistText cname : _) -> (Map.lookup cname refMap)
                    _ -> Nothing

    helperCntrs = do
      let check [ PersistText cntrName
                , PersistText clmnName] = return ( cntrName, clmnName )
          check other = fail $ "helperCntrs: unexpected " ++ show other
      rows <- mapM check =<< CL.consume
      return $ map (Right . Right . (DBName . fst . head &&& map (DBName . snd)))
             $ groupBy ((==) `on` fst) rows


-- | Get the information about a column in a table.
getColumn :: MySQL.ConnectInfo
          -> (Text -> IO Statement)
          -> DBName
          -> [PersistValue]
          -> Maybe (DBName, DBName)
          -> IO (Either Text Column)
getColumn connectInfo getter tname [ PersistText cname
                                   , PersistText null_
                                   , PersistText dataType
                                   , PersistText colType
                                   , colMaxLen
                                   , colPrecision
                                   , colScale
                                   , default'] refName =
    fmap (either (Left . pack) Right) $
    runExceptT $ do
      -- Default value
      default_ <- case default' of
                    PersistNull   -> return Nothing
                    PersistText t -> return (Just t)
                    PersistByteString bs ->
                      case T.decodeUtf8' bs of
                        Left exc -> fail $ "Invalid default column: " ++
                                           show default' ++ " (error: " ++
                                           show exc ++ ")"
                        Right t  -> return (Just t)
                    _ -> fail $ "Invalid default column: " ++ show default'

      ref <- getRef refName
      let colMaxLen' = case colMaxLen of
            PersistInt64 l -> Just (fromIntegral l)
            _ -> Nothing
          ci = ColumnInfo
            { ciColumnType = colType
            , ciMaxLength = colMaxLen'
            , ciNumericPrecision = colPrecision
            , ciNumericScale = colScale
            }
      (typ, maxLen) <- parseColumnType dataType ci
      -- Okay!
      return Column
        { cName = DBName $ cname
        , cNull = null_ == "YES"
        , cSqlType = typ
        , cDefault = default_
        , cDefaultConstraintName = Nothing
        , cMaxLen = maxLen
        , cReference = ref
        }
  where getRef Nothing = return Nothing
        getRef (Just (_, refName')) = do
          -- Foreign key (if any)
          stmt <- lift . getter $ T.concat
            [ "SELECT REFERENCED_TABLE_NAME, "
            ,   "CONSTRAINT_NAME, "
            ,   "ORDINAL_POSITION "
            , "FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE "
            , "WHERE TABLE_SCHEMA = ? "
            ,   "AND TABLE_NAME   = ? "
            ,   "AND COLUMN_NAME  = ? "
            ,   "AND REFERENCED_TABLE_SCHEMA = ? "
            ,   "AND CONSTRAINT_NAME = ? "
            , "ORDER BY CONSTRAINT_NAME, "
            ,   "COLUMN_NAME"
            ]
          let vars = [ PersistText $ pack $ MySQL.connectDatabase connectInfo
                     , PersistText $ unDBName $ tname
                     , PersistText cname
                     , PersistText $ pack $ MySQL.connectDatabase connectInfo
                     , PersistText $ unDBName refName' ]
          cntrs <- liftIO $ with (stmtQuery stmt vars) (\src -> runConduit $ src .| CL.consume)
          case cntrs of
            [] -> return Nothing
            [[PersistText tab, PersistText ref, PersistInt64 pos]] ->
              return $ if pos == 1 then Just (DBName tab, DBName ref) else Nothing
            xs -> error $ mconcat
              [ "MySQL.getColumn/getRef: error fetching constraints. Expected a single result for foreign key query for table: "
              , T.unpack (unDBName tname)
              , " and column: "
              , T.unpack cname
              , " but got: "
              , show xs
              ]


getColumn _ _ _ x  _ =
    return $ Left $ pack $ "Invalid result from INFORMATION_SCHEMA: " ++ show x

-- | Extra column information from MySQL schema
data ColumnInfo = ColumnInfo
  { ciColumnType :: Text
  , ciMaxLength :: Maybe Integer
  , ciNumericPrecision :: PersistValue
  , ciNumericScale :: PersistValue
  }

-- | Parse the type of column as returned by MySQL's
-- @INFORMATION_SCHEMA@ tables.
parseColumnType :: Text -> ColumnInfo -> ExceptT String IO (SqlType, Maybe Integer)
-- Ints
parseColumnType "tinyint" ci | ciColumnType ci == "tinyint(1)" = return (SqlBool, Nothing)
parseColumnType "int" ci | ciColumnType ci == "int(11)"        = return (SqlInt32, Nothing)
parseColumnType "bigint" ci | ciColumnType ci == "bigint(20)"  = return (SqlInt64, Nothing)
-- Double
parseColumnType x@("double") ci | ciColumnType ci == x         = return (SqlReal, Nothing)
parseColumnType "decimal" ci                                   =
  case (ciNumericPrecision ci, ciNumericScale ci) of
    (PersistInt64 p, PersistInt64 s) ->
      return (SqlNumeric (fromIntegral p) (fromIntegral s), Nothing)
    _ ->
      fail "missing DECIMAL precision in DB schema"
-- Text
parseColumnType "varchar" ci                                   = return (SqlString, ciMaxLength ci)
parseColumnType "text" _                                       = return (SqlString, Nothing)
-- ByteString
parseColumnType "varbinary" ci                                 = return (SqlBlob, ciMaxLength ci)
parseColumnType "blob" _                                       = return (SqlBlob, Nothing)
-- Time-related
parseColumnType "time" _                                       = return (SqlTime, Nothing)
parseColumnType "datetime" _                                   = return (SqlDayTime, Nothing)
parseColumnType "date" _                                       = return (SqlDay, Nothing)

parseColumnType _ ci                                           = return (SqlOther (ciColumnType ci), Nothing)


----------------------------------------------------------------------


-- | @getAlters allDefs tblName new old@ finds out what needs to
-- be changed from @old@ to become @new@.
getAlters :: [EntityDef]
          -> DBName
          -> ([Column], [(DBName, [DBName])])
          -> ([Column], [(DBName, [DBName])])
          -> ([AlterColumn'], [AlterTable])
getAlters allDefs tblName (c1, u1) (c2, u2) =
    (getAltersC c1 c2, getAltersU u1 u2)
  where
    getAltersC [] old = concatMap dropColumn old
    getAltersC (new:news) old =
        let (alters, old') = findAlters tblName allDefs new old
         in alters ++ getAltersC news old'

    dropColumn col =
      map ((,) (cName col)) $
        [DropReference n | Just (_, n) <- [cReference col]] ++
        [Drop]

    getAltersU [] old = map (DropUniqueConstraint . fst) old
    getAltersU ((name, cols):news) old =
        case lookup name old of
            Nothing ->
                AddUniqueConstraint name (map findTypeAndMaxLen cols) : getAltersU news old
            Just ocols ->
                let old' = filter (\(x, _) -> x /= name) old
                 in if sort cols == ocols
                        then getAltersU news old'
                        else  DropUniqueConstraint name
                            : AddUniqueConstraint name (map findTypeAndMaxLen cols)
                            : getAltersU news old'
        where
          findTypeAndMaxLen col = let (col', ty) = findTypeOfColumn allDefs tblName col
                                      (_, ml) = findMaxLenOfColumn allDefs tblName col
                                   in (col', ty, ml)


-- | @findAlters newColumn oldColumns@ finds out what needs to be
-- changed in the columns @oldColumns@ for @newColumn@ to be
-- supported.
findAlters :: DBName -> [EntityDef] -> Column -> [Column] -> ([AlterColumn'], [Column])
findAlters tblName allDefs col@(Column name isNull type_ def _defConstraintName maxLen ref) cols =
    case filter ((name ==) . cName) cols of
    -- new fkey that didn't exist before
        [] -> case ref of
               Nothing -> ([(name, Add' col)],[])
               Just (tname, cname) -> let cnstr = [addReference allDefs cname tname name]
                                  in (map ((,) tname) (Add' col : cnstr), cols)
        Column _ isNull' type_' def' _defConstraintName' maxLen' ref':_ ->
            let -- Foreign key
                refDrop = case (ref == ref', ref') of
                            (False, Just (_, cname)) -> [(name, DropReference cname)]
                            _ -> []
                refAdd  = case (ref == ref', ref) of
                            (False, Just (tname, cname)) -> [(tname, addReference allDefs cname tname name)]
                            _ -> []
                -- Type and nullability
                modType | showSqlType type_ maxLen False `ciEquals` showSqlType type_' maxLen' False && isNull == isNull' = []
                        | otherwise = [(name, Change col)]
                -- Default value
                -- Avoid DEFAULT NULL, since it is always unnecessary, and is an error for text/blob fields
                modDef | def == def' = []
                       | otherwise   = case def of
                                         Nothing -> [(name, NoDefault)]
                                         Just s -> if T.toUpper s == "NULL" then []
                                                   else [(name, Default $ T.unpack s)]
            in ( refDrop ++ modType ++ modDef ++ refAdd
               , filter ((name /=) . cName) cols )

  where
    ciEquals x y = T.toCaseFold (T.pack x) == T.toCaseFold (T.pack y)

----------------------------------------------------------------------


-- | Prints the part of a @CREATE TABLE@ statement about a given
-- column.
showColumn :: Column -> String
showColumn (Column n nu t def _defConstraintName maxLen ref) = concat
    [ escapeDBName n
    , " "
    , showSqlType t maxLen True
    , " "
    , if nu then "NULL" else "NOT NULL"
    , case def of
        Nothing -> ""
        Just s -> -- Avoid DEFAULT NULL, since it is always unnecessary, and is an error for text/blob fields
                  if T.toUpper s == "NULL" then ""
                  else " DEFAULT " ++ T.unpack s
    , case ref of
        Nothing -> ""
        Just (s, _) -> " REFERENCES " ++ escapeDBName s
    ]


-- | Renders an 'SqlType' in MySQL's format.
showSqlType :: SqlType
            -> Maybe Integer -- ^ @maxlen@
            -> Bool -- ^ include character set information?
            -> String
showSqlType SqlBlob    Nothing    _     = "BLOB"
showSqlType SqlBlob    (Just i)   _     = "VARBINARY(" ++ show i ++ ")"
showSqlType SqlBool    _          _     = "TINYINT(1)"
showSqlType SqlDay     _          _     = "DATE"
showSqlType SqlDayTime _          _     = "DATETIME"
showSqlType SqlInt32   _          _     = "INT(11)"
showSqlType SqlInt64   _          _     = "BIGINT"
showSqlType SqlReal    _          _     = "DOUBLE"
showSqlType (SqlNumeric s prec) _ _     = "NUMERIC(" ++ show s ++ "," ++ show prec ++ ")"
showSqlType SqlString  Nothing    True  = "TEXT CHARACTER SET utf8mb4"
showSqlType SqlString  Nothing    False = "TEXT"
showSqlType SqlString  (Just i)   True  = "VARCHAR(" ++ show i ++ ") CHARACTER SET utf8mb4"
showSqlType SqlString  (Just i)   False = "VARCHAR(" ++ show i ++ ")"
showSqlType SqlTime    _          _     = "TIME"
showSqlType (SqlOther t) _        _     = T.unpack t

-- | Render an action that must be done on the database.
showAlterDb :: AlterDB -> (Bool, Text)
showAlterDb (AddTable s) = (False, pack s)
showAlterDb (AlterColumn t (c, ac)) =
    (isUnsafe ac, pack $ showAlter t (c, ac))
  where
    isUnsafe Drop = True
    isUnsafe _    = False
showAlterDb (AlterTable t at) = (False, pack $ showAlterTable t at)


-- | Render an action that must be done on a table.
showAlterTable :: DBName -> AlterTable -> String
showAlterTable table (AddUniqueConstraint cname cols) = concat
    [ "ALTER TABLE "
    , escapeDBName table
    , " ADD CONSTRAINT "
    , escapeDBName cname
    , " UNIQUE("
    , intercalate "," $ map escapeDBName' cols
    , ")"
    ]
    where
      escapeDBName' (name, (FTTypeCon _ "Text"      ), maxlen) = escapeDBName name ++ "(" ++ show maxlen ++ ")"
      escapeDBName' (name, (FTTypeCon _ "String"    ), maxlen) = escapeDBName name ++ "(" ++ show maxlen ++ ")"
      escapeDBName' (name, (FTTypeCon _ "ByteString"), maxlen) = escapeDBName name ++ "(" ++ show maxlen ++ ")"
      escapeDBName' (name, _                         , _) = escapeDBName name
showAlterTable table (DropUniqueConstraint cname) = concat
    [ "ALTER TABLE "
    , escapeDBName table
    , " DROP INDEX "
    , escapeDBName cname
    ]


-- | Render an action that must be done on a column.
showAlter :: DBName -> AlterColumn' -> String
showAlter table (oldName, Change (Column n nu t def defConstraintName maxLen _ref)) =
    concat
    [ "ALTER TABLE "
    , escapeDBName table
    , " CHANGE "
    , escapeDBName oldName
    , " "
    , showColumn (Column n nu t def defConstraintName maxLen Nothing)
    ]
showAlter table (_, Add' col) =
    concat
    [ "ALTER TABLE "
    , escapeDBName table
    , " ADD COLUMN "
    , showColumn col
    ]
showAlter table (n, Drop) =
    concat
    [ "ALTER TABLE "
    , escapeDBName table
    , " DROP COLUMN "
    , escapeDBName n
    ]
showAlter table (n, Default s) =
    concat
    [ "ALTER TABLE "
    , escapeDBName table
    , " ALTER COLUMN "
    , escapeDBName n
    , " SET DEFAULT "
    , s
    ]
showAlter table (n, NoDefault) =
    concat
    [ "ALTER TABLE "
    , escapeDBName table
    , " ALTER COLUMN "
    , escapeDBName n
    , " DROP DEFAULT"
    ]
showAlter table (n, Update' s) =
    concat
    [ "UPDATE "
    , escapeDBName table
    , " SET "
    , escapeDBName n
    , "="
    , s
    , " WHERE "
    , escapeDBName n
    , " IS NULL"
    ]
showAlter table (_, AddReference reftable fkeyname t2 id2) = concat
    [ "ALTER TABLE "
    , escapeDBName table
    , " ADD CONSTRAINT "
    , escapeDBName fkeyname
    , " FOREIGN KEY("
    , intercalate "," $ map escapeDBName t2
    , ") REFERENCES "
    , escapeDBName reftable
    , "("
    , intercalate "," $ map escapeDBName id2
    , ")"
    ]
showAlter table (_, DropReference cname) = concat
    [ "ALTER TABLE "
    , escapeDBName table
    , " DROP FOREIGN KEY "
    , escapeDBName cname
    ]

----------------------------------------------------------------------

escape :: DBName -> Text
escape = T.pack . escapeDBName

-- | Escape a database name to be included on a query.
escapeDBName :: DBName -> String
escapeDBName (DBName s) = '`' : go (T.unpack s)
    where
      go ('`':xs) = '`' : '`' : go xs
      go ( x :xs) =     x     : go xs
      go ""       = "`"

-- | Information required to connect to a MySQL database
-- using @persistent@'s generic facilities.  These values are the
-- same that are given to 'withMySQLPool'.
data MySQLConf = MySQLConf
    { myConnInfo :: MySQL.ConnectInfo
      -- ^ The connection information.
    , myPoolSize :: Int
      -- ^ How many connections should be held on the connection pool.
    } deriving Show

instance FromJSON MySQLConf where
    parseJSON v = modifyFailure ("Persistent: error loading MySQL conf: " ++) $
      flip (withObject "MySQLConf") v $ \o -> do
        database <- o .: "database"
        host     <- o .: "host"
        port     <- o .: "port"
        path     <- o .:? "path"
        user     <- o .: "user"
        password <- o .: "password"
        pool     <- o .: "poolsize"
        let ci = MySQL.defaultConnectInfo
                   { MySQL.connectHost     = host
                   , MySQL.connectPort     = port
                   , MySQL.connectPath     = case path of
                         Just p  -> p
                         Nothing -> MySQL.connectPath MySQL.defaultConnectInfo
                   , MySQL.connectUser     = user
                   , MySQL.connectPassword = password
                   , MySQL.connectDatabase = database
                   }
        return $ MySQLConf ci pool

instance PersistConfig MySQLConf where
    type PersistConfigBackend MySQLConf = SqlPersistT

    type PersistConfigPool    MySQLConf = ConnectionPool

    createPoolConfig (MySQLConf cs size) = runNoLoggingT $ createMySQLPool cs size -- FIXME

    runPool _ = runSqlPool

    loadConfig = parseJSON

    applyEnv conf = do
        env <- getEnvironment
        let maybeEnv old var = maybe old id $ lookup ("MYSQL_" ++ var) env
        return conf
          { myConnInfo =
              case myConnInfo conf of
                MySQL.ConnectInfo
                  { MySQL.connectHost     = host
                  , MySQL.connectPort     = port
                  , MySQL.connectPath     = path
                  , MySQL.connectUser     = user
                  , MySQL.connectPassword = password
                  , MySQL.connectDatabase = database
                  } -> (myConnInfo conf)
                         { MySQL.connectHost     = maybeEnv host "HOST"
                         , MySQL.connectPort     = read $ maybeEnv (show port) "PORT"
                         , MySQL.connectPath     = maybeEnv path "PATH"
                         , MySQL.connectUser     = maybeEnv user "USER"
                         , MySQL.connectPassword = maybeEnv password "PASSWORD"
                         , MySQL.connectDatabase = maybeEnv database "DATABASE"
                         }
          }

mockMigrate :: MySQL.ConnectInfo
         -> [EntityDef]
         -> (Text -> IO Statement)
         -> EntityDef
         -> IO (Either [Text] [(Bool, Text)])
mockMigrate _connectInfo allDefs _getter val = do
    let name = entityDB val
    let (newcols, udefs, fdefs) = mysqlMkColumns allDefs val
    let udspair = map udToPair udefs
    case () of
      -- Nothing found, create everything
      () -> do
        let uniques = flip concatMap udspair $ \(uname, ucols) ->
                      [ AlterTable name $
                        AddUniqueConstraint uname $
                        map (findTypeAndMaxLen name) ucols ]
        let foreigns = do
              Column { cName=cname, cReference=Just (refTblName, refConstraintName) } <- newcols
              return $ AlterColumn name (refTblName, addReference allDefs refConstraintName refTblName cname)

        let foreignsAlt = map (\fdef -> let (childfields, parentfields) = unzip (map (\((_,b),(_,d)) -> (b,d)) (foreignFields fdef))
                                        in AlterColumn name (foreignRefTableDBName fdef, AddReference (foreignRefTableDBName fdef) (foreignConstraintNameDBName fdef) childfields parentfields)) fdefs

        return $ Right $ map showAlterDb $ (addTable newcols val): uniques ++ foreigns ++ foreignsAlt
    {- FIXME redundant, why is this here? The whole case expression is weird
      -- No errors and something found, migrate
      (_, _, ([], old')) -> do
        let excludeForeignKeys (xs,ys) = (map (\c -> case cReference c of
                                                    Just (_,fk) -> case find (\f -> fk == foreignConstraintNameDBName f) fdefs of
                                                                     Just _ -> c { cReference = Nothing }
                                                                     Nothing -> c
                                                    Nothing -> c) xs,ys)
            (acs, ats) = getAlters allDefs name (newcols, udspair) $ excludeForeignKeys $ partitionEithers old'
            acs' = map (AlterColumn name) acs
            ats' = map (AlterTable  name) ats
        return $ Right $ map showAlterDb $ acs' ++ ats'
      -- Errors
      (_, _, (errs, _)) -> return $ Left errs
    -}

      where
        findTypeAndMaxLen tblName col = let (col', ty) = findTypeOfColumn allDefs tblName col
                                            (_, ml) = findMaxLenOfColumn allDefs tblName col
                                         in (col', ty, ml)


-- | Mock a migration even when the database is not present.
-- This function will mock the migration for a database even when
-- the actual database isn't already present in the system.
mockMigration :: Migration -> IO ()
mockMigration mig = do
  smap <- newIORef $ Map.empty
  let sqlbackend = SqlBackend { connPrepare = \_ -> do
                                             return Statement
                                                        { stmtFinalize = return ()
                                                        , stmtReset = return ()
                                                        , stmtExecute = undefined
                                                        , stmtQuery = \_ -> return $ return ()
                                                        },
                             connInsertManySql = Nothing,
                             connInsertSql = undefined,
                             connStmtMap = smap,
                             connClose = undefined,
                             connMigrateSql = mockMigrate undefined,
                             connBegin = undefined,
                             connCommit = undefined,
                             connRollback = undefined,
                             connEscapeName = undefined,
                             connNoLimit = undefined,
                             connRDBMS = undefined,
                             connLimitOffset = undefined,
                             connLogFunc = undefined,
                             connUpsertSql = undefined,
                             connPutManySql = undefined,
                             connMaxParams = Nothing,
                             connRepsertManySql = Nothing
                             }
      result = runReaderT . runWriterT . runWriterT $ mig
  resp <- result sqlbackend
  mapM_ T.putStrLn $ map snd $ snd resp

-- | MySQL specific 'upsert_'. This will prevent multiple queries, when one will
-- do. The record will be inserted into the database. In the event that the
-- record already exists in the database, the record will have the
-- relevant updates performed.
insertOnDuplicateKeyUpdate
  :: ( backend ~ PersistEntityBackend record
     , PersistEntity record
     , MonadIO m
     , PersistStore backend
     , BackendCompatible SqlBackend backend
     )
  => record
  -> [Update record]
  -> ReaderT backend m ()
insertOnDuplicateKeyUpdate record =
  insertManyOnDuplicateKeyUpdate [record] []

-- | This type is used to determine how to update rows using MySQL's
-- @INSERT ... ON DUPLICATE KEY UPDATE@ functionality, exposed via
-- 'insertManyOnDuplicateKeyUpdate' in this library.
--
-- @since 2.8.0
data HandleUpdateCollision record where
  -- | Copy the field directly from the record.
  CopyField :: EntityField record typ -> HandleUpdateCollision record
  -- | Only copy the field if it is not equal to the provided value.
  CopyUnlessEq :: PersistField typ => EntityField record typ -> typ -> HandleUpdateCollision record

-- | An alias for 'HandleUpdateCollision'. The type previously was only
-- used to copy a single value, but was expanded to be handle more complex
-- queries.
--
-- @since 2.6.2
type SomeField = HandleUpdateCollision

pattern SomeField :: EntityField record typ -> SomeField record
pattern SomeField x = CopyField x
{-# DEPRECATED SomeField "The type SomeField is deprecated. Use the type HandleUpdateCollision instead, and use the function copyField instead of the data constructor." #-}

-- | Copy the field into the database only if the value in the
-- corresponding record is non-@NULL@.
--
-- @since  2.6.2
copyUnlessNull :: PersistField typ => EntityField record (Maybe typ) -> HandleUpdateCollision record
copyUnlessNull field = CopyUnlessEq field Nothing

-- | Copy the field into the database only if the value in the
-- corresponding record is non-empty, where "empty" means the Monoid
-- definition for 'mempty'. Useful for 'Text', 'String', 'ByteString', etc.
--
-- The resulting 'HandleUpdateCollision' type is useful for the
-- 'insertManyOnDuplicateKeyUpdate' function.
--
-- @since  2.6.2
copyUnlessEmpty :: (Monoid.Monoid typ, PersistField typ) => EntityField record typ -> HandleUpdateCollision record
copyUnlessEmpty field = CopyUnlessEq field Monoid.mempty

-- | Copy the field into the database only if the field is not equal to the
-- provided value. This is useful to avoid copying weird nullary data into
-- the database.
--
-- The resulting 'HandleUpdateCollision' type is useful for the
-- 'insertManyOnDuplicateKeyUpdate' function.
--
-- @since  2.6.2
copyUnlessEq :: PersistField typ => EntityField record typ -> typ -> HandleUpdateCollision record
copyUnlessEq = CopyUnlessEq

-- | Copy the field directly from the record.
--
-- @since 3.0
copyField :: PersistField typ => EntityField record typ -> HandleUpdateCollision record
copyField = CopyField

-- | Do a bulk insert on the given records in the first parameter. In the event
-- that a key conflicts with a record currently in the database, the second and
-- third parameters determine what will happen.
--
-- The second parameter is a list of fields to copy from the original value.
-- This allows you to specify which fields to copy from the record you're trying
-- to insert into the database to the preexisting row.
--
-- The third parameter is a list of updates to perform that are independent of
-- the value that is provided. You can use this to increment a counter value.
-- These updates only occur if the original record is present in the database.
--
-- === __More details on 'HandleUpdateCollision' usage__
--
-- The @['HandleUpdateCollision']@ parameter allows you to specify which fields (and
-- under which conditions) will be copied from the inserted rows. For
-- a brief example, consider the following data model and existing data set:
--
-- @
-- Item
--   name        Text
--   description Text
--   price       Double Maybe
--   quantity    Int Maybe
--
--   Primary name
-- @
--
-- > items:
-- > +------+-------------+-------+----------+
-- > | name | description | price | quantity |
-- > +------+-------------+-------+----------+
-- > | foo  | very good   |       |    3     |
-- > | bar  |             |  3.99 |          |
-- > +------+-------------+-------+----------+
--
-- This record type has a single natural key on @itemName@. Let's suppose
-- that we download a CSV of new items to store into the database. Here's
-- our CSV:
--
-- > name,description,price,quantity
-- > foo,,2.50,6
-- > bar,even better,,5
-- > yes,wow,,
--
-- We parse that into a list of Haskell records:
--
-- @
-- records =
--   [ Item { itemName = "foo", itemDescription = ""
--          , itemPrice = Just 2.50, itemQuantity = Just 6
--          }
--   , Item "bar" "even better" Nothing (Just 5)
--   , Item "yes" "wow" Nothing Nothing
--   ]
-- @
--
-- The new CSV data is partial. It only includes __updates__ from the
-- upstream vendor. Our CSV library parses the missing description field as
-- an empty string. We don't want to override the existing description. So
-- we can use the 'copyUnlessEmpty' function to say: "Don't update when the
-- value is empty."
--
-- Likewise, the new row for @bar@ includes a quantity, but no price. We do
-- not want to overwrite the existing price in the database with a @NULL@
-- value. So we can use 'copyUnlessNull' to only copy the existing values
-- in.
--
-- The final code looks like this:
-- @
-- 'insertManyOnDuplicateKeyUpdate' records
--   [ 'copyUnlessEmpty' ItemDescription
--   , 'copyUnlessNull' ItemPrice
--   , 'copyUnlessNull' ItemQuantity
--   ]
--   []
-- @
--
-- Once we run that code on the datahase, the new data set looks like this:
--
-- > items:
-- > +------+-------------+-------+----------+
-- > | name | description | price | quantity |
-- > +------+-------------+-------+----------+
-- > | foo  | very good   |  2.50 |    6     |
-- > | bar  | even better |  3.99 |    5     |
-- > | yes  | wow         |       |          |
-- > +------+-------------+-------+----------+
insertManyOnDuplicateKeyUpdate
    :: forall record backend m.
    ( backend ~ PersistEntityBackend record
    , BackendCompatible SqlBackend backend
    , PersistEntity record
    , MonadIO m
    )
    => [record] -- ^ A list of the records you want to insert, or update
    -> [HandleUpdateCollision record] -- ^ A list of the fields you want to copy over.
    -> [Update record] -- ^ A list of the updates to apply that aren't dependent on the record being inserted.
    -> ReaderT backend m ()
insertManyOnDuplicateKeyUpdate [] _ _ = return ()
insertManyOnDuplicateKeyUpdate records fieldValues updates =
    uncurry rawExecute
    $ mkBulkInsertQuery records fieldValues updates

-- | This creates the query for 'bulkInsertOnDuplicateKeyUpdate'. If you
-- provide an empty list of updates to perform, then it will generate
-- a dummy/no-op update using the first field of the record. This avoids
-- duplicate key exceptions.
mkBulkInsertQuery
    :: PersistEntity record
    => [record] -- ^ A list of the records you want to insert, or update
    -> [HandleUpdateCollision record] -- ^ A list of the fields you want to copy over.
    -> [Update record] -- ^ A list of the updates to apply that aren't dependent on the record being inserted.
    -> (Text, [PersistValue])
mkBulkInsertQuery records fieldValues updates =
    (q, recordValues <> updsValues <> copyUnlessValues)
  where
    mfieldDef x = case x of
        CopyField rec -> Right (fieldDbToText (persistFieldDef rec))
        CopyUnlessEq rec val -> Left (fieldDbToText (persistFieldDef rec), toPersistValue val)
    (fieldsToMaybeCopy, updateFieldNames) = partitionEithers $ map mfieldDef fieldValues
    fieldDbToText = T.pack . escapeDBName . fieldDB
    entityDef' = entityDef records
    firstField = case entityFieldNames of
        [] -> error "The entity you're trying to insert does not have any fields."
        (field:_) -> field
    entityFieldNames = map fieldDbToText (entityFields entityDef')
    tableName = T.pack . escapeDBName . entityDB $ entityDef'
    copyUnlessValues = map snd fieldsToMaybeCopy
    recordValues = concatMap (map toPersistValue . toPersistFields) records
    recordPlaceholders = Util.commaSeparated $ map (Util.parenWrapped . Util.commaSeparated . map (const "?") . toPersistFields) records
    mkCondFieldSet n _ = T.concat
        [ n
        , "=COALESCE("
        ,   "NULLIF("
        ,     "VALUES(", n, "),"
        ,     "?"
        ,   "),"
        ,   n
        , ")"
        ]
    condFieldSets = map (uncurry mkCondFieldSet) fieldsToMaybeCopy
    fieldSets = map (\n -> T.concat [n, "=VALUES(", n, ")"]) updateFieldNames
    upds = map (Util.mkUpdateText' (pack . escapeDBName) id) updates
    updsValues = map (\(Update _ val _) -> toPersistValue val) updates
    updateText = case fieldSets <> upds <> condFieldSets of
        [] -> T.concat [firstField, "=", firstField]
        xs -> Util.commaSeparated xs
    q = T.concat
        [ "INSERT INTO "
        , tableName
        , " ("
        , Util.commaSeparated entityFieldNames
        , ") "
        , " VALUES "
        , recordPlaceholders
        , " ON DUPLICATE KEY UPDATE "
        , updateText
        ]

putManySql :: EntityDef -> Int -> Text
putManySql ent n = putManySql' fields ent n
  where
    fields = entityFields ent

repsertManySql :: EntityDef -> Int -> Text
repsertManySql ent n = putManySql' fields ent n
  where
    fields = keyAndEntityFields ent

putManySql' :: [FieldDef] -> EntityDef -> Int -> Text
putManySql' fields ent n = q
  where
    fieldDbToText = escape . fieldDB
    mkAssignment f = T.concat [f, "=VALUES(", f, ")"]

    table = escape . entityDB $ ent
    columns = Util.commaSeparated $ map fieldDbToText fields
    placeholders = map (const "?") fields
    updates = map (mkAssignment . fieldDbToText) fields

    q = T.concat
        [ "INSERT INTO "
        , table
        , Util.parenWrapped columns
        , " VALUES "
        , Util.commaSeparated . replicate n
            . Util.parenWrapped . Util.commaSeparated $ placeholders
        , " ON DUPLICATE KEY UPDATE "
        , Util.commaSeparated updates
        ]

mysqlMkColumns :: [EntityDef] -> EntityDef -> ([Column], [UniqueDef], [ForeignDef])
mysqlMkColumns allDefs t = mkColumns allDefs t emptyBackendSpecificOverrides
