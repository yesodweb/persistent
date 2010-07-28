{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A postgresql backend for persistent.
module Database.Persist.Postgresql
    ( PostgresqlReader
    , runPostgresql
    , withPostgresql
    , Connection (..)
    , connectPostgreSQL -- should probably be removed in the future
    , Pool
    , module Database.Persist
    , runAlterDBs
    , parseMigration
    , parseMigration'
    , migrate
    , runMigration
    , runMigrationForce
    ) where

import Database.Persist
import Database.Persist.Base
import qualified Database.Persist.GenericSql as G
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as H
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Data.Char (toLower)
import Data.Int (Int64)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Applicative (Applicative)
import Database.Persist.Pool
import Data.IORef
import qualified Data.Map as Map
import Data.ByteString.Char8 (unpack)
import Data.Either (partitionEithers)
import Control.Monad (liftM, (<=<), forM_)
import System.IO (hPutStrLn, stderr)
import Control.Arrow
import Data.Maybe (fromJust)
import Data.List (sort, groupBy)
import Data.Function (on)

type StmtMap = Map.Map String H.Statement
data Connection = Connection H.Connection (IORef StmtMap)

-- | A ReaderT monad transformer holding a postgresql database connection.
newtype PostgresqlReader m a = PostgresqlReader (ReaderT Connection m a)
    deriving (Monad, MonadIO, MonadTrans, MonadCatchIO, Functor,
              Applicative)

-- | Handles opening and closing of the database connection automatically.
withPostgresql :: MonadCatchIO m
               => String -- ^ connection string
               -> Int -- ^ maximum number of connections in the pool
               -> (Pool Connection -> m a)
               -> m a
withPostgresql s i f = createPool (open' s) close' i f

open' :: String -> IO Connection
open' sql = do
    conn <- H.connectPostgreSQL sql
    istmtmap <- newIORef Map.empty
    return $ Connection conn istmtmap

close' :: Connection -> IO ()
close' (Connection conn _) = H.disconnect conn

-- | Run a series of database actions within a single transactions. On any
-- exception, the transaction is rolled back.
runPostgresql :: MonadCatchIO m
              => PostgresqlReader m a
              -> Pool Connection
              -> m a
runPostgresql (PostgresqlReader r) pconn = withPool' pconn $
  \conn@(Connection conn' _) -> do
    res <- onException (runReaderT r conn) $ liftIO (H.rollback conn')
    liftIO $ H.commit conn'
    return res

getStmt :: String -> Connection -> IO H.Statement
getStmt sql (Connection conn istmtmap) = do
    stmtmap <- readIORef istmtmap
    case Map.lookup sql stmtmap of
        Just stmt -> do
            H.finish stmt
            return stmt
        Nothing -> do
            stmt <- H.prepare conn sql
            let stmtmap' = Map.insert sql stmt stmtmap
            writeIORef istmtmap stmtmap'
            return stmt

withStmt :: MonadIO m
         => String
         -> [PersistValue]
         -> (G.RowPopper (PostgresqlReader m) -> PostgresqlReader m a)
         -> PostgresqlReader m a
withStmt sql vals f = do
    conn <- PostgresqlReader ask
    stmt <- liftIO $ getStmt sql conn
    _ <- liftIO $ H.execute stmt $ map pToSql vals
    f $ liftIO $ (fmap . fmap) (map pFromSql) $ H.fetchRow stmt

execute :: MonadIO m => String -> [PersistValue] -> PostgresqlReader m ()
execute sql vals = do
    conn <- PostgresqlReader ask
    stmt <- liftIO $ getStmt sql conn
    _ <- liftIO $ H.execute stmt $ map pToSql vals
    return ()

insert' :: MonadIO m
        => String -> [String] -> [PersistValue] -> PostgresqlReader m Int64
insert' t cols vals = do
    let sql = "INSERT INTO " ++ t ++
              "(" ++ intercalate "," cols ++
              ") VALUES(" ++
              intercalate "," (map (const "?") cols) ++ ") " ++
              "RETURNING id"
    withStmt sql vals $ \pop -> do
        Just [PersistInt64 i] <- pop
        return i

tableExists :: MonadIO m => String -> PostgresqlReader m Bool
tableExists t = do
    Connection conn _ <- PostgresqlReader ask
    tables <- liftIO $ H.getTables conn
    return $ map toLower t `elem` tables

genericSql :: MonadIO m => G.GenericSql (PostgresqlReader m)
genericSql =
    G.GenericSql withStmt execute insert' tableExists
                 "SERIAL PRIMARY KEY" showSqlType

pToSql :: PersistValue -> H.SqlValue
pToSql (PersistString s) = H.SqlString s
pToSql (PersistByteString bs) = H.SqlByteString bs
pToSql (PersistInt64 i) = H.SqlInt64 i
pToSql (PersistDouble d) = H.SqlDouble d
pToSql (PersistBool b) = H.SqlBool b
pToSql (PersistDay d) = H.SqlLocalDate d
pToSql (PersistTimeOfDay t) = H.SqlLocalTimeOfDay t
pToSql (PersistUTCTime t) = H.SqlUTCTime t
pToSql PersistNull = H.SqlNull

pFromSql :: H.SqlValue -> PersistValue
pFromSql (H.SqlString s) = PersistString s
pFromSql (H.SqlByteString bs) = PersistByteString bs
pFromSql (H.SqlWord32 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlWord64 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInt32 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInt64 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInteger i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlChar c) = PersistInt64 $ fromIntegral $ fromEnum c
pFromSql (H.SqlBool b) = PersistBool b
pFromSql (H.SqlDouble b) = PersistDouble b
pFromSql (H.SqlRational b) = PersistDouble $ fromRational b
pFromSql (H.SqlLocalDate d) = PersistDay d
pFromSql (H.SqlLocalTimeOfDay d) = PersistTimeOfDay d
pFromSql (H.SqlUTCTime d) = PersistUTCTime d
pFromSql H.SqlNull = PersistNull
pFromSql x = PersistString $ H.fromSql x -- FIXME

data Column = Column
    { cName :: String
    , _cNull :: Bool
    , _cType :: SqlType
    , _cDefault :: Maybe String
    , _cReference :: (Maybe (String, String)) -- table name, constraint name
    }

instance Show Column where
    show (Column n nu t def ref) = concat
        [ n
        , " "
        , showSqlType t
        , " "
        , if nu then "NULL" else "NOT NULL"
        , case def of
            Nothing -> ""
            Just s -> " DEFAULT " ++ s
        , case ref of
            Nothing -> ""
            Just (s, _) -> " REFERENCES " ++ s
        ]

data AlterColumn = Type SqlType | IsNull | NotNull | Add Column | Drop
                 | Default String | NoDefault | Update String
                 | AddReference String | DropReference String
    deriving Show
type AlterColumn' = (String, AlterColumn)

data AlterTable = AddUniqueConstraint String [String]
                | DropConstraint String
    deriving Show

data AlterDB = AddTable String
             | AlterColumn String AlterColumn'
             | AlterTable String AlterTable

runMigrationForce :: MonadIO m
                  => Migration (PostgresqlReader m)
                  -> PostgresqlReader m ()
runMigrationForce = runAlterDBs <=< parseMigration'

runMigration :: MonadIO m
             => Migration (PostgresqlReader m)
             -> PostgresqlReader m ()
runMigration m = do
    m' <- parseMigration' m
    case partitionEithers $ map noUnsafe m' of
        ([], alts) -> runAlterDBs alts
        (errs, _) -> error $ concat
            [ "\n\nDatabase migration: manual intervention required.\n"
            , "The following actions are considered unsafe:\n\n"
            , unlines $ map ("    " ++) errs
            ]
  where
    noUnsafe (AlterColumn table (column, Drop)) = Left $ concat
        [ "Drop column "
        , table
        , "."
        , column
        , "."
        ]
    noUnsafe x = Right x

runAlterDBs :: MonadIO m => [AlterDB] -> PostgresqlReader m ()
runAlterDBs = mapM_ go
  where
    go (AddTable s) = execute' s
    go (AlterTable table alt) = execute' $ showAlterTable table alt
    go (AlterColumn table alt) = execute' $ showAlter table alt
    execute' s = do
        liftIO $ hPutStrLn stderr $ "Migrating: " ++ s
        execute s []

showAlterTable :: String -> AlterTable -> String
showAlterTable table (AddUniqueConstraint cname cols) = concat
    [ "ALTER TABLE "
    , table
    , " ADD CONSTRAINT "
    , cname
    , " UNIQUE("
    , intercalate "," cols
    , ")"
    ]
showAlterTable table (DropConstraint cname) = concat
    [ "ALTER TABLE "
    , table
    , " DROP CONSTRAINT "
    , cname
    ]

showAlter :: String -> AlterColumn' -> String
showAlter table (n, Type t) =
    "ALTER TABLE " ++ table ++ " ALTER COLUMN " ++ n ++ " TYPE " ++ showSqlType t
showAlter table (n, IsNull) =
    "ALTER TABLE " ++ table ++ " ALTER COLUMN " ++ n ++ " DROP NOT NULL"
showAlter table (n, NotNull) =
    "ALTER TABLE " ++ table ++ " ALTER COLUMN " ++ n ++ " SET NOT NULL"
showAlter table (_, Add col) =
    "ALTER TABLE " ++ table ++ " ADD COLUMN " ++ show col
showAlter table (n, Drop) =
    "ALTER TABLE " ++ table ++ " DROP COLUMN " ++ n
showAlter table (n, Default s) =
    "ALTER TABLE " ++ table ++ " ALTER COLUMN " ++ n ++ " SET DEFAULT " ++ s
showAlter table (n, NoDefault) =
    "ALTER TABLE " ++ table ++ " ALTER COLUMN " ++ n ++ " DROP DEFAULT"
showAlter table (n, Update s) =
    "UPDATE " ++ table ++ " SET " ++ n ++ "=" ++ s ++ " WHERE " ++
    n ++ " IS NULL"
showAlter table (n, AddReference t2) = concat
    [ "ALTER TABLE "
    , table
    , " ADD CONSTRAINT "
    , refName table n
    , " FOREIGN KEY("
    , n
    , ") REFERENCES "
    , t2
    ]
showAlter table (_, DropReference cname) =
    "ALTER TABLE " ++ table ++ " DROP CONSTRAINT " ++ cname

findAlters :: Column -> [Column] -> ([AlterColumn'], [Column])
findAlters col@(Column name isNull type_ def ref) cols =
    case filter (\c -> cName c == name') cols of
        [] -> ([(name, Add col)], cols)
        Column _ isNull' type_' def' ref':_ ->
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
                                            Just s -> (:) (name, Update s)
                                 in up [(name, NotNull)]
                            _ -> []
                modType = if type_ == type_' then [] else [(name, Type type_)]
                modDef =
                    if def == def'
                        then []
                        else case def of
                                Nothing -> [(name, NoDefault)]
                                Just s -> [(name, Default s)]
             in (modRef ++ modDef ++ modNull ++ modType,
                 filter (\c -> cName c /= name') cols)
  where
    name' = map toLower name

getAlters :: ([Column], [UniqueDef])
          -> ([Column], [UniqueDef])
          -> ([AlterColumn'], [AlterTable])
getAlters (c1, u1) (c2, u2) =
    (getAltersC c1 c2, getAltersU u1 u2)
  where
    getAltersC [] old = map (\x -> (cName x, Drop)) old
    getAltersC (new:news) old =
        let (alters, old') = findAlters new old
         in alters ++ getAltersC news old'
    getAltersU [] old = map (DropConstraint . fst) old
    getAltersU ((name, cols):news) old =
        case lookup (map toLower name) old of
            Nothing -> AddUniqueConstraint name cols : getAltersU news old
            Just ocols ->
                let old' = filter (\(x, _) -> x /= map toLower name) old
                 in if sort (map (map toLower) cols) == ocols
                        then getAltersU news old'
                        else  DropConstraint name
                            : AddUniqueConstraint name cols
                            : getAltersU news old'

getColumn :: MonadIO m => String -> [PersistValue]
          -> PostgresqlReader m (Either String Column)
getColumn tname
        [PersistByteString x, PersistByteString y,
         PersistByteString z, d] =
    case d' of
        Left s -> return $ Left s
        Right d'' ->
            case getType $ unpack z of
                Left s -> return $ Left s
                Right t -> do
                    let cname = unpack x
                    ref <- getRef cname
                    return $ Right $ Column cname (unpack y == "YES")
                                     t d'' ref
  where
    getRef cname = do
        let sql = concat
                [ "SELECT COUNT(*) FROM "
                , "information_schema.table_constraints "
                , "WHERE table_name=? "
                , "AND constraint_type='FOREIGN KEY' "
                , "AND constraint_name=?"
                ]
        let ref = refName tname cname
        withStmt sql [ PersistString $ map toLower tname
                     , PersistString ref
                     ] $ \pop -> do
            Just [PersistInt64 i] <- pop
            return $ if i == 0 then Nothing else Just ("", ref)
    d' = case d of
            PersistNull -> Right Nothing
            PersistByteString a -> Right $ Just $ unpack a
            _ -> Left $ "Invalid default column: " ++ show d
    getType "int4" = Right $ SqlInteger
    getType "varchar" = Right $ SqlString
    getType "date" = Right $ SqlDay
    getType "bool" = Right $ SqlBool
    getType "timestamp" = Right $ SqlDayTime
    getType "float4" = Right $ SqlReal
    getType "bytea" = Right $ SqlBlob
    getType a = Left $ "Unknown type: " ++ a
getColumn _ x =
    return $ Left $ "Invalid result from information_schema: " ++ show x

-- | Returns all of the columns in the given table currently in the database.
getColumns :: MonadIO m => String -> PostgresqlReader m [Either String (Either Column UniqueDef)]
getColumns name = do
    cs <-
      withStmt ("SELECT column_name,is_nullable,udt_name,column_default " ++
                "FROM information_schema.columns " ++
                "WHERE table_name=? AND column_name <> 'id'")
        [PersistString name]
        helper
    us <-
      withStmt (concat
        [ "SELECT constraint_name, column_name "
        , "FROM information_schema.constraint_column_usage "
        , "WHERE table_name=? AND column_name <> 'id' "
        , "ORDER BY constraint_name, column_name"
        ]) [PersistString name] helperU
    return $ cs ++ us
  where
    getAll pop front = do
        x <- pop
        case x of
            Nothing -> return $ front []
            Just [PersistByteString con, PersistByteString col] ->
                getAll pop (front . (:) (unpack con, unpack col))
            Just _ -> getAll pop front -- FIXME error message?
    helperU pop = do
        rows <- getAll pop id
        return $ map (Right . Right . (fst . head &&& map snd))
               $ groupBy ((==) `on` fst) rows
    helper pop = do
        x <- pop
        case x of
            Nothing -> return []
            Just x' -> do
                col <- getColumn name x'
                let col' = case col of
                            Left e -> Left e
                            Right c -> Right $ Left c
                cols <- helper pop
                return $ col' : cols

type UniqueDef = (String, [String])

-- | Create the list of columns for the given entity.
mkColumns :: PersistEntity val => val -> ([Column], [UniqueDef])
mkColumns val =
    (cols, uniqs)
  where
    colNameMap = map ((\(x, _, _) -> x) &&& G.toField) $ entityColumns t
    uniqs = map (second $ map $ fromJust . flip lookup colNameMap) $ entityUniques t
    cols = zipWith go (G.tableColumns t) $ toPersistFields $ halfDefined `asTypeOf` val
    t = entityDef val
    tn = map toLower $ G.tableName t
    go (name, t', as) p =
        Column name ("null" `elem` as) (sqlType p) (def as) (ref name t' as)
    def [] = Nothing
    def (('d':'e':'f':'a':'u':'l':'t':'=':d):_) = Just d
    def (_:rest) = def rest
    ref c t' [] =
        let l = length t'
            (f, b) = splitAt (l - 2) t'
         in if b == "Id"
                then Just ("tbl" ++ f, refName tn c)
                else Nothing
    ref _ _ ("noreference":_) = Nothing
    ref c _ (('r':'e':'f':'e':'r':'e':'n':'c':'e':'=':x):_) =
        Just (x, refName tn c)
    ref c x (_:y) = ref c x y

type Migration m = WriterT [String] (WriterT [AlterDB] m) ()

parseMigration :: Monad m => Migration m -> m (Either [String] [AlterDB])
parseMigration m = liftM go $ runWriterT $ execWriterT m
  where
    go ([], x) = Right x
    go (x, _) = Left x

parseMigration' :: Monad m => Migration m -> m [AlterDB]
parseMigration' = liftM (either (error . show) id) . parseMigration

migrate :: (MonadIO m, PersistEntity val)
        => val
        -> Migration (PostgresqlReader m)
migrate val = do
    let name = map toLower $ G.tableName $ entityDef val
    old <- lift $ lift $ getColumns name
    case partitionEithers old of
        ([], old'') -> do
            let old' = partitionEithers old''
            let new = mkColumns val
            if null old
                then do
                    lift $ tell [AddTable $ concat
                        [ "CREATE TABLE "
                        , name
                        , "(id SERIAL PRIMARY KEY UNIQUE"
                        , concatMap (\x -> ',' : show x) $ fst new
                        , ")"
                        ]]
                    forM_ (snd new) $ \(uname, ucols) ->
                        lift $ tell [AlterTable name $ AddUniqueConstraint uname ucols]
                else do
                    let (acs, ats) = getAlters new old'
                    forM_ acs $ lift . tell . return . AlterColumn name
                    forM_ ats $ lift . tell . return . AlterTable name
        (errs, _) -> tell errs

instance MonadIO m => PersistBackend (PostgresqlReader m) where
    insert = G.insert genericSql
    get = G.get genericSql
    replace = G.replace genericSql
    select = G.select genericSql
    count = G.count genericSql
    deleteWhere = G.deleteWhere genericSql
    update = G.update genericSql
    updateWhere = G.updateWhere genericSql
    getBy = G.getBy genericSql
    delete = G.delete genericSql
    deleteBy = G.deleteBy genericSql

showSqlType :: SqlType -> String
showSqlType SqlString = "VARCHAR"
showSqlType SqlInteger = "INTEGER"
showSqlType SqlReal = "REAL"
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTime = "TIMESTAMP"
showSqlType SqlBlob = "BYTEA"
showSqlType SqlBool = "BOOLEAN"

refName :: String -> String -> String
refName table column =
    map toLower table ++ '_' : map toLower column ++ "_fkey"
