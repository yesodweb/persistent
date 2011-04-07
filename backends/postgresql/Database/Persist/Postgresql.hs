{-# LANGUAGE OverloadedStrings #-}
-- | A postgresql backend for persistent.
module Database.Persist.Postgresql
    ( withPostgresqlPool
    , withPostgresqlConn
    , module Database.Persist
    , module Database.Persist.GenericSql
    ) where

import Database.Persist
import Database.Persist.Base hiding (Add, Update)
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Internal

import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as H

import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as Map
import Data.Either (partitionEithers)
import Control.Arrow
import Data.List (sort, groupBy)
import Data.Function (on)
import Control.Monad.IO.Control (MonadControlIO)

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Time.LocalTime (localTimeToUTC, utc)
import Data.Text (Text, pack, unpack)

withPostgresqlPool :: MonadControlIO m
                   => T.Text
                   -> Int -- ^ number of connections to open
                   -> (ConnectionPool -> m a) -> m a
withPostgresqlPool s = withSqlPool $ open' s

withPostgresqlConn :: MonadControlIO m => T.Text -> (Connection -> m a) -> m a
withPostgresqlConn = withSqlConn . open'

open' :: T.Text -> IO Connection
open' s = do
    conn <- H.connectPostgreSQL $ T.unpack s
    smap <- newIORef $ Map.empty
    return Connection
        { prepare = prepare' conn
        , stmtMap = smap
        , insertSql = insertSql'
        , close = H.disconnect conn
        , migrateSql = migrate'
        , begin = const $ return ()
        , commit = const $ H.commit conn
        , rollback = const $ H.rollback conn
        , escapeName = escape
        , noLimit = "LIMIT ALL"
        }

prepare' :: H.Connection -> Text -> IO Statement
prepare' conn sql = do
    stmt <- H.prepare conn $ unpack sql
    return Statement
        { finalize = return ()
        , reset = return ()
        , execute = execute' stmt
        , withStmt = withStmt' stmt
        }

insertSql' :: RawName -> [RawName] -> Either Text (Text, Text)
insertSql' t cols = Left $ pack $ concat
    [ "INSERT INTO "
    , escape t
    , "("
    , intercalate "," $ map escape cols
    , ") VALUES("
    , intercalate "," (map (const "?") cols)
    , ") RETURNING id"
    ]

execute' :: H.Statement -> [PersistValue] -> IO ()
execute' stmt vals = do
    _ <- H.execute stmt $ map pToSql vals
    return ()

withStmt' :: MonadControlIO m
          => H.Statement
          -> [PersistValue]
          -> (RowPopper m -> m a)
          -> m a
withStmt' stmt vals f = do
    _ <- liftIO $ H.execute stmt $ map pToSql vals
    f $ liftIO $ (fmap . fmap) (map pFromSql) $ H.fetchRow stmt

pToSql :: PersistValue -> H.SqlValue
pToSql (PersistText t) = H.SqlString $ unpack t
pToSql (PersistByteString bs) = H.SqlByteString bs
pToSql (PersistInt64 i) = H.SqlInt64 i
pToSql (PersistDouble d) = H.SqlDouble d
pToSql (PersistBool b) = H.SqlBool b
pToSql (PersistDay d) = H.SqlLocalDate d
pToSql (PersistTimeOfDay t) = H.SqlLocalTimeOfDay t
pToSql (PersistUTCTime t) = H.SqlUTCTime t
pToSql PersistNull = H.SqlNull
pToSql (PersistList _) = error "Refusing to serialize a PersistList to a PostgreSQL value"
pToSql (PersistMap _) = error "Refusing to serialize a PersistMap to a SQLite value"

pFromSql :: H.SqlValue -> PersistValue
pFromSql (H.SqlString s) = PersistText $ pack s
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
pFromSql (H.SqlLocalTime d) = PersistUTCTime $ localTimeToUTC utc d
pFromSql x = PersistText $ pack $ H.fromSql x -- FIXME

migrate' :: PersistEntity val
         => (Text -> IO Statement)
         -> val
         -> IO (Either [Text] [(Bool, Text)])
migrate' getter val = do
    let name = rawTableName $ entityDef val
    old <- getColumns getter name
    case partitionEithers old of
        ([], old'') -> do
            let old' = partitionEithers old''
            let new = mkColumns val
            if null old
                then do
                    let addTable = AddTable $ concat
                            [ "CREATE TABLE "
                            , escape name
                            , "(id SERIAL PRIMARY KEY UNIQUE"
                            , concatMap (\x -> ',' : showColumn x) $ fst new
                            , ")"
                            ]
                    let rest = flip concatMap (snd new) $ \(uname, ucols) ->
                            [AlterTable name $ AddUniqueConstraint uname ucols]
                    return $ Right $ map showAlterDb $ addTable : rest
                else do
                    let (acs, ats) = getAlters new old'
                    let acs' = map (AlterColumn name) acs
                    let ats' = map (AlterTable name) ats
                    return $ Right $ map showAlterDb $ acs' ++ ats'
        (errs, _) -> return $ Left errs

data AlterColumn = Type SqlType | IsNull | NotNull | Add Column | Drop
                 | Default String | NoDefault | Update String
                 | AddReference RawName | DropReference RawName
type AlterColumn' = (RawName, AlterColumn)

data AlterTable = AddUniqueConstraint RawName [RawName]
                | DropConstraint RawName

data AlterDB = AddTable String
             | AlterColumn RawName AlterColumn'
             | AlterTable RawName AlterTable

-- | Returns all of the columns in the given table currently in the database.
getColumns :: (Text -> IO Statement)
           -> RawName -> IO [Either Text (Either Column UniqueDef)]
getColumns getter name = do
    stmt <- getter "SELECT column_name,is_nullable,udt_name,column_default FROM information_schema.columns WHERE table_name=? AND column_name <> 'id'"
    cs <- withStmt stmt [PersistText $ pack $ unRawName name] helper
    stmt' <- getter
        "SELECT constraint_name, column_name FROM information_schema.constraint_column_usage WHERE table_name=? AND column_name <> 'id' ORDER BY constraint_name, column_name"
    us <- withStmt stmt' [PersistText $ pack $ unRawName name] helperU
    return $ cs ++ us
  where
    getAll pop front = do
        x <- pop
        case x of
            Nothing -> return $ front []
            Just [PersistByteString con, PersistByteString col] ->
                getAll pop (front . (:) (bsToChars con, bsToChars col))
            Just _ -> getAll pop front -- FIXME error message?
    helperU pop = do
        rows <- getAll pop id
        return $ map (Right . Right . (RawName . fst . head &&& map (RawName . snd)))
               $ groupBy ((==) `on` fst) rows
    helper pop = do
        x <- pop
        case x of
            Nothing -> return []
            Just x' -> do
                col <- getColumn getter name x'
                let col' = case col of
                            Left e -> Left e
                            Right c -> Right $ Left c
                cols <- helper pop
                return $ col' : cols

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
        case lookup name old of
            Nothing -> AddUniqueConstraint name cols : getAltersU news old
            Just ocols ->
                let old' = filter (\(x, _) -> x /= name) old
                 in if sort cols == ocols
                        then getAltersU news old'
                        else  DropConstraint name
                            : AddUniqueConstraint name cols
                            : getAltersU news old'

getColumn :: (Text -> IO Statement)
          -> RawName -> [PersistValue]
          -> IO (Either Text Column)
getColumn getter tname
        [PersistByteString x, PersistByteString y,
         PersistByteString z, d] =
    case d' of
        Left s -> return $ Left s
        Right d'' ->
            case getType $ bsToChars z of
                Left s -> return $ Left s
                Right t -> do
                    let cname = RawName $ bsToChars x
                    ref <- getRef cname
                    return $ Right $ Column cname (bsToChars y == "YES")
                                     t d'' ref
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
        withStmt stmt
                     [ PersistText $ pack $ unRawName tname
                     , PersistText $ pack $ unRawName ref
                     ] $ \pop -> do
            Just [PersistInt64 i] <- pop
            return $ if i == 0 then Nothing else Just (RawName "", ref)
    d' = case d of
            PersistNull -> Right Nothing
            PersistByteString a -> Right $ Just $ bsToChars a
            _ -> Left $ pack $ "Invalid default column: " ++ show d
    getType "int4" = Right $ SqlInt32
    getType "int8" = Right $ SqlInteger
    getType "varchar" = Right $ SqlString
    getType "date" = Right $ SqlDay
    getType "bool" = Right $ SqlBool
    getType "timestamp" = Right $ SqlDayTime
    getType "float4" = Right $ SqlReal
    getType "float8" = Right $ SqlReal
    getType "bytea" = Right $ SqlBlob
    getType a = Left $ pack $ "Unknown type: " ++ a
getColumn _ _ x =
    return $ Left $ pack $ "Invalid result from information_schema: " ++ show x

findAlters :: Column -> [Column] -> ([AlterColumn'], [Column])
findAlters col@(Column name isNull type_ def ref) cols =
    case filter (\c -> cName c == name) cols of
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
                 filter (\c -> cName c /= name) cols)

showColumn :: Column -> String
showColumn (Column n nu t def ref) = concat
    [ escape n
    , " "
    , showSqlType t
    , " "
    , if nu then "NULL" else "NOT NULL"
    , case def of
        Nothing -> ""
        Just s -> " DEFAULT " ++ s
    , case ref of
        Nothing -> ""
        Just (s, _) -> " REFERENCES " ++ escape s
    ]

showSqlType :: SqlType -> String
showSqlType SqlString = "VARCHAR"
showSqlType SqlInt32 = "INT4"
showSqlType SqlInteger = "INT8"
showSqlType SqlReal = "DOUBLE PRECISION"
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTime = "TIMESTAMP"
showSqlType SqlBlob = "BYTEA"
showSqlType SqlBool = "BOOLEAN"

showAlterDb :: AlterDB -> (Bool, Text)
showAlterDb (AddTable s) = (False, pack s)
showAlterDb (AlterColumn t (c, ac)) =
    (isUnsafe ac, pack $ showAlter t (c, ac))
  where
    isUnsafe Drop = True
    isUnsafe _ = False
showAlterDb (AlterTable t at) = (False, pack $ showAlterTable t at)

showAlterTable :: RawName -> AlterTable -> String
showAlterTable table (AddUniqueConstraint cname cols) = concat
    [ "ALTER TABLE "
    , escape table
    , " ADD CONSTRAINT "
    , escape cname
    , " UNIQUE("
    , intercalate "," $ map escape cols
    , ")"
    ]
showAlterTable table (DropConstraint cname) = concat
    [ "ALTER TABLE "
    , escape table
    , " DROP CONSTRAINT "
    , escape cname
    ]

showAlter :: RawName -> AlterColumn' -> String
showAlter table (n, Type t) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " TYPE "
        , showSqlType t
        ]
showAlter table (n, IsNull) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " DROP NOT NULL"
        ]
showAlter table (n, NotNull) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " SET NOT NULL"
        ]
showAlter table (_, Add col) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ADD COLUMN "
        , showColumn col
        ]
showAlter table (n, Drop) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " DROP COLUMN "
        , escape n
        ]
showAlter table (n, Default s) =
    concat
        [ "ALTER TABLE "
        , escape table
        , " ALTER COLUMN "
        , escape n
        , " SET DEFAULT "
        , s
        ]
showAlter table (n, NoDefault) = concat
    [ "ALTER TABLE "
    , escape table
    , " ALTER COLUMN "
    , escape n
    , " DROP DEFAULT"
    ]
showAlter table (n, Update s) = concat
    [ "UPDATE "
    , escape table
    , " SET "
    , escape n
    , "="
    , s
    , " WHERE "
    , escape n
    , " IS NULL"
    ]
showAlter table (n, AddReference t2) = concat
    [ "ALTER TABLE "
    , escape table
    , " ADD CONSTRAINT "
    , escape $ refName table n
    , " FOREIGN KEY("
    , escape n
    , ") REFERENCES "
    , escape t2
    ]
showAlter table (_, DropReference cname) =
    "ALTER TABLE " ++ escape table ++ " DROP CONSTRAINT " ++ escape cname

escape :: RawName -> String
escape (RawName s) =
    '"' : go s ++ "\""
  where
    go "" = ""
    go ('"':xs) = "\"\"" ++ go xs
    go (x:xs) = x : go xs

bsToChars :: ByteString -> String
bsToChars = T.unpack . T.decodeUtf8With T.lenientDecode
