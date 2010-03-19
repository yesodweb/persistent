{-# LANGUAGE TypeFamilies #-}
module Data.Persist.SQLite
    ( SQLite
    , loadSQLite
    ) where

import Data.Persist
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List
import Data.Maybe

newtype SQLite = SQLite Connection

loadSQLite :: FilePath -> IO SQLite
loadSQLite = fmap SQLite . connectSqlite3

instance DataStore SQLite where
    type RecordId SQLite = Integer
    initTable (SQLite conn) table = do
        let sql = concat
                [ "CREATE TABLE "
                , safe $ tableName table
                , "(id INTEGER PRIMARY KEY"
                , concatMap go $ tableFields table
                , ")"
                ]
        _ <- run conn sql []
        commit conn
        return ()
          where
            go f = ',' : safe (fieldName f)
    createRecord (SQLite conn) table rec = do
        let sql = concat
                [ "INSERT INTO "
                , safe $ tableName table
                , " VALUES(NULL"
                , concatMap (const ",?") $ tableFields table
                , ")"
                ]
        _ <- run conn sql $ map (fvToSql . snd) $ tableFreeze table rec
        commit conn
        x <- quickQuery conn "SELECT last_insert_rowid()" []
        case x of
            [[i]] -> return $ fromSql i
            _ -> error "SQLite.createRecord: Error with last_insert_rowid"
    readRecord (SQLite conn) table rid = do
        let sql = concat
                [ "SELECT * FROM "
                , safe $ tableName table
                , " WHERE id=?"
                ]
        get <- prepare conn sql
        _ <- execute get [toSql rid]
        mrow <- fetchRow get
        case mrow of
            Just (_:row) -> return $ readSqlValues table row
            Nothing -> return Nothing
    updateRecord (SQLite conn) table rid rec = do
        let sql = concat
                [ "UPDATE "
                , safe $ tableName table
                , " SET "
                , intercalate "," $ map go $ tableFields table
                , " WHERE id=?"
                ]
        _ <- run conn sql $ (map (fvToSql . snd) $ tableFreeze table rec)
                           ++ [toSql rid]
        commit conn
        return ()
          where
            go f = safe (fieldName f) ++ "=?"
    deleteRecord (SQLite conn) table rid = do
        let sql = concat
                [ "DELETE FROM "
                , safe $ tableName table
                , " WHERE id=?"
                ]
        _ <- run conn sql [toSql rid]
        commit conn
        return ()
    filterTable (SQLite conn) table filters = do
        let sql = concat
                [ "SELECT * FROM "
                , safe $ tableName table
                , " WHERE 1"
                , concatMap go filters
                ]
        mapMaybe go'' `fmap` quickQuery conn sql (map go' filters)
          where
            go (Filter name _ orderings) = concat
                [ " AND "
                , safe name
                , case sort orderings of
                    [LT] -> "<"
                    [EQ] -> "="
                    [GT] -> ">"
                    [LT, EQ] -> "<="
                    [LT, GT] -> "<>"
                    [EQ, GT] -> ">="
                    _ -> error $ "Invalid orderings: " ++ show orderings
                , "?"
                ]
            go' = fvToSql . filterValue
            go'' (i:rest) = do
                rec <- readSqlValues table rest
                return (fromSql i, rec)

safe :: String -> String
safe = id -- FIXME

readSqlValues :: Table a -> [SqlValue] -> Maybe a
readSqlValues table vals = do
    let fvals = zipWith fvFromSql (map fieldType $ tableFields table) vals
        pairs = zip (map fieldName $ tableFields table) fvals
    tableThaw table pairs

fvToSql :: FieldValue -> SqlValue
fvToSql (FVString s) = toSql s
fvToSql (FVInt i) = toSql i

fvFromSql :: FieldType -> SqlValue -> FieldValue
fvFromSql FTString = FVString . fromSql
fvFromSql FTInt = FVInt . fromSql
