{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Generate postgresql migrations for a set of EntityDefs, either from scratch
-- or based on the current state of a database.
module Database.Persist.Postgresql.Internal.Migration where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Acquire (with)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable
import Database.Persist.Sql

-- | In order to ensure that generating migrations is fast and avoids N+1
-- queries, we split it into two phases. The first phase involves querying the
-- database to gather all of the information we need about the existing schema.
-- The second phase then generates migrations based on the information from the
-- first phase. This data type represents all of the data that's gathered during
-- the first phase: information about the current state of the entities we're
-- migrating in the database.
newtype SchemaState = SchemaState (Map EntityNameDB EntitySchemaState)
    deriving (Eq, Show)

-- | The state of a particular entity in the database; we generate migrations
-- based on the diff of this versus an EntityDef.
data EntitySchemaState = EntitySchemaState
    { essColumns :: [Column]
    -- ^ The columns in this entity
    , essConstraints :: Map ConstraintNameDB [FieldNameDB]
    -- ^ A map of constraint names to the columns that are affected by those
    -- constraints. Primary key and foreign key constraints are not included
    -- here, since they are part of the 'Column'.
    }
    deriving (Eq, Show)

-- | Query a database in order to assemble a SchemaState containing information
-- about each of the entities in the given list.
collectSchemaState
    :: (Text -> IO Statement) -> [EntityNameDB] -> IO (Either Text SchemaState)
collectSchemaState getStmt entityNames = runExceptT $ do
    columns <- getColumnsWithoutReferences getStmt entityNames
    constraints <- getConstraints getStmt entityNames
    foreignKeyReferences <- getForeignKeyReferences getStmt entityNames

    fmap (SchemaState . Map.fromList) $
        for entityNames $ \entityNameDB -> do
            let
                addColumnReference column =
                    column
                        { cReference = Map.lookup (cName column) =<< Map.lookup entityNameDB foreignKeyReferences
                        }

            essColumns <- case Map.lookup entityNameDB columns of
                Just cols ->
                    pure (map addColumnReference cols)
                Nothing ->
                    throwError
                        ("Missing entity name from columns map: " <> unEntityNameDB entityNameDB)

            let
                essConstraints = fromMaybe Map.empty (Map.lookup entityNameDB constraints)
            pure
                ( entityNameDB
                , EntitySchemaState{essColumns, essConstraints}
                )

runStmt
    :: (Show a)
    => (Text -> IO Statement)
    -> Text
    -> [PersistValue]
    -> ([PersistValue] -> a)
    -> IO [a]
runStmt getStmt sql values process = do
    stmt <- getStmt sql
    results <-
        with
            (stmtQuery stmt values)
            (\src -> runConduit $ src .| CL.map process .| CL.consume)
    pure results

-- | Get all columns for the listed tables from the database, ignoring foreign
-- key references (those are filled in later).
getColumnsWithoutReferences
    :: (Text -> IO Statement)
    -> [EntityNameDB]
    -> ExceptT Text IO (Map EntityNameDB [Column])
getColumnsWithoutReferences getStmt entityNames = do
    results <-
        liftIO $
            runStmt
                getStmt
                getColumnsSql
                [PersistArray (map (PersistText . unEntityNameDB) entityNames)]
                processColumn
    case partitionEithers results of
        ([], xs) -> pure $ Map.fromListWith (++) $ map (second (: [])) xs
        (errs, _) -> throwError (T.intercalate "\n" errs)
  where
    getColumnsSql =
        T.concat
            [ "SELECT "
            , "table_name "
            , ",column_name "
            , ",is_nullable "
            , ",COALESCE(domain_name, udt_name)" -- See DOMAINS below
            , ",column_default "
            , ",generation_expression "
            , ",numeric_precision "
            , ",numeric_scale "
            , ",character_maximum_length "
            , "FROM information_schema.columns "
            , "WHERE table_catalog=current_database() "
            , "AND table_schema=current_schema() "
            , "AND table_name=ANY (?) "
            ]

    -- DOMAINS Postgres supports the concept of domains, which are data types
    -- with optional constraints.  An app might make an "email" domain over the
    -- varchar type, with a CHECK that the emails are valid In this case the
    -- generated SQL should use the domain name: ALTER TABLE users ALTER COLUMN
    -- foo TYPE email This code exists to use the domain name (email), instead
    -- of the underlying type (varchar).  This is tested in
    -- EquivalentTypeTest.hs
    processColumn :: [PersistValue] -> Either Text (EntityNameDB, Column)
    processColumn resultRow = do
        case resultRow of
            [ PersistText tableName
                , PersistText columnName
                , PersistText isNullable
                , PersistText typeName
                , defaultValue
                , generationExpression
                , numericPrecision
                , numericScale
                , maxlen
                ] -> mapLeft (addErrorContext tableName columnName) $ do
                    defaultValue' <-
                        case defaultValue of
                            PersistNull ->
                                pure Nothing
                            PersistText t ->
                                pure $ Just t
                            _ ->
                                throwError $ T.pack $ "Invalid default column: " ++ show defaultValue
                    generationExpression' <-
                        case generationExpression of
                            PersistNull ->
                                pure Nothing
                            PersistText t ->
                                pure $ Just t
                            _ ->
                                throwError $ T.pack $ "Invalid generated column: " ++ show generationExpression
                    let
                        typeStr =
                            case maxlen of
                                PersistInt64 n ->
                                    T.concat [typeName, "(", T.pack (show n), ")"]
                                _ ->
                                    typeName

                    t <- getType numericPrecision numericScale typeStr

                    pure
                        ( EntityNameDB tableName
                        , Column
                            { cName = FieldNameDB columnName
                            , cNull = isNullable == "YES"
                            , cSqlType = t
                            , cDefault = fmap stripSuffixes defaultValue'
                            , cGenerated = fmap stripSuffixes generationExpression'
                            , cDefaultConstraintName = Nothing
                            , cMaxLen = Nothing
                            , cReference = Nothing
                            }
                        )
            other ->
                Left $
                    T.pack $
                        "Invalid result from information_schema: " ++ show other

    stripSuffixes t =
        loop'
            [ "::character varying"
            , "::text"
            ]
      where
        loop' [] = t
        loop' (p : ps) =
            case T.stripSuffix p t of
                Nothing -> loop' ps
                Just t' -> t'

    getType _ _ "int4" = pure SqlInt32
    getType _ _ "int8" = pure SqlInt64
    getType _ _ "varchar" = pure SqlString
    getType _ _ "text" = pure SqlString
    getType _ _ "date" = pure SqlDay
    getType _ _ "bool" = pure SqlBool
    getType _ _ "timestamptz" = pure SqlDayTime
    getType _ _ "float4" = pure SqlReal
    getType _ _ "float8" = pure SqlReal
    getType _ _ "bytea" = pure SqlBlob
    getType _ _ "time" = pure SqlTime
    getType precision scale "numeric" = getNumeric precision scale
    getType _ _ a = pure $ SqlOther a

    getNumeric (PersistInt64 a) (PersistInt64 b) =
        pure $ SqlNumeric (fromIntegral a) (fromIntegral b)
    getNumeric PersistNull PersistNull =
        throwError $
            T.concat
                [ "No precision and scale were specified. "
                , "Postgres defaults to a maximum scale of 147,455 and precision of 16383,"
                , " which is probably not what you intended."
                , " Specify the values as numeric(total_digits, digits_after_decimal_place)."
                ]
    getNumeric a b =
        throwError $
            T.concat
                [ "Can not get numeric field precision. "
                , "Expected an integer for both precision and scale, "
                , "got: "
                , T.pack $ show a
                , " and "
                , T.pack $ show b
                , ", respectively."
                , " Specify the values as numeric(total_digits, digits_after_decimal_place)."
                ]

-- cyclist putting a stick into his own wheel meme
addErrorContext :: Text -> Text -> Text -> Text
addErrorContext tableName columnName originalMsg =
    T.concat
        [ "Error in column "
        , tableName
        , "."
        , columnName
        , ": "
        , originalMsg
        ]

-- | Get all constraints for the listed tables from the database, except for foreign
-- keys and primary keys (those go in the Column data type)
getConstraints
    :: (Text -> IO Statement)
    -> [EntityNameDB]
    -> ExceptT Text IO (Map EntityNameDB (Map ConstraintNameDB [FieldNameDB]))
getConstraints getStmt entityNames = do
    results <-
        liftIO $
            runStmt
                getStmt
                getConstraintsSql
                [PersistArray (map (PersistText . unEntityNameDB) entityNames)]
                processConstraint
    case partitionEithers results of
        ([], xs) -> pure $ Map.unionsWith (Map.unionWith (<>)) xs
        (errs, _) -> throwError (T.intercalate "\n" errs)
  where
    getConstraintsSql =
        T.concat
            [ "SELECT "
            , "c.table_name, "
            , "c.constraint_name, "
            , "c.column_name "
            , "FROM information_schema.key_column_usage AS c, "
            , "information_schema.table_constraints AS k "
            , "WHERE c.table_catalog=current_database() "
            , "AND c.table_catalog=k.table_catalog "
            , "AND c.table_schema=current_schema() "
            , "AND c.table_schema=k.table_schema "
            , "AND c.table_name=ANY (?) "
            , "AND c.table_name=k.table_name "
            , "AND c.constraint_name=k.constraint_name "
            , "AND NOT k.constraint_type IN ('PRIMARY KEY', 'FOREIGN KEY') "
            , "ORDER BY c.constraint_name, c.column_name"
            ]

    processConstraint
        :: [PersistValue]
        -> Either Text (Map EntityNameDB (Map ConstraintNameDB [FieldNameDB]))
    processConstraint resultRow = do
        (tableName, constraintName, columnName) <- case resultRow of
            [PersistText tab, PersistText con, PersistText col] ->
                pure (tab, con, col)
            [PersistByteString tab, PersistByteString con, PersistByteString col] ->
                pure (T.decodeUtf8 tab, T.decodeUtf8 con, T.decodeUtf8 col)
            o ->
                throwError $ T.pack $ "unexpected datatype returned for postgres o=" ++ show o

        pure $
            Map.singleton
                (EntityNameDB tableName)
                (Map.singleton (ConstraintNameDB constraintName) [FieldNameDB columnName])

-- | Get foreign key reference information for all columns in the supplied
-- tables from the database.
getForeignKeyReferences
    :: (Text -> IO Statement)
    -> [EntityNameDB]
    -> ExceptT Text IO (Map EntityNameDB (Map FieldNameDB ColumnReference))
getForeignKeyReferences getStmt entityNames = do
    results <-
        liftIO $
            runStmt
                getStmt
                getForeignKeyReferencesSql
                [PersistArray (map (PersistText . unEntityNameDB) entityNames)]
                processForeignKeyReference
    case partitionEithers results of
        ([], xs) -> pure $ Map.unionsWith Map.union xs
        (errs, _) -> throwError (T.intercalate "\n" errs)
  where
    -- TODO: should this filter by schema?
    getForeignKeyReferencesSql =
        T.concat
            [ "SELECT DISTINCT "
            , "kcu.table_name, "
            , "kcu.column_name, "
            , "ccu.table_name, "
            , "tc.constraint_name, "
            , "rc.update_rule, "
            , "rc.delete_rule "
            , "FROM information_schema.constraint_column_usage ccu "
            , "INNER JOIN information_schema.key_column_usage kcu "
            , "  ON ccu.constraint_name = kcu.constraint_name "
            , "INNER JOIN information_schema.table_constraints tc "
            , "  ON tc.constraint_name = kcu.constraint_name "
            , "LEFT JOIN information_schema.referential_constraints AS rc"
            , "  ON rc.constraint_name = ccu.constraint_name "
            , "WHERE tc.constraint_type='FOREIGN KEY' "
            , "AND kcu.ordinal_position=1 "
            , "AND kcu.table_name=ANY (?) "
            ]

    processForeignKeyReference
        :: [PersistValue]
        -> Either Text (Map EntityNameDB (Map FieldNameDB ColumnReference))
    processForeignKeyReference resultRow = do
        (sourceTableName, sourceColumnName, refTableName, constraintName, updRule, delRule) <-
            case resultRow of
                [ PersistText srcTable
                    , PersistText srcColumn
                    , PersistText refTable
                    , PersistText constraint
                    , PersistText updRule
                    , PersistText delRule
                    ] ->
                        pure
                            ( EntityNameDB srcTable
                            , FieldNameDB srcColumn
                            , EntityNameDB refTable
                            , ConstraintNameDB constraint
                            , updRule
                            , delRule
                            )
                other ->
                    throwError $ T.pack $ "unexpected row returned for postgres: " ++ show other

        fcOnUpdate <- parseCascade updRule
        fcOnDelete <- parseCascade delRule

        let columnRef = ColumnReference
                { crTableName = refTableName
                , crConstraintName = constraintName
                , crFieldCascade = FieldCascade
                    { fcOnUpdate = Just fcOnUpdate
                    , fcOnDelete = Just fcOnDelete
                    }
                }

        pure $ Map.singleton sourceTableName (Map.singleton sourceColumnName columnRef)

parseCascade :: Text -> Either Text CascadeAction
parseCascade txt =
    case txt of
        "NO ACTION" ->
            Right NoAction
        "CASCADE" ->
            Right Cascade
        "SET NULL" ->
            Right SetNull
        "SET DEFAULT" ->
            Right SetDefault
        "RESTRICT" ->
            Right Restrict
        _ ->
            Left $ "Unexpected value in parseCascade: " <> txt

mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft _ (Right x) = Right x
mapLeft f (Left x) = Left (f x)
