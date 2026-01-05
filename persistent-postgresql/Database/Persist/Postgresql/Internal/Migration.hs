{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generate postgresql migrations for a set of EntityDefs, either from scratch
-- or based on the current state of a database.
module Database.Persist.Postgresql.Internal.Migration where

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Acquire (with)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Either (partitionEithers)
import Data.FileEmbed (embedFileRelative)
import Data.List as List
import qualified Data.List.NonEmpty as NEL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable
import Database.Persist.Sql
import qualified Database.Persist.Sql.Util as Util

-- | Returns a structured representation of all of the
-- DB changes required to migrate the Entity from its
-- current state in the database to the state described in
-- Haskell.
--
-- @since 2.17.1.0
migrateStructured
    :: [EntityDef]
    -> (Text -> IO Statement)
    -> EntityDef
    -> IO (Either [Text] [AlterDB])
migrateStructured allDefs getter entity =
    migrateEntitiesStructured getter allDefs [entity]

-- | Returns a structured representation of all of the DB changes required to
-- migrate the listed entities from their current state in the database to the
-- state described in Haskell. This function avoids N+1 queries, so if you
-- have a lot of entities to migrate, it's much faster to use this rather than
-- using 'migrateStructured' in a loop.
--
-- @since 2.14.1.0
migrateEntitiesStructured
    :: (Text -> IO Statement)
    -> [EntityDef]
    -> [EntityDef]
    -> IO (Either [Text] [AlterDB])
migrateEntitiesStructured getStmt allDefs defsToMigrate = do
    r <- collectSchemaState getStmt (map getEntityDBName defsToMigrate)
    pure $ case r of
        Right schemaState ->
            migrateEntitiesFromSchemaState schemaState allDefs defsToMigrate
        Left err ->
            Left [err]

-- | Returns a structured representation of all of the
-- DB changes required to migrate the Entity to the state
-- described in Haskell, assuming it currently does not
-- exist in the database.
--
-- @since 2.17.1.0
mockMigrateStructured
    :: [EntityDef]
    -> EntityDef
    -> [AlterDB]
mockMigrateStructured allDefs entity =
    migrateEntityFromSchemaState EntityDoesNotExist allDefs entity

-- | In order to ensure that generating migrations is fast and avoids N+1
-- queries, we split it into two phases. The first phase involves querying the
-- database to gather all of the information we need about the existing schema.
-- The second phase then generates migrations based on the information from the
-- first phase. This data type represents all of the data that's gathered during
-- the first phase: information about the current state of the entities we're
-- migrating in the database.
newtype SchemaState = SchemaState (Map EntityNameDB EntitySchemaState)
    deriving (Eq, Show)

-- | The state of a particular entity (i.e. table) in the database; we generate
-- migrations based on the diff of this versus an EntityDef.
data EntitySchemaState
    = -- | The table does not exist in the database
      EntityDoesNotExist
    | -- | The table does exist in the database
      EntityExists ExistingEntitySchemaState
    deriving (Eq, Show)

-- | Information about an existing table in the database
data ExistingEntitySchemaState = ExistingEntitySchemaState
    { essColumns :: Map FieldNameDB (Column, (Set ColumnReference))
    -- ^ The columns in this entity, together with the set of foreign key
    -- constraints that they are subject to. Usually the ColumnReference list
    -- will contain 0-1 elements, but in the event that there are multiple FK
    -- constraints applying to a given column in the database we need to keep
    -- track of them all because we don't yet know which one has the right name
    -- (based on what is in the corresponding model's EntityDef).
    --
    -- Note that cReference will be unset for these columns, for the same reason:
    -- there may be multiple FK constraints and we don't yet know which one to
    -- use.
    , essUniqueConstraints :: Map ConstraintNameDB [FieldNameDB]
    -- ^ A map of unique constraint names to the columns that are affected by
    -- those constraints.
    }
    deriving (Eq, Show)

-- | Query a database in order to assemble a SchemaState containing information
-- about each of the entities in the given list. Every entity name in the input
-- should be present in the returned Map.
collectSchemaState
    :: (Text -> IO Statement) -> [EntityNameDB] -> IO (Either Text SchemaState)
collectSchemaState getStmt entityNames = runExceptT $ do
    existence <- getTableExistence getStmt entityNames
    columns <- getColumnsWithoutReferences getStmt entityNames
    constraints <- getConstraints getStmt entityNames
    foreignKeyReferences <- getForeignKeyReferences getStmt entityNames

    fmap (SchemaState . Map.fromList) $
        for entityNames $ \entityNameDB -> do
            tableExists <- case Map.lookup entityNameDB existence of
                Just e -> pure e
                Nothing ->
                    throwError
                        ("Missing entity name from existence map: " <> unEntityNameDB entityNameDB)

            if tableExists
                then do
                    essColumns <- case Map.lookup entityNameDB columns of
                        Just cols ->
                            pure $ Map.fromList $ flip map cols $ \c ->
                                ( cName c
                                ,
                                    ( c
                                    , fromMaybe Set.empty $
                                        Map.lookup (cName c) =<< Map.lookup entityNameDB foreignKeyReferences
                                    )
                                )
                        Nothing ->
                            throwError
                                ("Missing entity name from columns map: " <> unEntityNameDB entityNameDB)

                    let
                        essUniqueConstraints = fromMaybe Map.empty (Map.lookup entityNameDB constraints)
                    pure
                        ( entityNameDB
                        , EntityExists $ ExistingEntitySchemaState{essColumns, essUniqueConstraints}
                        )
                else
                    pure
                        ( entityNameDB
                        , EntityDoesNotExist
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

-- | Check for the existence of each of the input tables. The keys in the
-- returned Map are exactly the entity names in the argument; True means the
-- table exists.
getTableExistence
    :: (Text -> IO Statement)
    -> [EntityNameDB]
    -> ExceptT Text IO (Map EntityNameDB Bool)
getTableExistence getStmt entityNames = do
    results <-
        liftIO $
            runStmt
                getStmt
                getTableExistenceSql
                [PersistArray (map (PersistText . unEntityNameDB) entityNames)]
                processTable
    case partitionEithers results of
        ([], xs) ->
            let
                existing = Set.fromList xs
             in
                pure $ Map.fromList $ map (\n -> (n, Set.member n existing)) entityNames
        (errs, _) -> throwError (T.intercalate "\n" errs)
  where
    getTableExistenceSql =
        "SELECT tablename FROM pg_catalog.pg_tables WHERE schemaname != 'pg_catalog'"
            <> " AND schemaname != 'information_schema' AND tablename=ANY (?)"

    processTable :: [PersistValue] -> Either Text EntityNameDB
    processTable resultRow = do
        fmap EntityNameDB $
            case resultRow of
                [PersistText tableName] ->
                    pure tableName
                [PersistByteString tableName] ->
                    pure (T.decodeUtf8 tableName)
                other ->
                    throwError $ T.pack $ "Invalid result from information_schema: " ++ show other

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

-- | Get foreign key constraint information for all columns in the supplied
-- tables from the database. We return a list of references per column because
-- there may be duplicate FK constraints in the database.
--
-- Note that we only care about FKs where the column in question has ordinal
-- position 1 i.e. is the first column appearing in the FK constraint.
-- Eventually we may want to fill this gap so that multi-column FK constraints
-- can be dealt with by this migrator, but for now that is not something that
-- persistent-postgresql handles.
getForeignKeyReferences
    :: (Text -> IO Statement)
    -> [EntityNameDB]
    -> ExceptT Text IO (Map EntityNameDB (Map FieldNameDB (Set ColumnReference)))
getForeignKeyReferences getStmt entityNames = do
    results <-
        liftIO $
            runStmt
                getStmt
                getForeignKeyReferencesSql
                [PersistArray (map (PersistText . unEntityNameDB) entityNames)]
                processForeignKeyReference
    case partitionEithers results of
        ([], xs) -> pure $ Map.unionsWith (Map.unionWith Set.union) xs
        (errs, _) -> throwError (T.intercalate "\n" errs)
  where
    getForeignKeyReferencesSql = T.decodeUtf8 $(embedFileRelative "sql/getForeignKeyReferences.sql")

    processForeignKeyReference
        :: [PersistValue]
        -> Either Text (Map EntityNameDB (Map FieldNameDB (Set ColumnReference)))
    processForeignKeyReference resultRow = do
        ( sourceTableName
            , sourceColumnName
            , refTableName
            , constraintName
            , updRule
            , delRule
            ) <-
            case resultRow of
                [ PersistText constrName
                    , PersistText srcTable
                    , PersistText refTable
                    , PersistText srcColumn
                    , PersistText _refColumn
                    , PersistText updRule
                    , PersistText delRule
                    ] ->
                        pure
                            ( EntityNameDB srcTable
                            , FieldNameDB srcColumn
                            , EntityNameDB refTable
                            , ConstraintNameDB constrName
                            , updRule
                            , delRule
                            )
                other ->
                    throwError $ T.pack $ "unexpected row returned for postgres: " ++ show other

        fcOnUpdate <- parseCascade updRule
        fcOnDelete <- parseCascade delRule

        let
            columnRef =
                ColumnReference
                    { crTableName = refTableName
                    , crConstraintName = constraintName
                    , crFieldCascade =
                        FieldCascade
                            { fcOnUpdate = Just fcOnUpdate
                            , fcOnDelete = Just fcOnDelete
                            }
                    }

        pure $
            Map.singleton
                sourceTableName
                (Map.singleton sourceColumnName (Set.singleton columnRef))

-- Parse a cascade action as represented in pg_constraint
parseCascade :: Text -> Either Text CascadeAction
parseCascade txt =
    case txt of
        "a" ->
            Right NoAction
        "c" ->
            Right Cascade
        "n" ->
            Right SetNull
        "d" ->
            Right SetDefault
        "r" ->
            Right Restrict
        _ ->
            Left $ "Unexpected value in parseCascade: " <> txt

mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft _ (Right x) = Right x
mapLeft f (Left x) = Left (f x)

migrateEntitiesFromSchemaState
    :: SchemaState
    -> [EntityDef]
    -> [EntityDef]
    -> Either [Text] [AlterDB]
migrateEntitiesFromSchemaState (SchemaState schemaStateMap) allDefs defsToMigrate =
    let
        go :: EntityDef -> Either Text [AlterDB]
        go entity = do
            let
                name = getEntityDBName entity
            case Map.lookup name schemaStateMap of
                Just entityState ->
                    Right $ migrateEntityFromSchemaState entityState allDefs entity
                Nothing ->
                    Left $ T.pack $ "No entry for entity in schemaState: " <> show name
     in
        case partitionEithers (map go defsToMigrate) of
            ([], xs) -> Right (concat xs)
            (errs, _) -> Left errs

migrateEntityFromSchemaState
    :: EntitySchemaState
    -> [EntityDef]
    -> EntityDef
    -> [AlterDB]
migrateEntityFromSchemaState schemaState allDefs entity =
    case schemaState of
        EntityDoesNotExist ->
            (addTable newcols entity) : uniques ++ references ++ foreignsAlt
        EntityExists ExistingEntitySchemaState{essColumns, essUniqueConstraints} ->
            let
                (acs, ats) =
                    getAlters
                        allDefs
                        entity
                        (newcols, udspair)
                        ( map pickColumnReference (Map.elems essColumns)
                        , Map.toList essUniqueConstraints
                        )
                acs' = map (AlterColumn name) acs
                ats' = map (AlterTable name) ats
             in
                acs' ++ ats'
  where
    name = getEntityDBName entity
    (newcols', udefs, fdefs) = postgresMkColumns allDefs entity
    newcols = filter (not . safeToRemove entity . cName) newcols'
    udspair = map udToPair udefs

    uniques = flip concatMap udspair $ \(uname, ucols) ->
        [AlterTable name $ AddUniqueConstraint uname ucols]
    references =
        mapMaybe
            ( \Column{cName, cReference} ->
                getAddReference allDefs entity cName =<< cReference
            )
            newcols
    foreignsAlt = mapMaybe (mkForeignAlt entity) fdefs

    -- HACK! This was added to preserve existing behaviour during a refactor.
    -- The migrator currently expects to only see cReference set in the old
    -- columns if it is also set in the new ones. It also ignores any existing
    -- FK constraints in the database that don't match the expected FK
    -- constraint name as defined by the Persistent EntityDef.
    --
    -- This means that the migrator sometimes behaves incorrectly for standalone
    -- Foreign declarations, like Child in the ForeignKey test in
    -- persistent-test, as well as in situations where there are duplicate FK
    -- constraints for a given column.
    --
    -- See https://github.com/yesodweb/persistent/issues/1611#issuecomment-3613251095 for
    -- more info
    pickColumnReference (oldCol, oldReferences) =
        case List.find (\c -> cName c == cName oldCol) newcols of
            Just new -> fromMaybe oldCol $ do
                -- Note that if this do block evaluates to Nothing, it means
                -- we'll return a Column that has cReference = Nothing -
                -- effectively, we are telling the migrator that this particular
                -- column has no FK constraints in the DB.

                -- If the persistent models don't define a FK constraint, ignore
                -- any FK constraints that might exist in the DB (this is
                -- arguably a bug, but it's a pre-existing one)
                newRef <- cReference new

                -- If the persistent models _do_ define an FK constraint but
                -- there's no matching FK constraint in the DB, we don't have
                -- to do anything else here: `getAlters` should handle adding
                -- the FK constraint for us
                oldRef <-
                    List.find
                        (\oldRef -> crConstraintName oldRef == crConstraintName newRef)
                        oldReferences

                -- Finally, if the persistent models define an FK constraint and
                -- an FK constraint of that name exists in the DB, return it, so
                -- that `getAlters` can check that the constraint is set up
                -- correctly
                pure $ oldCol{cReference = Just oldRef}
            Nothing ->
                -- We have a column that exists in the DB but not in the
                -- EntityDef. We can no-op here, since `getAlters` will handle
                -- dropping this for us.
                oldCol

-- | Indicates whether a Postgres Column is safe to drop.
--
-- @since 2.17.1.0
newtype SafeToRemove = SafeToRemove Bool
    deriving (Show, Eq)

-- | Represents a change to a Postgres column in a DB statement.
--
-- @since 2.17.1.0
data AlterColumn
    = ChangeType Column SqlType Text
    | IsNull Column
    | NotNull Column
    | AddColumn Column
    | Drop Column SafeToRemove
    | Default Column Text
    | NoDefault Column
    | UpdateNullToValue Column Text
    | AddReference
        EntityNameDB
        ConstraintNameDB
        (NEL.NonEmpty FieldNameDB)
        [Text]
        FieldCascade
    | DropReference ConstraintNameDB
    deriving (Show, Eq)

-- | Represents a change to a Postgres table in a DB statement.
--
-- @since 2.17.1.0
data AlterTable
    = AddUniqueConstraint ConstraintNameDB [FieldNameDB]
    | DropConstraint ConstraintNameDB
    deriving (Show, Eq)

-- | Represents a change to a Postgres DB in a statement.
--
-- @since 2.17.1.0
data AlterDB
    = AddTable EntityNameDB EntityIdDef [Column]
    | AlterColumn EntityNameDB AlterColumn
    | AlterTable EntityNameDB AlterTable
    deriving (Show, Eq)

-- | Create a table if it doesn't exist.
--
-- @since 2.17.1.0
addTable :: [Column] -> EntityDef -> AlterDB
addTable cols entity =
    AddTable name entityId nonIdCols
  where
    nonIdCols =
        case entityPrimary entity of
            Just _ ->
                cols
            _ ->
                filter keepField cols
      where
        keepField c =
            Just (cName c) /= fmap fieldDB (getEntityIdField entity)
                && not (safeToRemove entity (cName c))
    entityId = getEntityId entity
    name = getEntityDBName entity

maySerial :: SqlType -> Maybe Text -> Text
maySerial SqlInt64 Nothing = " SERIAL8 "
maySerial sType _ = " " <> showSqlType sType

mayDefault :: Maybe Text -> Text
mayDefault def = case def of
    Nothing -> ""
    Just d -> " DEFAULT " <> d

getAlters
    :: [EntityDef]
    -> EntityDef
    -> ([Column], [(ConstraintNameDB, [FieldNameDB])])
    -> ([Column], [(ConstraintNameDB, [FieldNameDB])])
    -> ([AlterColumn], [AlterTable])
getAlters defs def (c1, u1) (c2, u2) =
    (getAltersC c1 c2, getAltersU u1 u2)
  where
    getAltersC [] old =
        map (\x -> Drop x $ SafeToRemove $ safeToRemove def $ cName x) old
    getAltersC (new : news) old =
        let
            (alters, old') = findAlters defs def new old
         in
            alters ++ getAltersC news old'

    getAltersU
        :: [(ConstraintNameDB, [FieldNameDB])]
        -> [(ConstraintNameDB, [FieldNameDB])]
        -> [AlterTable]
    getAltersU [] old =
        map DropConstraint $ filter (not . isManual) $ map fst old
    getAltersU ((name, cols) : news) old =
        case lookup name old of
            Nothing ->
                AddUniqueConstraint name cols : getAltersU news old
            Just ocols ->
                let
                    old' = filter (\(x, _) -> x /= name) old
                 in
                    if sort cols == sort ocols
                        then getAltersU news old'
                        else
                            DropConstraint name
                                : AddUniqueConstraint name cols
                                : getAltersU news old'

    -- Don't drop constraints which were manually added.
    isManual (ConstraintNameDB x) = "__manual_" `T.isPrefixOf` x

-- | Postgres' default maximum identifier length in bytes
-- (You can re-compile Postgres with a new limit, but I'm assuming that virtually noone does this).
-- See https://www.postgresql.org/docs/11/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
maximumIdentifierLength :: Int
maximumIdentifierLength = 63

-- | Intelligent comparison of SQL types, to account for SqlInt32 vs SqlOther integer
sqlTypeEq :: SqlType -> SqlType -> Bool
sqlTypeEq x y =
    let
        -- Non exhaustive helper to map postgres aliases to the same name. Based on
        -- https://www.postgresql.org/docs/9.5/datatype.html.
        -- This prevents needless `ALTER TYPE`s when the type is the same.
        normalize "int8" = "bigint"
        normalize "serial8" = "bigserial"
        normalize v = v
     in
        normalize (T.toCaseFold (showSqlType x))
            == normalize (T.toCaseFold (showSqlType y))

-- We check if we should alter a foreign key. This is almost an equality check,
-- except we consider 'Nothing' and 'Just Restrict' equivalent.
equivalentRef :: Maybe ColumnReference -> Maybe ColumnReference -> Bool
equivalentRef Nothing Nothing = True
equivalentRef (Just cr1) (Just cr2) =
    crTableName cr1 == crTableName cr2
        && crConstraintName cr1 == crConstraintName cr2
        && eqCascade (fcOnUpdate $ crFieldCascade cr1) (fcOnUpdate $ crFieldCascade cr2)
        && eqCascade (fcOnDelete $ crFieldCascade cr1) (fcOnDelete $ crFieldCascade cr2)
  where
    eqCascade :: Maybe CascadeAction -> Maybe CascadeAction -> Bool
    eqCascade Nothing Nothing = True
    eqCascade Nothing (Just Restrict) = True
    eqCascade (Just Restrict) Nothing = True
    eqCascade (Just cs1) (Just cs2) = cs1 == cs2
    eqCascade _ _ = False
equivalentRef _ _ = False

-- | Generate the default foreign key constraint name for a given source table and
-- source column name. Note that this function should generally not be used
-- except as an argument to postgresMkColumns, because if you use it in other contexts,
-- you're likely to miss nonstandard constraint names declared in the persistent
-- models files via `constraint=`
refName :: EntityNameDB -> FieldNameDB -> ConstraintNameDB
refName (EntityNameDB table) (FieldNameDB column) =
    let
        overhead = T.length $ T.concat ["_", "_fkey"]
        (fromTable, fromColumn) = shortenNames overhead (T.length table, T.length column)
     in
        ConstraintNameDB $
            T.concat [T.take fromTable table, "_", T.take fromColumn column, "_fkey"]
  where
    -- Postgres automatically truncates too long foreign keys to a combination of
    -- truncatedTableName + "_" + truncatedColumnName + "_fkey"
    -- This works fine for normal use cases, but it creates an issue for Persistent
    -- Because after running the migrations, Persistent sees the truncated foreign key constraint
    -- doesn't have the expected name, and suggests that you migrate again
    -- To workaround this, we copy the Postgres truncation approach before sending foreign key constraints to it.
    --
    -- I believe this will also be an issue for extremely long table names,
    -- but it's just much more likely to exist with foreign key constraints because they're usually tablename * 2 in length

    -- Approximation of the algorithm Postgres uses to truncate identifiers
    -- See makeObjectName https://github.com/postgres/postgres/blob/5406513e997f5ee9de79d4076ae91c04af0c52f6/src/backend/commands/indexcmds.c#L2074-L2080
    shortenNames :: Int -> (Int, Int) -> (Int, Int)
    shortenNames overhead (x, y)
        | x + y + overhead <= maximumIdentifierLength = (x, y)
        | x > y = shortenNames overhead (x - 1, y)
        | otherwise = shortenNames overhead (x, y - 1)

postgresMkColumns
    :: [EntityDef] -> EntityDef -> ([Column], [UniqueDef], [ForeignDef])
postgresMkColumns allDefs t =
    mkColumns allDefs t $
        setBackendSpecificForeignKeyName refName emptyBackendSpecificOverrides

-- | Check if a column name is listed as the "safe to remove" in the entity
-- list.
safeToRemove :: EntityDef -> FieldNameDB -> Bool
safeToRemove def (FieldNameDB colName) =
    any (elem FieldAttrSafeToRemove . fieldAttrs) $
        filter ((== FieldNameDB colName) . fieldDB) $
            allEntityFields
  where
    allEntityFields =
        getEntityFieldsDatabase def <> case getEntityId def of
            EntityIdField fdef ->
                [fdef]
            _ ->
                []

udToPair :: UniqueDef -> (ConstraintNameDB, [FieldNameDB])
udToPair ud = (uniqueDBName ud, map snd $ NEL.toList $ uniqueFields ud)

-- | Get the references to be added to a table for the given column.
getAddReference
    :: [EntityDef]
    -> EntityDef
    -> FieldNameDB
    -> ColumnReference
    -> Maybe AlterDB
getAddReference allDefs entity cname cr@ColumnReference{crTableName = s, crConstraintName = constraintName} = do
    guard $ Just cname /= fmap fieldDB (getEntityIdField entity)
    pure $
        AlterColumn
            table
            (AddReference s constraintName (cname NEL.:| []) id_ (crFieldCascade cr))
  where
    table = getEntityDBName entity
    id_ =
        fromMaybe
            (error $ "Could not find ID of entity " ++ show s)
            $ do
                entDef <- find ((== s) . getEntityDBName) allDefs
                return $ NEL.toList $ Util.dbIdColumnsEsc escapeF entDef

mkForeignAlt
    :: EntityDef
    -> ForeignDef
    -> Maybe AlterDB
mkForeignAlt entity fdef = case NEL.nonEmpty childfields of
    Nothing -> Nothing
    Just childfields' -> Just $ AlterColumn tableName_ addReference
      where
        addReference =
            AddReference
                (foreignRefTableDBName fdef)
                constraintName
                childfields'
                escapedParentFields
                (foreignFieldCascade fdef)
  where
    tableName_ = getEntityDBName entity
    constraintName =
        foreignConstraintNameDBName fdef
    (childfields, parentfields) =
        unzip (map (\((_, b), (_, d)) -> (b, d)) (foreignFields fdef))
    escapedParentFields =
        map escapeF parentfields

escapeC :: ConstraintNameDB -> Text
escapeC = escapeWith escape

escapeE :: EntityNameDB -> Text
escapeE = escapeWith escape

escapeF :: FieldNameDB -> Text
escapeF = escapeWith escape

escape :: Text -> Text
escape s =
    T.pack $ '"' : go (T.unpack s) ++ "\""
  where
    go "" = ""
    go ('"' : xs) = "\"\"" ++ go xs
    go (x : xs) = x : go xs

showAlterDb :: AlterDB -> (Bool, Text)
showAlterDb (AddTable name entityId nonIdCols) = (False, rawText)
  where
    idtxt =
        case entityId of
            EntityIdNaturalKey pdef ->
                T.concat
                    [ " PRIMARY KEY ("
                    , T.intercalate "," $ map (escapeF . fieldDB) $ NEL.toList $ compositeFields pdef
                    , ")"
                    ]
            EntityIdField field ->
                let
                    defText = defaultAttribute $ fieldAttrs field
                    sType = fieldSqlType field
                 in
                    T.concat
                        [ escapeF $ fieldDB field
                        , maySerial sType defText
                        , " PRIMARY KEY UNIQUE"
                        , mayDefault defText
                        ]
    rawText =
        T.concat
            -- Lower case e: see Database.Persist.Sql.Migration
            [ "CREATe TABLE " -- DO NOT FIX THE CAPITALIZATION!
            , escapeE name
            , "("
            , idtxt
            , if null nonIdCols then "" else ","
            , T.intercalate "," $ map showColumn nonIdCols
            , ")"
            ]
showAlterDb (AlterColumn t ac) =
    (isUnsafe ac, showAlter t ac)
  where
    isUnsafe (Drop _ (SafeToRemove safeRemove)) = not safeRemove
    isUnsafe _ = False
showAlterDb (AlterTable t at) = (False, showAlterTable t at)

showAlterTable :: EntityNameDB -> AlterTable -> Text
showAlterTable table (AddUniqueConstraint cname cols) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ADD CONSTRAINT "
        , escapeC cname
        , " UNIQUE("
        , T.intercalate "," $ map escapeF cols
        , ")"
        ]
showAlterTable table (DropConstraint cname) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " DROP CONSTRAINT "
        , escapeC cname
        ]

showAlter :: EntityNameDB -> AlterColumn -> Text
showAlter table (ChangeType c t extra) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ALTER COLUMN "
        , escapeF (cName c)
        , " TYPE "
        , showSqlType t
        , extra
        ]
showAlter table (IsNull c) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ALTER COLUMN "
        , escapeF (cName c)
        , " DROP NOT NULL"
        ]
showAlter table (NotNull c) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ALTER COLUMN "
        , escapeF (cName c)
        , " SET NOT NULL"
        ]
showAlter table (AddColumn col) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ADD COLUMN "
        , showColumn col
        ]
showAlter table (Drop c _) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " DROP COLUMN "
        , escapeF (cName c)
        ]
showAlter table (Default c s) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ALTER COLUMN "
        , escapeF (cName c)
        , " SET DEFAULT "
        , s
        ]
showAlter table (NoDefault c) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ALTER COLUMN "
        , escapeF (cName c)
        , " DROP DEFAULT"
        ]
showAlter table (UpdateNullToValue c s) =
    T.concat
        [ "UPDATE "
        , escapeE table
        , " SET "
        , escapeF (cName c)
        , "="
        , s
        , " WHERE "
        , escapeF (cName c)
        , " IS NULL"
        ]
showAlter table (AddReference reftable fkeyname t2 id2 cascade) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " ADD CONSTRAINT "
        , escapeC fkeyname
        , " FOREIGN KEY("
        , T.intercalate "," $ map escapeF $ NEL.toList t2
        , ") REFERENCES "
        , escapeE reftable
        , "("
        , T.intercalate "," id2
        , ")"
        ]
        <> renderFieldCascade cascade
showAlter table (DropReference cname) =
    T.concat
        [ "ALTER TABLE "
        , escapeE table
        , " DROP CONSTRAINT "
        , escapeC cname
        ]

showColumn :: Column -> Text
showColumn (Column n nu sqlType' def gen _defConstraintName _maxLen _ref) =
    T.concat
        [ escapeF n
        , " "
        , showSqlType sqlType'
        , " "
        , if nu then "NULL" else "NOT NULL"
        , case def of
            Nothing -> ""
            Just s -> " DEFAULT " <> s
        , case gen of
            Nothing -> ""
            Just s -> " GENERATED ALWAYS AS (" <> s <> ") STORED"
        ]

showSqlType :: SqlType -> Text
showSqlType SqlString = "VARCHAR"
showSqlType SqlInt32 = "INT4"
showSqlType SqlInt64 = "INT8"
showSqlType SqlReal = "DOUBLE PRECISION"
showSqlType (SqlNumeric s prec) = T.concat ["NUMERIC(", T.pack (show s), ",", T.pack (show prec), ")"]
showSqlType SqlDay = "DATE"
showSqlType SqlTime = "TIME"
showSqlType SqlDayTime = "TIMESTAMP WITH TIME ZONE"
showSqlType SqlBlob = "BYTEA"
showSqlType SqlBool = "BOOLEAN"
-- Added for aliasing issues re: https://github.com/yesodweb/yesod/issues/682
showSqlType (SqlOther (T.toLower -> "integer")) = "INT4"
showSqlType (SqlOther t) = t

findAlters
    :: [EntityDef]
    -- ^ The list of all entity definitions that persistent is aware of.
    -> EntityDef
    -- ^ The entity definition for the entity that we're working on.
    -> Column
    -- ^ The column that we're searching for potential alterations for, derived
    -- from the Persistent EntityDef. That is: this is how we _want_ the column
    -- to look, and not necessarily how it actually looks in the database right
    -- now.
    -> [Column]
    -- ^ The columns for this table, as they currently exist in the database.
    -> ([AlterColumn], [Column])
findAlters defs edef newCol oldCols =
    case List.find (\c -> cName c == cName newCol) oldCols of
        Nothing ->
            ([AddColumn newCol] ++ refAdd (cReference newCol), oldCols)
        Just
            oldCol ->
                let
                    refDrop Nothing = []
                    refDrop (Just ColumnReference{crConstraintName = cname}) =
                        [DropReference cname]

                    modRef =
                        if equivalentRef (cReference oldCol) (cReference newCol)
                            then []
                            else refDrop (cReference oldCol) ++ refAdd (cReference newCol)
                    modNull = case (cNull newCol, cNull oldCol) of
                        (True, False) -> do
                            guard $ Just (cName newCol) /= fmap fieldDB (getEntityIdField edef)
                            pure (IsNull newCol)
                        (False, True) ->
                            let
                                up = case cDefault newCol of
                                    Nothing -> id
                                    Just s -> (:) (UpdateNullToValue newCol s)
                             in
                                up [NotNull newCol]
                        _ -> []
                    modType
                        | sqlTypeEq (cSqlType newCol) (cSqlType oldCol) = []
                        -- When converting from Persistent pre-2.0 databases, we
                        -- need to make sure that TIMESTAMP WITHOUT TIME ZONE is
                        -- treated as UTC.
                        | cSqlType newCol == SqlDayTime && cSqlType oldCol == SqlOther "timestamp" =
                            [ ChangeType newCol (cSqlType newCol) $
                                T.concat
                                    [ " USING "
                                    , escapeF (cName newCol)
                                    , " AT TIME ZONE 'UTC'"
                                    ]
                            ]
                        | otherwise = [ChangeType newCol (cSqlType newCol) ""]
                    modDef =
                        if cDefault newCol == cDefault oldCol
                            || isJust (T.stripPrefix "nextval" =<< cDefault oldCol)
                            then []
                            else case cDefault newCol of
                                Nothing -> [NoDefault newCol]
                                Just s -> [Default newCol s]
                    dropSafe =
                        if safeToRemove edef (cName newCol)
                            then error "wtf" [Drop newCol (SafeToRemove True)]
                            else []
                 in
                    ( modRef ++ modDef ++ modNull ++ modType ++ dropSafe
                    , filter (\c -> cName c /= cName newCol) oldCols
                    )
  where
    refAdd Nothing = []
    -- This check works around a bug where persistent will sometimes
    -- generate an erroneous ForeignRef for ID fields.
    -- See: https://github.com/yesodweb/persistent/issues/1615
    refAdd _ | fmap fieldDB (getEntityIdField edef) == Just (cName newCol) = []
    refAdd (Just colRef) =
        case find ((== crTableName colRef) . getEntityDBName) defs of
            Just refdef ->
                [ AddReference
                    (crTableName colRef)
                    (crConstraintName colRef)
                    (cName newCol NEL.:| [])
                    (NEL.toList $ Util.dbIdColumnsEsc escapeF refdef)
                    (crFieldCascade colRef)
                ]
            Nothing ->
                error $
                    "could not find the entityDef for reftable["
                        ++ show (crTableName colRef)
                        ++ "]"
