{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Persist.Postgresql.Internal
    ( P (..)
    , PgInterval (..)
    , getGetter
    , AlterDB (..)
    , AlterTable (..)
    , AlterColumn (..)
    , SafeToRemove
    , migrateStructured
    , mockMigrateStructured
    , addTable
    , findAlters
    , maySerial
    , mayDefault
    , showSqlType
    , showColumn
    , showAlter
    , showAlterDb
    , showAlterTable
    , getAddReference
    , udToPair
    , safeToRemove
    , postgresMkColumns
    , getAlters
    , escapeE
    , escapeF
    , escape
    ) where

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PGFF
import qualified Database.PostgreSQL.Simple.Internal as PG
import qualified Database.PostgreSQL.Simple.Interval as Interval
import qualified Database.PostgreSQL.Simple.ToField as PGTF
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as PS
import qualified Database.PostgreSQL.Simple.Types as PG

import qualified Blaze.ByteString.Builder.Char8 as BBB
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Unlift (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Data.Acquire (with)
import Data.Bits (toIntegralSized)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Data (Typeable)
import Data.Either (partitionEithers)
import Data.Fixed (Fixed (..), Micro, Pico)
import Data.Function (on)
import qualified Data.IntMap as I
import Data.List as List (find, foldl', groupBy, sort)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
    ( NominalDiffTime
    , localTimeToUTC
    , nominalDiffTimeToSeconds
    , secondsToNominalDiffTime
    , utc
    )
import Database.Persist.Sql
import qualified Database.Persist.Sql.Util as Util

-- | Newtype used to avoid orphan instances for @postgresql-simple@ classes.
--
-- @since 2.13.2.0
newtype P = P {unP :: PersistValue}

instance PGTF.ToField P where
    toField (P (PersistText t)) = PGTF.toField t
    toField (P (PersistByteString bs)) = PGTF.toField (PG.Binary bs)
    toField (P (PersistInt64 i)) = PGTF.toField i
    toField (P (PersistDouble d)) = PGTF.toField d
    toField (P (PersistRational r)) =
        PGTF.Plain $
            BBB.fromString $
                show (fromRational r :: Pico) --  FIXME: Too Ambigous, can not select precision without information about field
    toField (P (PersistBool b)) = PGTF.toField b
    toField (P (PersistDay d)) = PGTF.toField d
    toField (P (PersistTimeOfDay t)) = PGTF.toField t
    toField (P (PersistUTCTime t)) = PGTF.toField t
    toField (P PersistNull) = PGTF.toField PG.Null
    toField (P (PersistList l)) = PGTF.toField $ listToJSON l
    toField (P (PersistMap m)) = PGTF.toField $ mapToJSON m
    toField (P (PersistLiteral_ DbSpecific s)) = PGTF.toField (Unknown s)
    toField (P (PersistLiteral_ Unescaped l)) = PGTF.toField (UnknownLiteral l)
    toField (P (PersistLiteral_ Escaped e)) = PGTF.toField (Unknown e)
    toField (P (PersistArray a)) = PGTF.toField $ PG.PGArray $ P <$> a
    toField (P (PersistObjectId _)) =
        error "Refusing to serialize a PersistObjectId to a PostgreSQL value"

instance PGFF.FromField P where
    fromField field mdata = fmap P $ case mdata of
        -- If we try to simply decode based on oid, we will hit unexpected null
        -- errors.
        Nothing -> pure PersistNull
        data' -> getGetter (PGFF.typeOid field) field data'

newtype Unknown = Unknown {unUnknown :: ByteString}
    deriving (Eq, Show, Read, Ord)

instance PGFF.FromField Unknown where
    fromField f mdata =
        case mdata of
            Nothing ->
                PGFF.returnError
                    PGFF.UnexpectedNull
                    f
                    "Database.Persist.Postgresql/PGFF.FromField Unknown"
            Just dat -> return (Unknown dat)

instance PGTF.ToField Unknown where
    toField (Unknown a) = PGTF.Escape a

newtype UnknownLiteral = UnknownLiteral {unUnknownLiteral :: ByteString}
    deriving (Eq, Show, Read, Ord, Typeable)

instance PGFF.FromField UnknownLiteral where
    fromField f mdata =
        case mdata of
            Nothing ->
                PGFF.returnError
                    PGFF.UnexpectedNull
                    f
                    "Database.Persist.Postgresql/PGFF.FromField UnknownLiteral"
            Just dat -> return (UnknownLiteral dat)

instance PGTF.ToField UnknownLiteral where
    toField (UnknownLiteral a) = PGTF.Plain $ BB.byteString a

type Getter a = PGFF.FieldParser a

convertPV :: (PGFF.FromField a) => (a -> b) -> Getter b
convertPV f = (fmap f .) . PGFF.fromField

builtinGetters :: I.IntMap (Getter PersistValue)
builtinGetters =
    I.fromList
        [ (k PS.bool, convertPV PersistBool)
        , (k PS.bytea, convertPV (PersistByteString . unBinary))
        , (k PS.char, convertPV PersistText)
        , (k PS.name, convertPV PersistText)
        , (k PS.int8, convertPV PersistInt64)
        , (k PS.int2, convertPV PersistInt64)
        , (k PS.int4, convertPV PersistInt64)
        , (k PS.text, convertPV PersistText)
        , (k PS.xml, convertPV (PersistByteString . unUnknown))
        , (k PS.float4, convertPV PersistDouble)
        , (k PS.float8, convertPV PersistDouble)
        , (k PS.money, convertPV PersistRational)
        , (k PS.bpchar, convertPV PersistText)
        , (k PS.varchar, convertPV PersistText)
        , (k PS.date, convertPV PersistDay)
        , (k PS.time, convertPV PersistTimeOfDay)
        , (k PS.timestamp, convertPV (PersistUTCTime . localTimeToUTC utc))
        , (k PS.timestamptz, convertPV PersistUTCTime)
        , (k PS.interval, convertPV $ toPersistValue @Interval.Interval)
        , (k PS.bit, convertPV PersistInt64)
        , (k PS.varbit, convertPV PersistInt64)
        , (k PS.numeric, convertPV PersistRational)
        , (k PS.void, \_ _ -> return PersistNull)
        , (k PS.json, convertPV (PersistByteString . unUnknown))
        , (k PS.jsonb, convertPV (PersistByteString . unUnknown))
        , (k PS.unknown, convertPV (PersistByteString . unUnknown))
        , -- Array types: same order as above.
          -- The OIDs were taken from pg_type.
          (1000, listOf PersistBool)
        , (1001, listOf (PersistByteString . unBinary))
        , (1002, listOf PersistText)
        , (1003, listOf PersistText)
        , (1016, listOf PersistInt64)
        , (1005, listOf PersistInt64)
        , (1007, listOf PersistInt64)
        , (1009, listOf PersistText)
        , (143, listOf (PersistByteString . unUnknown))
        , (1021, listOf PersistDouble)
        , (1022, listOf PersistDouble)
        , (1023, listOf PersistUTCTime)
        , (1024, listOf PersistUTCTime)
        , (791, listOf PersistRational)
        , (1014, listOf PersistText)
        , (1015, listOf PersistText)
        , (1182, listOf PersistDay)
        , (1183, listOf PersistTimeOfDay)
        , (1115, listOf PersistUTCTime)
        , (1185, listOf PersistUTCTime)
        , (1187, listOf $ toPersistValue @Interval.Interval)
        , (1561, listOf PersistInt64)
        , (1563, listOf PersistInt64)
        , (1231, listOf PersistRational)
        , -- no array(void) type
          (2951, listOf (PersistLiteralEscaped . unUnknown))
        , (199, listOf (PersistByteString . unUnknown))
        , (3807, listOf (PersistByteString . unUnknown))
        -- no array(unknown) either
        ]
  where
    k (PGFF.typoid -> i) = PG.oid2int i
    -- A @listOf f@ will use a @PGArray (Maybe T)@ to convert
    -- the values to Haskell-land.  The @Maybe@ is important
    -- because the usual way of checking NULLs
    -- (c.f. withStmt') won't check for NULL inside
    -- arrays---or any other compound structure for that matter.
    listOf f = convertPV (PersistList . map (nullable f) . PG.fromPGArray)
      where
        nullable = maybe PersistNull

-- | Get the field parser corresponding to the given 'PG.Oid'.
--
-- For example, pass in the 'PG.Oid' of 'PS.bool', and you will get back a
-- field parser which parses boolean values in the table into 'PersistBool's.
--
-- @since 2.13.2.0
getGetter :: PG.Oid -> Getter PersistValue
getGetter oid =
    fromMaybe defaultGetter $ I.lookup (PG.oid2int oid) builtinGetters
  where
    defaultGetter = convertPV (PersistLiteralEscaped . unUnknown)

unBinary :: PG.Binary a -> a
unBinary (PG.Binary x) = x

-- | Represent Postgres interval using NominalDiffTime
--
-- Note that this type cannot be losslessly round tripped through PostgreSQL.
-- For example the value @'PgInterval' 0.0000009@ will truncate extra
-- precision. And the value @'PgInterval'  9223372036854.775808@ will overflow.
-- Use the 'Interval.Interval' type if that is a problem for you.
--
-- @since 2.11.0.0
newtype PgInterval = PgInterval {getPgInterval :: NominalDiffTime}
    deriving (Eq, Show)

instance PGTF.ToField PgInterval where
    toField = PGTF.toField . pgIntervalToInterval

instance PGFF.FromField PgInterval where
    fromField f =
        maybe (PGFF.returnError PGFF.ConversionFailed f "invalid interval") pure
            . intervalToPgInterval
            <=< PGFF.fromField f

instance PersistField PgInterval where
    toPersistValue =
        toPersistValue
            . pgIntervalToInterval
    fromPersistValue =
        maybe (Left "invalid interval") pure
            . intervalToPgInterval
            <=< fromPersistValue

instance PersistFieldSql PgInterval where
    sqlType _ = SqlOther "interval"

pgIntervalToInterval :: PgInterval -> Interval.Interval
pgIntervalToInterval =
    Interval.fromTimeSaturating mempty
        . getPgInterval

intervalToPgInterval :: Interval.Interval -> Maybe PgInterval
intervalToPgInterval interval =
    let
        (calendarDiffDays, nominalDiffTime) = Interval.intoTime interval
     in
        if calendarDiffDays == mempty
            then Just $ PgInterval nominalDiffTime
            else Nothing

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
migrateStructured allDefs getter entity = do
    old <- getColumns getter entity newcols'
    case partitionEithers old of
        ([], old'') -> do
            exists' <-
                if null old
                    then doesTableExist getter name
                    else return True
            return $ Right $ migrationText exists' old''
        (errs, _) -> return $ Left errs
  where
    name = getEntityDBName entity
    (newcols', udefs, fdefs) = postgresMkColumns allDefs entity
    migrationText exists' old''
        | not exists' =
            createText newcols fdefs udspair
        | otherwise =
            let
                (acs, ats) =
                    getAlters allDefs entity (newcols, udspair) old'
                acs' = map (AlterColumn name) acs
                ats' = map (AlterTable name) ats
             in
                acs' ++ ats'
      where
        old' = partitionEithers old''
        newcols = filter (not . safeToRemove entity . cName) newcols'
        udspair = map udToPair udefs
    -- Check for table existence if there are no columns, workaround
    -- for https://github.com/yesodweb/persistent/issues/152

    createText newcols fdefs_ udspair =
        (addTable newcols entity) : uniques ++ references ++ foreignsAlt
      where
        uniques = flip concatMap udspair $ \(uname, ucols) ->
            [AlterTable name $ AddUniqueConstraint uname ucols]
        references =
            mapMaybe
                ( \Column{cName, cReference} ->
                    getAddReference allDefs entity cName =<< cReference
                )
                newcols
        foreignsAlt = mapMaybe (mkForeignAlt entity) fdefs_

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
mockMigrateStructured allDefs entity = migrationText
  where
    name = getEntityDBName entity
    migrationText = createText newcols fdefs udspair
      where
        (newcols', udefs, fdefs) = postgresMkColumns allDefs entity
        newcols = filter (not . safeToRemove entity . cName) newcols'
        udspair = map udToPair udefs
    -- Check for table existence if there are no columns, workaround
    -- for https://github.com/yesodweb/persistent/issues/152

    createText newcols fdefs udspair =
        (addTable newcols entity) : uniques ++ references ++ foreignsAlt
      where
        uniques = flip concatMap udspair $ \(uname, ucols) ->
            [AlterTable name $ AddUniqueConstraint uname ucols]
        references =
            mapMaybe
                ( \Column{cName, cReference} ->
                    getAddReference allDefs entity cName =<< cReference
                )
                newcols
        foreignsAlt = mapMaybe (mkForeignAlt entity) fdefs

-- | Returns a structured representation of all of the
-- DB changes required to migrate the Entity from its current state
-- in the database to the state described in Haskell.
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
    -- ^ The column that we're searching for potential alterations for.
    -> [Column]
    -> ([AlterColumn], [Column])
findAlters defs edef col@(Column name isNull sqltype def _gen _defConstraintName _maxLen ref) cols =
    case List.find (\c -> cName c == name) cols of
        Nothing ->
            ([AddColumn col], cols)
        Just
            (Column _oldName isNull' sqltype' def' _gen' _defConstraintName' _maxLen' ref') ->
                let
                    refDrop Nothing = []
                    refDrop (Just ColumnReference{crConstraintName = cname}) =
                        [DropReference cname]

                    refAdd Nothing = []
                    refAdd (Just colRef) =
                        case find ((== crTableName colRef) . getEntityDBName) defs of
                            Just refdef
                                | Just _oldName /= fmap fieldDB (getEntityIdField edef) ->
                                    [ AddReference
                                        (crTableName colRef)
                                        (crConstraintName colRef)
                                        (name NEL.:| [])
                                        (NEL.toList $ Util.dbIdColumnsEsc escapeF refdef)
                                        (crFieldCascade colRef)
                                    ]
                            Just _ -> []
                            Nothing ->
                                error $
                                    "could not find the entityDef for reftable["
                                        ++ show (crTableName colRef)
                                        ++ "]"
                    modRef =
                        if equivalentRef ref ref'
                            then []
                            else refDrop ref' ++ refAdd ref
                    modNull = case (isNull, isNull') of
                        (True, False) -> do
                            guard $ Just name /= fmap fieldDB (getEntityIdField edef)
                            pure (IsNull col)
                        (False, True) ->
                            let
                                up = case def of
                                    Nothing -> id
                                    Just s -> (:) (UpdateNullToValue col s)
                             in
                                up [NotNull col]
                        _ -> []
                    modType
                        | sqlTypeEq sqltype sqltype' = []
                        -- When converting from Persistent pre-2.0 databases, we
                        -- need to make sure that TIMESTAMP WITHOUT TIME ZONE is
                        -- treated as UTC.
                        | sqltype == SqlDayTime && sqltype' == SqlOther "timestamp" =
                            [ ChangeType col sqltype $
                                T.concat
                                    [ " USING "
                                    , escapeF name
                                    , " AT TIME ZONE 'UTC'"
                                    ]
                            ]
                        | otherwise = [ChangeType col sqltype ""]
                    modDef =
                        if def == def'
                            || isJust (T.stripPrefix "nextval" =<< def')
                            then []
                            else case def of
                                Nothing -> [NoDefault col]
                                Just s -> [Default col s]
                    dropSafe =
                        if safeToRemove edef name
                            then error "wtf" [Drop col (SafeToRemove True)]
                            else []
                 in
                    ( modRef ++ modDef ++ modNull ++ modType ++ dropSafe
                    , filter (\c -> cName c /= name) cols
                    )

-- | Returns all of the columns in the given table currently in the database.
getColumns
    :: (Text -> IO Statement)
    -> EntityDef
    -> [Column]
    -> IO [Either Text (Either Column (ConstraintNameDB, [FieldNameDB]))]
getColumns getter def cols = do
    let
        sqlv =
            T.concat
                [ "SELECT "
                , "column_name "
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
                , "AND table_name=? "
                ]

    -- DOMAINS Postgres supports the concept of domains, which are data types
    -- with optional constraints.  An app might make an "email" domain over the
    -- varchar type, with a CHECK that the emails are valid In this case the
    -- generated SQL should use the domain name: ALTER TABLE users ALTER COLUMN
    -- foo TYPE email This code exists to use the domain name (email), instead
    -- of the underlying type (varchar).  This is tested in
    -- EquivalentTypeTest.hs

    stmt <- getter sqlv
    let
        vals =
            [ PersistText $ unEntityNameDB $ getEntityDBName def
            ]
    columns <-
        with
            (stmtQuery stmt vals)
            (\src -> runConduit $ src .| processColumns .| CL.consume)
    let
        sqlc =
            T.concat
                [ "SELECT "
                , "c.constraint_name, "
                , "c.column_name "
                , "FROM information_schema.key_column_usage AS c, "
                , "information_schema.table_constraints AS k "
                , "WHERE c.table_catalog=current_database() "
                , "AND c.table_catalog=k.table_catalog "
                , "AND c.table_schema=current_schema() "
                , "AND c.table_schema=k.table_schema "
                , "AND c.table_name=? "
                , "AND c.table_name=k.table_name "
                , "AND c.constraint_name=k.constraint_name "
                , "AND NOT k.constraint_type IN ('PRIMARY KEY', 'FOREIGN KEY') "
                , "ORDER BY c.constraint_name, c.column_name"
                ]

    stmt' <- getter sqlc

    us <- with (stmtQuery stmt' vals) (\src -> runConduit $ src .| helperU)
    return $ columns ++ us
  where
    refMap =
        fmap (\cr -> (crTableName cr, crConstraintName cr)) $
            Map.fromList $
                List.foldl' ref [] cols
      where
        ref rs c =
            maybe rs (\r -> (unFieldNameDB $ cName c, r) : rs) (cReference c)
    getAll =
        CL.mapM $ \x ->
            pure $ case x of
                [PersistText con, PersistText col] ->
                    (con, col)
                [PersistByteString con, PersistByteString col] ->
                    (T.decodeUtf8 con, T.decodeUtf8 col)
                o ->
                    error $ "unexpected datatype returned for postgres o=" ++ show o
    helperU = do
        rows <- getAll .| CL.consume
        return
            $ map
                (Right . Right . (ConstraintNameDB . fst . head &&& map (FieldNameDB . snd)))
            $ groupBy ((==) `on` fst) rows
    processColumns =
        CL.mapM $ \x'@((PersistText cname) : _) -> do
            col <-
                liftIO $ getColumn getter (getEntityDBName def) x' (Map.lookup cname refMap)
            pure $ case col of
                Left e -> Left e
                Right c -> Right $ Left c

getColumn
    :: (Text -> IO Statement)
    -> EntityNameDB
    -> [PersistValue]
    -> Maybe (EntityNameDB, ConstraintNameDB)
    -> IO (Either Text Column)
getColumn
    getter
    tableName'
    [ PersistText columnName
        , PersistText isNullable
        , PersistText typeName
        , defaultValue
        , generationExpression
        , numericPrecision
        , numericScale
        , maxlen
        ]
    refName_ = runExceptT $ do
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

        t <- getType typeStr

        let
            cname = FieldNameDB columnName

        ref <- lift $ fmap join $ traverse (getRef cname) refName_

        return
            Column
                { cName = cname
                , cNull = isNullable == "YES"
                , cSqlType = t
                , cDefault = fmap stripSuffixes defaultValue'
                , cGenerated = fmap stripSuffixes generationExpression'
                , cDefaultConstraintName = Nothing
                , cMaxLen = Nothing
                , cReference = fmap (\(a, b, c, d) -> ColumnReference a b (mkCascade c d)) ref
                }
      where
        mkCascade updText delText =
            FieldCascade
                { fcOnUpdate = parseCascade updText
                , fcOnDelete = parseCascade delText
                }

        parseCascade txt =
            case txt of
                "NO ACTION" ->
                    Nothing
                "CASCADE" ->
                    Just Cascade
                "SET NULL" ->
                    Just SetNull
                "SET DEFAULT" ->
                    Just SetDefault
                "RESTRICT" ->
                    Just Restrict
                _ ->
                    error $ "Unexpected value in parseCascade: " <> show txt

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

        getRef cname (_, refName') = do
            let
                sql =
                    T.concat
                        [ "SELECT DISTINCT "
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
                        , "AND kcu.table_name=? "
                        , "AND kcu.column_name=? "
                        , "AND tc.constraint_name=?"
                        ]
            stmt <- getter sql
            cntrs <-
                with
                    ( stmtQuery
                        stmt
                        [ PersistText $ unEntityNameDB tableName'
                        , PersistText $ unFieldNameDB cname
                        , PersistText $ unConstraintNameDB refName'
                        ]
                    )
                    (\src -> runConduit $ src .| CL.consume)
            case cntrs of
                [] ->
                    return Nothing
                [ [ PersistText table
                        , PersistText constraint
                        , PersistText updRule
                        , PersistText delRule
                        ]
                    ] ->
                        return $
                            Just (EntityNameDB table, ConstraintNameDB constraint, updRule, delRule)
                xs ->
                    error $
                        mconcat
                            [ "Postgresql.getColumn: error fetching constraints. Expected a single result for foreign key query for table: "
                            , T.unpack (unEntityNameDB tableName')
                            , " and column: "
                            , T.unpack (unFieldNameDB cname)
                            , " but got: "
                            , show xs
                            ]

        getType "int4" = pure SqlInt32
        getType "int8" = pure SqlInt64
        getType "varchar" = pure SqlString
        getType "text" = pure SqlString
        getType "date" = pure SqlDay
        getType "bool" = pure SqlBool
        getType "timestamptz" = pure SqlDayTime
        getType "float4" = pure SqlReal
        getType "float8" = pure SqlReal
        getType "bytea" = pure SqlBlob
        getType "time" = pure SqlTime
        getType "numeric" = getNumeric numericPrecision numericScale
        getType a = pure $ SqlOther a

        getNumeric (PersistInt64 a) (PersistInt64 b) =
            pure $ SqlNumeric (fromIntegral a) (fromIntegral b)
        getNumeric PersistNull PersistNull =
            throwError $
                T.concat
                    [ "No precision and scale were specified for the column: "
                    , columnName
                    , " in table: "
                    , unEntityNameDB tableName'
                    , ". Postgres defaults to a maximum scale of 147,455 and precision of 16383,"
                    , " which is probably not what you intended."
                    , " Specify the values as numeric(total_digits, digits_after_decimal_place)."
                    ]
        getNumeric a b =
            throwError $
                T.concat
                    [ "Can not get numeric field precision for the column: "
                    , columnName
                    , " in table: "
                    , unEntityNameDB tableName'
                    , ". Expected an integer for both precision and scale, "
                    , "got: "
                    , T.pack $ show a
                    , " and "
                    , T.pack $ show b
                    , ", respectively."
                    , " Specify the values as numeric(total_digits, digits_after_decimal_place)."
                    ]
getColumn _ _ columnName _ =
    return $
        Left $
            T.pack $
                "Invalid result from information_schema: " ++ show columnName

doesTableExist
    :: (Text -> IO Statement)
    -> EntityNameDB
    -> IO Bool
doesTableExist getter (EntityNameDB name) = do
    stmt <- getter sql
    with (stmtQuery stmt vals) (\src -> runConduit $ src .| start)
  where
    sql =
        "SELECT COUNT(*) FROM pg_catalog.pg_tables WHERE schemaname != 'pg_catalog'"
            <> " AND schemaname != 'information_schema' AND tablename=?"
    vals = [PersistText name]

    start = await >>= maybe (error "No results when checking doesTableExist") start'
    start' [PersistInt64 0] = finish False
    start' [PersistInt64 1] = finish True
    start' res = error $ "doesTableExist returned unexpected result: " ++ show res
    finish x = await >>= maybe (return x) (error "Too many rows returned in doesTableExist")
