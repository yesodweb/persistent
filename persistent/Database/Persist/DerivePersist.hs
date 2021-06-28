{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


-- | This module provides the tools for defining your database schema and using
-- it to generate Haskell data types and migrations.
module Database.Persist.DerivePersist
    ( -- * Parse entity defs

      derivePersist
    , stripEntityNamePrefix
    , OptionalText(..)
    , DeriveFieldDef(..)
    , DeriveEntityDef(..)
    , DeriveForeignKey(..)
    , DeriveUniqueDef(..)
    , mkDeriveEntityDef
    , mkDeriveFieldDef
    , mkDeriveUniqueDef
    ) where


import Data.String (IsString (fromString))
import Database.Persist.TH
import Database.Persist.Quasi (PersistSettings)
import Language.Haskell.TH
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Database.Persist
import Language.Haskell.TH.Datatype
import qualified Language.Haskell.TH.Datatype as THD
import Database.Persist.Types.Base (EntityDef(..), EntityIdDef (..))
import Database.Persist.Quasi.Internal (PersistSettings(..), UnboundEntityDef (..), UnboundForeignDef (..), unbindEntityDef, PrimarySpec (..), mkAutoIdField', isCapitalizedText, UnboundForeignFieldList (..))
import Data.Maybe (fromMaybe)


derivePersist :: MkPersistSettings -> PersistSettings -> [DeriveEntityDef] -> Q [Dec]
derivePersist mps ps defs = do
    ents <- mapM (\ded -> datatypeToEntityDef mps ps ded <$> reifyDatatype (entityTypeName ded)) defs
    let mps' = mps {mpsCreateDataType = False}
    mkPersist mps' ents

-- | Helper data type for better ergonomics.
-- Overriding a string with it does not need Just, which makes it easier to use than @Maybe Text@.
newtype OptionalText = OptionalText {
    fromOptionalText :: Maybe Text
}
instance IsString OptionalText where
  fromString = OptionalText . Just . pack

data DeriveFieldDef = DeriveFieldDef
    { fieldRecordName :: Name
    , sqlNameOverride :: OptionalText
    , sqlTypeOverride :: OptionalText
    , generatedOverride :: OptionalText
    , fieldCascadeOverride :: Maybe FieldCascade
    , fieldAttrOverride :: [FieldAttr]
    }

data DeriveEntityDef = DeriveEntityDef
    { entityTypeName :: Name
    , primaryId :: Maybe (Either DeriveFieldDef [Name])
    , deriveEntityDB :: OptionalText
    , uniques :: [DeriveUniqueDef]
    , deriveFields :: [DeriveFieldDef]
    , foreignKeys :: [DeriveForeignKey]
    }

mkDeriveEntityDef :: Name -> DeriveEntityDef
mkDeriveEntityDef name = DeriveEntityDef
    { entityTypeName = name
    , primaryId = Nothing
    , deriveEntityDB = OptionalText Nothing
    , uniques = []
    , deriveFields = []
    , foreignKeys = []
    }

data DeriveUniqueDef = DeriveUniqueDef
    { uniqueHaskellName :: Text
    , deriveUniqueDBName :: OptionalText
    , deriveUniqueFields :: [Name]
    , forceNullableFields :: Bool
    }

mkDeriveUniqueDef :: Text -> [Name] -> DeriveUniqueDef
mkDeriveUniqueDef name fields = DeriveUniqueDef
    { uniqueHaskellName = name
    , deriveUniqueDBName = OptionalText Nothing
    , deriveUniqueFields = fields
    , forceNullableFields = False
    }

mkDeriveFieldDef :: Name -> DeriveFieldDef
mkDeriveFieldDef name = DeriveFieldDef
    { fieldRecordName = name
    , sqlNameOverride = OptionalText Nothing
    , sqlTypeOverride = OptionalText Nothing
    , generatedOverride = OptionalText Nothing
    , fieldCascadeOverride = Nothing
    , fieldAttrOverride = []
    }  

data DeriveForeignKey = DeriveForeignKey
    { otherEntity :: Name
    , constraintName :: Text
    , ourFields :: [Name]
    , parentFields :: Maybe [Name]
    }

stripEntityNamePrefix :: Text -> Text -> Text
stripEntityNamePrefix entName fieldName = if T.toLower entName `T.isPrefixOf` T.toLower fieldName
    then T.drop (T.length entName) fieldName
    else fieldName

datatypeToEntityDef :: MkPersistSettings -> PersistSettings -> DeriveEntityDef -> DatatypeInfo -> UnboundEntityDef
datatypeToEntityDef mps ps@PersistSettings{..} ded DatatypeInfo{..} = unbound' where
    unbound = unbindEntityDef ent
    unbound' = unbound {
        unboundForeignDefs = foreigns,
        unboundPrimarySpec = case entityIdField of
            EntityIdNaturalKey _ -> unboundPrimarySpec unbound
            -- unbindEntityDef never creates a DefaultKey, so we do it here
            EntityIdField f -> DefaultKey $ fieldDB f
    }
    ent = EntityDef
        { entityHaskell = EntityNameHS entName
        , entityDB = EntityNameDB tableName
        , entityId = entityIdField
        , entityAttrs = []
        , entityFields = map snd fields 
        , entityUniques = entityUniques'
        , entityForeigns = []
        , entityDerives = []
        , entityExtra = mempty
        , entitySum = False 
        , entityComments = Nothing
    }
    entName = pack $ nameBase datatypeName
    tableName = fromMaybe (psToDBName entName) (fromOptionalText $ deriveEntityDB ded)

    fields = fieldToFieldDefs mps ps ded $ case datatypeCons of
        [c] -> c
        _ -> error $ show entName <> ": data type must have a single constructor"

    entityIdField = case primaryId ded of
        Nothing -> EntityIdField $ mkAutoIdField' (FieldNameDB psIdName) (EntityNameHS entName) SqlInt64
        Just (Right names) | null names -> error "No fields on primary composite key."
        Just (Right names) -> EntityIdNaturalKey $ CompositeDef (getFieldByName <$> NEL.fromList names) []
        Just (Left primaryOverride) ->
            -- Only the id column name can be adjusted
            EntityIdField $ mkAutoIdField' (FieldNameDB $ fromMaybe psIdName $ fromOptionalText $ sqlNameOverride primaryOverride) (EntityNameHS entName) SqlInt64

    getFieldByName :: Name -> FieldDef
    getFieldByName name = case lookup name fields of
        Just field -> field
        Nothing -> error $ "datatypeToEntityDef: entity " <> show entName <> "  does not have field " <> show name

    entityUniques' = map mkUniqueDef (uniques ded)

    mkUniqueDef :: DeriveUniqueDef -> UniqueDef
    mkUniqueDef DeriveUniqueDef{..} = UniqueDef {..} where
        uniqueError :: String -> a
        uniqueError msg = error $ "Invalid unique constraint on table[" ++ unpack entName ++ "]: " ++ msg

        uniqueFields = case NEL.nonEmpty deriveUniqueFields of
            Just uniqueFields' -> fmap ((\f -> (fieldHaskell f, fieldDB f)) . getFieldByName) uniqueFields'
            Nothing -> uniqueError "list of fields cannot be empty"
        uniqueHaskell = if isCapitalizedText uniqueHaskellName
            then ConstraintNameHS uniqueHaskellName
            else uniqueError $ "expecting an uppercase constraint name, found =" ++ unpack uniqueHaskellName
        uniqueDBName = ConstraintNameDB $ fromMaybe (psToDBName (tableName `T.append` uniqueHaskellName)) (fromOptionalText deriveUniqueDBName)
        uniqueAttrs = ["!force" | forceNullableFields]

    foreigns = map mkForeign (foreignKeys ded)

    mkForeign :: DeriveForeignKey -> UnboundForeignDef
    mkForeign DeriveForeignKey{..} = UnboundForeignDef foreignFields $ ForeignDef
        { foreignRefTableHaskell = EntityNameHS $ pack $ nameBase otherEntity
        , foreignRefTableDBName = EntityNameDB $ psToDBName $ pack $ nameBase otherEntity
        , foreignConstraintNameHaskell = ConstraintNameHS constraintName
        , foreignConstraintNameDBName =
            ConstraintNameDB $ psToDBName (entName `T.append` constraintName)
        , foreignFieldCascade = FieldCascade
            { fcOnDelete = Nothing
            , fcOnUpdate = Nothing
            }
        , foreignFields = []
        , foreignAttrs = []
        , foreignNullable = False
        , foreignToPrimary = null parentFields
        } where
            foreignFields = FieldListImpliedId $ NEL.fromList $ map toFieldHaskell ourFields
    toFieldHaskell name = FieldNameHS $
        mpsRecordFieldToHaskellName mps entName (pack $ nameBase name)

fieldToFieldDefs :: MkPersistSettings -> PersistSettings -> DeriveEntityDef -> ConstructorInfo -> [(Name, FieldDef)]
fieldToFieldDefs mps PersistSettings{..} ded ConstructorInfo{..} = case constructorVariant of
    RecordConstructor names -> zipWith3 (\name -> toFieldDef name (lookupFieldOverride name)) names constructorFields constructorStrictness
    _ -> error "Data type must have a record constructor"
    where
    lookupFieldOverride :: Name -> Maybe DeriveFieldDef
    lookupFieldOverride =
        let fieldMap = M.fromList $ map (\f -> (fieldRecordName f, f)) $ deriveFields ded
        in flip M.lookup fieldMap

    toFieldDef :: Name -> Maybe DeriveFieldDef -> Type -> FieldStrictness -> (Name, FieldDef)
    toFieldDef name maybeDef typ strictness = (name, FieldDef{
          fieldHaskell = FieldNameHS fieldHaskell
        , fieldDB = FieldNameDB $ fromMaybe (psToDBName fieldHaskell) (maybeDef >>= fromOptionalText . sqlNameOverride)
        , fieldType = fieldType
        , fieldSqlType = SqlOther $ "SqlType unset for " `mappend` pack (show name)
        , fieldAttrs = recordNameAttr: sqlTypeAttr <> nullableAttr <> maybe [] fieldAttrOverride maybeDef
        , fieldStrict = fieldStrictness strictness == THD.Strict
        , fieldReference = NoReference
        , fieldComments = Nothing
        , fieldCascade = FieldCascade (maybeDef >>= fieldCascadeOverride >>= fcOnUpdate) (maybeDef >>= fieldCascadeOverride >>= fcOnDelete)
        , fieldGenerated = maybeDef >>= fromOptionalText . generatedOverride
        , fieldIsImplicitIdColumn = False
        }) where
        -- Save the field name for code generation. It cannot always be inferred from fieldHaskell.
        recordNameAttr = FieldAttrOther $ "recordName=" <> pack (nameBase name)
        entName = pack $ nameBase $ entityTypeName ded
        fieldHaskell = mpsRecordFieldToHaskellName mps entName (pack $ nameBase name)
        (fieldType, isNullable) = decomposeFieldType typ
        sqlTypeAttr = maybe [] (\t -> [FieldAttrSqltype t]) (maybeDef >>= fromOptionalText . sqlTypeOverride)
        nullableAttr = [FieldAttrMaybe | isNullable]

decomposeFieldType :: Type -> (FieldType, Bool)
decomposeFieldType = unwrapMaybe where
    unwrapMaybe (AppT t1 t2) | t1 == ConT ''Maybe = (go t2, True)
    unwrapMaybe t = (go t, False)

    go (ConT name) = FTTypeCon Nothing (pack $ nameBase name)
    go (ParensT t) = go t
    go (AppT ListT t) = FTList (go t)
    go (AppT t1 t2) | t1 == ConT ''Maybe = go t2
    go (AppT t1 t2) = FTApp (go t1) (go t2)
    go t = error $ "Cannot process type: " <> show t
