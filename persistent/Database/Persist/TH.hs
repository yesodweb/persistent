{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides the tools for defining your database schema and using
-- it to generate Haskell data types and migrations.
module Database.Persist.TH
    ( -- * Parse entity defs
      persistWith
    , persistUpperCase
    , persistLowerCase
    , persistFileWith
    , persistManyFileWith
      -- * Turn @EntityDef@s into types
    , mkPersist
    , mkPersistWith
    , MkPersistSettings
    , mpsBackend
    , mpsGeneric
    , mpsPrefixFields
    , mpsFieldLabelModifier
    , mpsConstraintLabelModifier
    , mpsEntityJSON
    , mpsGenerateLenses
    , mpsDeriveInstances
    , EntityJSON(..)
    , mkPersistSettings
    , sqlSettings
    -- ** Implicit ID Columns
    , ImplicitIdDef
    , setImplicitIdDef
      -- * Various other TH functions
    , mkMigrate
    , migrateModels
    , discoverEntities
    , mkSave
    , mkDeleteCascade
    , mkEntityDefList
    , share
    , derivePersistField
    , derivePersistFieldJSON
    , persistFieldFromEntity
      -- * Internal
    , lensPTH
    , parseReferences
    , embedEntityDefs
    , fieldError
    , AtLeastOneUniqueKey(..)
    , OnlyOneUniqueKey(..)
    , pkNewtype
    ) where

-- Development Tip: See persistent-template/README.md for advice on seeing generated Template Haskell code
-- It's highly recommended to check the diff between master and your PR's generated code.

import Prelude hiding (concat, exp, splitAt, take, (++))

import Control.Monad
import Data.Aeson
       ( FromJSON(parseJSON)
       , ToJSON(toJSON)
       , Value(Object)
       , eitherDecodeStrict'
       , object
       , (.:)
       , (.:?)
       , (.=)
       )
import qualified Data.ByteString as BS
import Data.Char (toLower, toUpper)
import Data.Coerce
import Data.Data (Data)
import Data.Either
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.Ix (Ix)
import Data.List (foldl')
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, concat, cons, pack, stripSuffix, uncons, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Instances.TH.Lift ()
    -- Bring `Lift (fmap k v)` instance into scope, as well as `Lift Text`
    -- instance on pre-1.2.4 versions of `text`
import Data.Foldable (toList)
import qualified Data.Set as Set
import Language.Haskell.TH.Lib
       (appT, conE, conK, conT, litT, strTyLit, varE, varP, varT)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import Web.PathPieces (PathPiece(..))

import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Quasi.Internal
import Database.Persist.Sql
       (Migration, PersistFieldSql, SqlBackend, migrate, sqlType)

import Database.Persist.EntityDef.Internal (EntityDef(..))
import Database.Persist.ImplicitIdDef (autoIncrementingInteger)
import Database.Persist.ImplicitIdDef.Internal

-- | Converts a quasi-quoted syntax into a list of entity definitions, to be
-- used as input to the template haskell generation code (mkPersist).
persistWith :: PersistSettings -> QuasiQuoter
persistWith ps = QuasiQuoter
    { quoteExp =
        parseReferences ps . pack
    , quotePat =
        error "persistWith can't be used as pattern"
    , quoteType =
        error "persistWith can't be used as type"
    , quoteDec =
        error "persistWith can't be used as declaration"
    }

-- | Apply 'persistWith' to 'upperCaseSettings'.
persistUpperCase :: QuasiQuoter
persistUpperCase = persistWith upperCaseSettings

-- | Apply 'persistWith' to 'lowerCaseSettings'.
persistLowerCase :: QuasiQuoter
persistLowerCase = persistWith lowerCaseSettings

-- | Same as 'persistWith', but uses an external file instead of a
-- quasiquotation. The recommended file extension is @.persistentmodels@.
persistFileWith :: PersistSettings -> FilePath -> Q Exp
persistFileWith ps fp = persistManyFileWith ps [fp]

-- | Same as 'persistFileWith', but uses several external files instead of
-- one. Splitting your Persistent definitions into multiple modules can
-- potentially dramatically speed up compile times.
--
-- The recommended file extension is @.persistentmodels@.
--
-- ==== __Examples__
--
-- Split your Persistent definitions into multiple files (@models1@, @models2@),
-- then create a new module for each new file and run 'mkPersist' there:
--
-- @
-- -- Model1.hs
-- 'share'
--     ['mkPersist' 'sqlSettings']
--     $('persistFileWith' 'lowerCaseSettings' "models1")
-- @
-- @
-- -- Model2.hs
-- 'share'
--     ['mkPersist' 'sqlSettings']
--     $('persistFileWith' 'lowerCaseSettings' "models2")
-- @
--
-- Use 'persistManyFileWith' to create your migrations:
--
-- @
-- -- Migrate.hs
-- 'share'
--     ['mkMigrate' "migrateAll"]
--     $('persistManyFileWith' 'lowerCaseSettings' ["models1.persistentmodels","models2.persistentmodels"])
-- @
--
-- Tip: To get the same import behavior as if you were declaring all your models in
-- one file, import your new files @as Name@ into another file, then export @module Name@.
--
-- This approach may be used in the future to reduce memory usage during compilation,
-- but so far we've only seen mild reductions.
--
-- See <https://github.com/yesodweb/persistent/issues/778 persistent#778> and
-- <https://github.com/yesodweb/persistent/pull/791 persistent#791> for more details.
--
-- @since 2.5.4
persistManyFileWith :: PersistSettings -> [FilePath] -> Q Exp
persistManyFileWith ps fps = do
    mapM_ qAddDependentFile fps
    ss <- mapM (qRunIO . getFileContents) fps
    let s = T.intercalate "\n" ss -- be tolerant of the user forgetting to put a line-break at EOF.
    parseReferences ps s

getFileContents :: FilePath -> IO Text
getFileContents = fmap decodeUtf8 . BS.readFile

-- | Takes a list of (potentially) independently defined entities and properly
-- links all foreign keys to reference the right 'EntityDef', tying the knot
-- between entities.
--
-- Allows users to define entities indepedently or in separate modules and then
-- fix the cross-references between them at runtime to create a 'Migration'.
--
-- @since 2.7.2
embedEntityDefs
    :: [EntityDef]
    -- ^ A list of 'EntityDef' that have been defined in a previous 'mkPersist'
    -- call.
    --
    -- @since 2.13.0.0
    -> [UnboundEntityDef]
    -> [UnboundEntityDef]
embedEntityDefs eds = snd . embedEntityDefsMap eds

embedEntityDefsMap
    :: [EntityDef]
    -- ^ A list of 'EntityDef' that have been defined in a previous 'mkPersist'
    -- call.
    --
    -- @since 2.13.0.0
    -> [UnboundEntityDef]
    -> (EmbedEntityMap, [UnboundEntityDef])
embedEntityDefsMap existingEnts rawEnts =
    (embedEntityMap, noCycleEnts)
  where
    noCycleEnts = entsWithEmbeds
    embedEntityMap = constructEmbedEntityMap entsWithEmbeds
    entsWithEmbeds = fmap setEmbedEntity (rawEnts <> map unbindEntityDef existingEnts)
    setEmbedEntity ubEnt =
        let
            ent = unboundEntityDef ubEnt
        in
            ubEnt
                { unboundEntityDef =
                    overEntityFields
                        (fmap (setEmbedField (entityHaskell ent) embedEntityMap))
                        ent
                }


-- | Calls 'parse' to Quasi.parse individual entities in isolation
-- afterwards, sets references to other entities
--
-- In 2.13.0.0, this was changed to splice in @['UnboundEntityDef']@
-- instead of @['EntityDef']@.
--
-- @since 2.5.3
parseReferences :: PersistSettings -> Text -> Q Exp
parseReferences ps s = lift $ parse ps s

preprocessUnboundDefs
    :: [EntityDef]
    -> [UnboundEntityDef]
    -> (M.Map EntityNameHS (), [UnboundEntityDef])
preprocessUnboundDefs preexistingEntities unboundDefs =
    (embedEntityMap, noCycleEnts)
  where
    (embedEntityMap, noCycleEnts) =
        embedEntityDefsMap preexistingEntities unboundDefs

stripId :: FieldType -> Maybe Text
stripId (FTTypeCon Nothing t) = stripSuffix "Id" t
stripId _ = Nothing

liftAndFixKeys
    :: MkPersistSettings
    -> M.Map EntityNameHS a
    -> EntityMap
    -> UnboundEntityDef
    -> Q Exp
liftAndFixKeys mps emEntities entityMap unboundEnt =
    let
        ent =
            unboundEntityDef unboundEnt
        fields =
            getUnboundFieldDefs unboundEnt
    in
        [|
    ent
        { entityFields =
            $(ListE <$> traverse combinedFixFieldDef fields)
        , entityId =
            $(fixPrimarySpec mps unboundEnt)
        , entityForeigns =
            $(fixUnboundForeignDefs (unboundForeignDefs unboundEnt))
        }
    |]
  where
    fixUnboundForeignDefs
        :: [UnboundForeignDef]
        -> Q Exp
    fixUnboundForeignDefs fdefs =
        fmap ListE $ forM fdefs fixUnboundForeignDef
      where
        fixUnboundForeignDef UnboundForeignDef{..} =
            [|
            unboundForeignDef
                { foreignFields =
                    $(lift fixForeignFields)
                , foreignNullable =
                    $(lift fixForeignNullable)
                , foreignRefTableDBName =
                    $(lift fixForeignRefTableDBName)
                }
            |]
          where
            fixForeignRefTableDBName =
                entityDB (unboundEntityDef parentDef)
            foreignFieldNames =
                case unboundForeignFields of
                    FieldListImpliedId ffns ->
                        ffns
                    FieldListHasReferences references ->
                        fmap ffrSourceField references
            parentDef =
                case M.lookup parentTableName entityMap of
                    Nothing ->
                        error $ mconcat
                        [ "Foreign table not defined: "
                        , show parentTableName
                        ]
                    Just a ->
                        a
            parentTableName =
                foreignRefTableHaskell unboundForeignDef
            fixForeignFields :: [(ForeignFieldDef, ForeignFieldDef)]
            fixForeignFields =
                case unboundForeignFields of
                    FieldListImpliedId ffns ->
                        mkReferences $ toList ffns
                    FieldListHasReferences references ->
                        toList $ fmap convReferences references
              where
                -- in this case, we're up against the implied ID of the parent
                -- dodgy assumption: columns are listed in the right order. we
                -- can't check this any more clearly right now.
                mkReferences fieldNames
                    | length fieldNames /= length parentKeyFieldNames =
                        error $ mconcat
                            [ "Foreign reference needs to have the same number "
                            , "of fields as the target table."
                            , "\n  Table        : "
                            , show (getUnboundEntityNameHS unboundEnt)
                            , "\n  Foreign Table: "
                            , show parentTableName
                            , "\n  Fields       : "
                            , show fieldNames
                            , "\n  Parent fields: "
                            , show (fmap fst parentKeyFieldNames)
                            , "\n\nYou can use the References keyword to fix this."
                            ]
                    | otherwise =
                        zip (fmap (withDbName fieldStore) fieldNames) parentKeyFieldNames
                  where
                    parentKeyFieldNames
                        :: [(FieldNameHS, FieldNameDB)]
                    parentKeyFieldNames =
                        case unboundPrimarySpec parentDef of
                            NaturalKey ucd ->
                                fmap (withDbName parentFieldStore) (unboundCompositeCols ucd)
                            SurrogateKey uid ->
                                [(FieldNameHS "Id", unboundIdDBName  uid)]
                            DefaultKey dbName ->
                                [(FieldNameHS "Id", dbName)]
                withDbName store fieldNameHS =
                    ( fieldNameHS
                    , findDBName store fieldNameHS
                    )
                convReferences
                    :: ForeignFieldReference
                    -> (ForeignFieldDef, ForeignFieldDef)
                convReferences ForeignFieldReference {..} =
                    ( withDbName fieldStore ffrSourceField
                    , withDbName parentFieldStore ffrTargetField
                    )
            fixForeignNullable =
                all ((NotNullable /=) . isForeignNullable) foreignFieldNames
              where
                isForeignNullable fieldNameHS =
                    case getFieldDef fieldNameHS fieldStore of
                        Nothing ->
                            error "Field name not present in map"
                        Just a ->
                            isUnboundFieldNullable a

            fieldStore =
                mkFieldStore unboundEnt
            parentFieldStore =
                mkFieldStore parentDef
            findDBName store fieldNameHS =
                case getFieldDBName fieldNameHS store of
                    Nothing ->
                        error $ mconcat
                            [ "findDBName: failed to fix dbname for: "
                            , show fieldNameHS
                            ]
                    Just a->
                        a

    combinedFixFieldDef :: UnboundFieldDef -> Q Exp
    combinedFixFieldDef ufd@UnboundFieldDef{..} =
        [|
        FieldDef
            { fieldHaskell =
                unboundFieldNameHS
            , fieldDB =
                unboundFieldNameDB
            , fieldType =
                unboundFieldType
            , fieldSqlType =
                $(sqlTyp')
            , fieldAttrs =
                unboundFieldAttrs
            , fieldStrict =
                unboundFieldStrict
            , fieldReference =
                $(fieldRef')
            , fieldCascade =
                unboundFieldCascade
            , fieldComments =
                unboundFieldComments
            , fieldGenerated =
                unboundFieldGenerated
            , fieldIsImplicitIdColumn =
                False
            }
        |]
      where
        sqlTypeExp =
            getSqlType emEntities entityMap ufd
        FieldDef _x _ _ _ _ _ _ _ _ _ _ =
            error "need to update this record wildcard match"
        (fieldRef', sqlTyp') =
            case extractForeignRef entityMap ufd of
                Just targetTable ->
                    (lift (ForeignRef targetTable), liftSqlTypeExp (SqlTypeReference targetTable))
                Nothing ->
                    (lift NoReference, liftSqlTypeExp sqlTypeExp)

data FieldStore
    = FieldStore
    { fieldStoreMap :: M.Map FieldNameHS UnboundFieldDef
    , fieldStoreId :: Maybe FieldNameDB
    , fieldStoreEntity :: UnboundEntityDef
    }

mkFieldStore :: UnboundEntityDef -> FieldStore
mkFieldStore ued =
    FieldStore
        { fieldStoreEntity = ued
        , fieldStoreMap =
            M.fromList
                $ fmap (\ufd ->
                    ( unboundFieldNameHS ufd
                    , ufd
                    )
                )
                $ getUnboundFieldDefs
                $ ued
        , fieldStoreId =
            case unboundPrimarySpec ued of
                NaturalKey _ ->
                    Nothing
                SurrogateKey fd ->
                    Just $ unboundIdDBName fd
                DefaultKey n ->
                    Just n
        }

getFieldDBName :: FieldNameHS -> FieldStore -> Maybe FieldNameDB
getFieldDBName name fs
    | FieldNameHS "Id" == name =
        fieldStoreId fs
    | otherwise =
        unboundFieldNameDB <$> getFieldDef name fs

getFieldDef :: FieldNameHS -> FieldStore -> Maybe UnboundFieldDef
getFieldDef fieldNameHS fs =
    M.lookup fieldNameHS (fieldStoreMap fs)

extractForeignRef :: EntityMap -> UnboundFieldDef -> Maybe EntityNameHS
extractForeignRef entityMap fieldDef = do
    refName <- guessFieldReference fieldDef
    ent <- M.lookup refName entityMap
    pure $ entityHaskell $ unboundEntityDef ent

guessFieldReference :: UnboundFieldDef -> Maybe EntityNameHS
guessFieldReference = guessReference . unboundFieldType

guessReference :: FieldType -> Maybe EntityNameHS
guessReference ft =
    case ft of
        FTTypeCon Nothing (T.stripSuffix "Id" -> Just tableName) ->
            Just (EntityNameHS tableName)
        FTApp (FTTypeCon Nothing "Key") (FTTypeCon Nothing tableName) ->
            Just (EntityNameHS tableName)
        _ ->
            Nothing

mkDefaultKey
    :: MkPersistSettings
    -> FieldNameDB
    -> EntityNameHS
    -> FieldDef
mkDefaultKey mps  pk unboundHaskellName =
    let
        iid =
            mpsImplicitIdDef mps
    in
        maybe id addFieldAttr (FieldAttrDefault <$> iidDefault iid) $
        maybe id addFieldAttr (FieldAttrMaxlen <$> iidMaxLen iid) $
        mkAutoIdField' pk unboundHaskellName (iidFieldSqlType iid)

fixPrimarySpec
    :: MkPersistSettings
    -> UnboundEntityDef
    -> Q Exp
fixPrimarySpec mps unboundEnt= do
    case unboundPrimarySpec unboundEnt of
        DefaultKey pk ->
            lift $ EntityIdField $
                mkDefaultKey mps pk unboundHaskellName
        SurrogateKey uid -> do
            let
                entNameHS =
                    getUnboundEntityNameHS unboundEnt
                fieldTyp =
                    fromMaybe (mkKeyConType entNameHS) (unboundIdType uid)
            [|
                EntityIdField
                    FieldDef
                        { fieldHaskell =
                            FieldNameHS "Id"
                        , fieldDB =
                            $(lift $ getSqlNameOr (unboundIdDBName uid) (unboundIdAttrs uid))
                        , fieldType =
                            $(lift fieldTyp)
                        , fieldSqlType =
                            $( liftSqlTypeExp (SqlTypeExp  fieldTyp) )
                        , fieldStrict =
                            False
                        , fieldReference =
                            ForeignRef entNameHS
                        , fieldAttrs =
                            unboundIdAttrs uid
                        , fieldComments =
                            Nothing
                        , fieldCascade = unboundIdCascade uid
                        , fieldGenerated = Nothing
                        , fieldIsImplicitIdColumn = True
                        }

                |]
        NaturalKey ucd ->
            [| EntityIdNaturalKey $(bindCompositeDef unboundEnt ucd) |]
  where
    unboundHaskellName =
        getUnboundEntityNameHS unboundEnt

bindCompositeDef :: UnboundEntityDef -> UnboundCompositeDef -> Q Exp
bindCompositeDef ued ucd = do
    fieldDefs <-
       fmap ListE $ forM (unboundCompositeCols ucd) $ \col ->
           mkLookupEntityField ued col
    [|
        CompositeDef
            { compositeFields =
                NEL.fromList $(pure fieldDefs)
            , compositeAttrs =
                $(lift $ unboundCompositeAttrs ucd)
            }
        |]

getSqlType :: M.Map EntityNameHS a -> EntityMap -> UnboundFieldDef -> SqlTypeExp
getSqlType emEntities entityMap field =
    maybe
        (defaultSqlTypeExp emEntities entityMap field)
        (SqlType' . SqlOther)
        (listToMaybe $ mapMaybe attrSqlType $ unboundFieldAttrs field)

-- In the case of embedding, there won't be any datatype created yet.
-- We just use SqlString, as the data will be serialized to JSON.
defaultSqlTypeExp :: M.Map EntityNameHS a -> EntityMap -> UnboundFieldDef -> SqlTypeExp
defaultSqlTypeExp emEntities entityMap field =
    case mEmbedded emEntities ftype of
        Right _ ->
            SqlType' SqlString
        Left (Just (FTKeyCon ty)) ->
            SqlTypeExp (FTTypeCon Nothing ty)
        Left Nothing ->
            case extractForeignRef entityMap field of
                Just refName ->
                    case M.lookup refName entityMap of
                        Nothing ->
                            -- error $ mconcat
                            --     [ "Failed to find model: "
                            --     , show refName
                            --     , " in entity list: \n"
                            --     ]
                            --     <> (unlines $ map show $ M.keys $ entityMap)
                            -- going to assume that it's fine, will reify it out
                            -- right later anyway)
                            SqlTypeExp ftype
                        -- A ForeignRef is blindly set to an Int64 in setEmbedField
                        -- correct that now
                        Just _ ->
                            SqlTypeReference refName
                _ ->
                    case ftype of
                        -- In the case of lists, we always serialize to a string
                        -- value (via JSON).
                        --
                        -- Normally, this would be determined automatically by
                        -- SqlTypeExp. However, there's one corner case: if there's
                        -- a list of entity IDs, the datatype for the ID has not
                        -- yet been created, so the compiler will fail. This extra
                        -- clause works around this limitation.
                        FTList _ ->
                            SqlType' SqlString
                        _ ->
                            SqlTypeExp ftype
    where
        ftype = unboundFieldType field

attrSqlType :: FieldAttr -> Maybe Text
attrSqlType = \case
    FieldAttrSqltype x -> Just x
    _ -> Nothing

data SqlTypeExp
    = SqlTypeExp FieldType
    | SqlType' SqlType
    | SqlTypeReference EntityNameHS
    deriving Show

liftSqlTypeExp :: SqlTypeExp -> Q Exp
liftSqlTypeExp ste =
    case ste of
        SqlType' t ->
            lift t
        SqlTypeExp ftype -> do
            let
                typ = ftToType ftype
                mtyp = ConT ''Proxy `AppT` typ
                typedNothing = SigE (ConE 'Proxy) mtyp
            pure $ VarE 'sqlType `AppE` typedNothing
        SqlTypeReference entNameHs -> do
            let
                entNameId :: Name
                entNameId =
                    mkName $ T.unpack (unEntityNameHS entNameHs) <> "Id"

            [| sqlType (Proxy :: Proxy $(conT entNameId)) |]


type EmbedEntityMap = M.Map EntityNameHS ()

constructEmbedEntityMap :: [UnboundEntityDef] -> EmbedEntityMap
constructEmbedEntityMap =
    M.fromList . fmap
        (\ent ->
                ( entityHaskell (unboundEntityDef ent)
                -- , toEmbedEntityDef (unboundEntityDef ent)
                , ()
                )
        )

lookupEmbedEntity :: M.Map EntityNameHS a -> FieldDef -> Maybe EntityNameHS
lookupEmbedEntity allEntities field = do
    entName <- EntityNameHS <$> stripId (fieldType field)
    guard (M.member entName allEntities) -- check entity name exists in embed fmap
    pure entName

type EntityMap = M.Map EntityNameHS UnboundEntityDef

constructEntityMap :: [UnboundEntityDef] -> EntityMap
constructEntityMap =
    M.fromList . fmap (\ent -> (entityHaskell (unboundEntityDef ent), ent))

data FTTypeConDescr = FTKeyCon Text
    deriving Show

-- | Recurses through the 'FieldType'. Returns a 'Right' with the
-- 'EmbedEntityDef' if the 'FieldType' corresponds to an unqualified use of
-- a name and that name is present in the 'EmbedEntityMap' provided as
-- a first argument.
--
-- If the 'FieldType' represents a @Key something@, this returns a @'Left
-- ('Just' 'FTKeyCon')@.
--
-- If the 'FieldType' has a module qualified value, then it returns @'Left'
-- 'Nothing'@.
mEmbedded
    :: M.Map EntityNameHS a
    -> FieldType
    -> Either (Maybe FTTypeConDescr) EntityNameHS
mEmbedded _ (FTTypeCon Just{} _) =
    Left Nothing
mEmbedded ents (FTTypeCon Nothing (EntityNameHS -> name)) =
    maybe (Left Nothing) (\_ -> Right name) $ M.lookup name ents
mEmbedded _ (FTTypePromoted _) =
    Left Nothing
mEmbedded ents (FTList x) =
    mEmbedded ents x
mEmbedded _ (FTApp (FTTypeCon Nothing "Key") (FTTypeCon _ a)) =
    Left $ Just $ FTKeyCon $ a <> "Id"
mEmbedded _ (FTApp _ _) =
    Left Nothing

setEmbedField :: EntityNameHS -> M.Map EntityNameHS a -> FieldDef -> FieldDef
setEmbedField entName allEntities field =
    case fieldReference field of
      NoReference ->
          setFieldReference ref field
      _ ->
          field
  where
    ref =
        case mEmbedded allEntities (fieldType field) of
            Left _ -> fromMaybe NoReference $ do
                refEntName <- lookupEmbedEntity allEntities field
                pure $ ForeignRef refEntName
            Right em ->
                if em /= entName
                     then EmbedRef em
                else if maybeNullable (unbindFieldDef field)
                     then SelfReference
                else case fieldType field of
                         FTList _ -> SelfReference
                         _ -> error $ unpack $ unEntityNameHS entName <> ": a self reference must be a Maybe or List"

setFieldReference :: ReferenceDef -> FieldDef -> FieldDef
setFieldReference ref field = field { fieldReference = ref }

-- | Create data types and appropriate 'PersistEntity' instances for the given
-- 'EntityDef's. Works well with the persist quasi-quoter.
mkPersist
    :: MkPersistSettings
    -> [UnboundEntityDef]
    -> Q [Dec]
mkPersist mps = mkPersistWith mps []

-- | Like '
--
-- @since 2.13.0.0
mkPersistWith
    :: MkPersistSettings
    -> [EntityDef]
    -> [UnboundEntityDef]
    -> Q [Dec]
mkPersistWith mps preexistingEntities ents' = do
    let
        (embedEntityMap, predefs) =
            preprocessUnboundDefs preexistingEntities ents'
        allEnts =
            embedEntityDefs preexistingEntities
            $ fmap (setDefaultIdFields mps)
            $ predefs
        entityMap =
            constructEntityMap allEnts
        preexistingSet =
            Set.fromList $ map getEntityHaskellName preexistingEntities
        newEnts =
            filter
                (\e -> getUnboundEntityNameHS e `Set.notMember` preexistingSet)
                allEnts
    ents <- filterM shouldGenerateCode newEnts
    requireExtensions
        [ [TypeFamilies], [GADTs, ExistentialQuantification]
        , [DerivingStrategies], [GeneralizedNewtypeDeriving], [StandaloneDeriving]
        , [UndecidableInstances], [DataKinds], [FlexibleInstances]
        ]
    persistFieldDecs <- fmap mconcat $ mapM (persistFieldFromEntity mps) ents
    entityDecs <- fmap mconcat $ mapM (mkEntity embedEntityMap entityMap mps) ents
    jsonDecs <- fmap mconcat $ mapM (mkJSON mps) ents
    uniqueKeyInstances <- fmap mconcat $ mapM (mkUniqueKeyInstances mps) ents
    symbolToFieldInstances <- fmap mconcat $ mapM (mkSymbolToFieldInstances mps entityMap) ents
    return $ mconcat
        [ persistFieldDecs
        , entityDecs
        , jsonDecs
        , uniqueKeyInstances
        , symbolToFieldInstances
        ]

-- we can't just use 'isInstance' because TH throws an error
shouldGenerateCode :: UnboundEntityDef -> Q Bool
shouldGenerateCode ed = do
    mtyp <- lookupTypeName entityName
    case mtyp of
        Nothing -> do
            pure True
        Just typeName -> do
            instanceExists <- isInstance ''PersistEntity [ConT typeName]
            pure (not instanceExists)
  where
    entityName =
        T.unpack . unEntityNameHS . getEntityHaskellName . unboundEntityDef $ ed

overEntityDef :: (EntityDef -> EntityDef) -> UnboundEntityDef -> UnboundEntityDef
overEntityDef f ued = ued { unboundEntityDef = f (unboundEntityDef ued) }

setDefaultIdFields :: MkPersistSettings -> UnboundEntityDef -> UnboundEntityDef
setDefaultIdFields mps ued
    | defaultIdType ued =
        overEntityDef
            (setEntityIdDef (setToMpsDefault (mpsImplicitIdDef mps) (getEntityId ed)))
            ued
    | otherwise =
        ued
  where
    ed =
        unboundEntityDef ued
    setToMpsDefault :: ImplicitIdDef -> EntityIdDef -> EntityIdDef
    setToMpsDefault iid (EntityIdField fd) =
        EntityIdField fd
            { fieldType =
                iidFieldType iid (getEntityHaskellName ed)
            , fieldSqlType =
                iidFieldSqlType iid
            , fieldAttrs =
                let
                    def =
                        toList (FieldAttrDefault <$> iidDefault iid)
                    maxlen =
                        toList (FieldAttrMaxlen <$> iidMaxLen iid)
                 in
                    def <> maxlen <> fieldAttrs fd
            , fieldIsImplicitIdColumn =
                True
            }
    setToMpsDefault _ x =
        x

-- | Implement special preprocessing on EntityDef as necessary for 'mkPersist'.
-- For example, strip out any fields marked as MigrationOnly.
--
-- This should be called when performing Haskell codegen, but the 'EntityDef'
-- *should* keep all of the fields present when defining 'entityDef'. This is
-- necessary so that migrations know to keep these columns around, or to delete
-- them, as appropriate.
fixEntityDef :: UnboundEntityDef -> UnboundEntityDef
fixEntityDef ued =
    ued
        { unboundEntityFields =
            filter isHaskellUnboundField (unboundEntityFields ued)
        }

-- | Settings to be passed to the 'mkPersist' function.
data MkPersistSettings = MkPersistSettings
    { mpsBackend :: Type
    -- ^ Which database backend we\'re using. This type is used for the
    -- 'PersistEntityBackend' associated type in the entities that are
    -- generated.
    --
    -- If the 'mpsGeneric' value is set to 'True', then this type is used for
    -- the non-Generic type alias. The data and type will be named:
    --
    -- @
    -- data ModelGeneric backend = Model { ... }
    -- @
    --
    -- And, for convenience's sake, we provide a type alias:
    --
    -- @
    -- type Model = ModelGeneric $(the type you give here)
    -- @
    , mpsGeneric :: Bool
    -- ^ Create generic types that can be used with multiple backends. Good for
    -- reusable code, but makes error messages harder to understand. Default:
    -- False.
    , mpsPrefixFields :: Bool
    -- ^ Prefix field names with the model name. Default: True.
    --
    -- Note: this field is deprecated. Use the mpsFieldLabelModifier  and
    -- 'mpsConstraintLabelModifier' instead.
    , mpsFieldLabelModifier :: Text -> Text -> Text
    -- ^ Customise the field accessors and lens names using the entity and field
    -- name.  Both arguments are upper cased.
    --
    -- Default: appends entity and field.
    --
    -- Note: this setting is ignored if mpsPrefixFields is set to False.
    --
    -- @since 2.11.0.0
    , mpsConstraintLabelModifier :: Text -> Text -> Text
    -- ^ Customise the Constraint names using the entity and field name. The
    -- result should be a valid haskell type (start with an upper cased letter).
    --
    -- Default: appends entity and field
    --
    -- Note: this setting is ignored if mpsPrefixFields is set to False.
    --
    -- @since 2.11.0.0
    , mpsEntityJSON :: Maybe EntityJSON
    -- ^ Generate @ToJSON@/@FromJSON@ instances for each model types. If it's
    -- @Nothing@, no instances will be generated. Default:
    --
    -- @
    --  Just 'EntityJSON'
    --      { 'entityToJSON' = 'entityIdToJSON
    --      , 'entityFromJSON' = 'entityIdFromJSON
    --      }
    -- @
    , mpsGenerateLenses :: Bool
    -- ^ Instead of generating normal field accessors, generator lens-style
    -- accessors.
    --
    -- Default: False
    --
    -- @since 1.3.1
    , mpsDeriveInstances :: [Name]
    -- ^ Automatically derive these typeclass instances for all record and key
    -- types.
    --
    -- Default: []
    --
    -- @since 2.8.1
    , mpsImplicitIdDef :: ImplicitIdDef
    -- ^ TODO: document
    --
    -- @since 2.13.0.0
    }

{-# DEPRECATED mpsGeneric "The mpsGeneric function adds a considerable amount of overhead and complexity to the library without bringing significant benefit. We would like to remove it. If you require this feature, please comment on the linked GitHub issue, and we'll either keep it around, or we can figure out a nicer way to solve your problem.\n\n Github: https://github.com/yesodweb/persistent/issues/1204" #-}

-- |  Set the 'ImplicitIdDef' in the given 'MkPersistSettings'. The default
-- value is 'autoIncrementingInteger'.
--
-- @since 2.13.0.0
setImplicitIdDef :: ImplicitIdDef -> MkPersistSettings -> MkPersistSettings
setImplicitIdDef iid mps =
    mps { mpsImplicitIdDef = iid }

getImplicitIdType :: MkPersistSettings -> Type
getImplicitIdType = do
    idDef <- mpsImplicitIdDef
    isGeneric <- mpsGeneric
    backendTy <- mpsBackend
    pure $ iidType idDef isGeneric backendTy

data EntityJSON = EntityJSON
    { entityToJSON :: Name
    -- ^ Name of the @toJSON@ implementation for @Entity a@.
    , entityFromJSON :: Name
    -- ^ Name of the @fromJSON@ implementation for @Entity a@.
    }

-- | Create an @MkPersistSettings@ with default values.
mkPersistSettings
    :: Type -- ^ Value for 'mpsBackend'
    -> MkPersistSettings
mkPersistSettings backend = MkPersistSettings
    { mpsBackend = backend
    , mpsGeneric = False
    , mpsPrefixFields = True
    , mpsFieldLabelModifier = (++)
    , mpsConstraintLabelModifier = (++)
    , mpsEntityJSON = Just EntityJSON
        { entityToJSON = 'entityIdToJSON
        , entityFromJSON = 'entityIdFromJSON
        }
    , mpsGenerateLenses = False
    , mpsDeriveInstances = []
    , mpsImplicitIdDef =
        autoIncrementingInteger
    }

-- | Use the 'SqlPersist' backend.
sqlSettings :: MkPersistSettings
sqlSettings = mkPersistSettings $ ConT ''SqlBackend

lowerFirst :: Text -> Text
lowerFirst t =
    case uncons t of
        Just (a, b) -> cons (toLower a) b
        Nothing -> t

upperFirst :: Text -> Text
upperFirst t =
    case uncons t of
        Just (a, b) -> cons (toUpper a) b
        Nothing -> t

dataTypeDec :: MkPersistSettings -> EntityMap -> UnboundEntityDef -> Q Dec
dataTypeDec mps entityMap entDef = do
    let
        names =
            mkEntityDefDeriveNames mps entDef

    let (stocks, anyclasses) = partitionEithers (fmap stratFor names)
    let stockDerives = do
            guard (not (null stocks))
            pure (DerivClause (Just StockStrategy) (fmap ConT stocks))
        anyclassDerives = do
            guard (not (null anyclasses))
            pure (DerivClause (Just AnyclassStrategy) (fmap ConT anyclasses))
    unless (null anyclassDerives) $ do
        requireExtensions [[DeriveAnyClass]]
    pure $ DataD [] nameFinal paramsFinal
                Nothing
                constrs
                (stockDerives <> anyclassDerives)
  where
    stratFor n =
        if n `elem` stockClasses then
            Left n
        else
            Right n

    stockClasses =
        Set.fromList (fmap mkName
        [ "Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Generic", "Data", "Typeable"
        ] <> [''Eq, ''Ord, ''Show, ''Read, ''Bounded, ''Enum, ''Ix, ''Generic, ''Data, ''Typeable
        ]
        )

    (nameFinal, paramsFinal)
        | mpsGeneric mps =
            ( mkEntityDefGenericName entDef
            , [ mkPlainTV backendName
              ]
            )

        | otherwise =
            (mkEntityDefName entDef, [])

    cols :: [VarBangType]
    cols = do
        fieldDef <- getUnboundFieldDefs entDef
        let
            recordName =
                fieldDefToRecordName mps entDef fieldDef
            strictness =
                if unboundFieldStrict fieldDef
                then isStrict
                else notStrict
            fieldIdType =
                maybeIdType mps entityMap fieldDef Nothing Nothing
        pure (recordName, strictness, fieldIdType)

    constrs
        | unboundEntitySum entDef = fmap sumCon $ getUnboundFieldDefs entDef
        | otherwise = [RecC (mkEntityDefName entDef) cols]

    sumCon fieldDef = NormalC
        (sumConstrName mps entDef fieldDef)
        [(notStrict, maybeIdType mps entityMap fieldDef Nothing Nothing)]

uniqueTypeDec :: MkPersistSettings -> EntityMap -> UnboundEntityDef -> Dec
uniqueTypeDec mps entityMap entDef =
    DataInstD
        []
#if MIN_VERSION_template_haskell(2,15,0)
        Nothing
        (AppT (ConT ''Unique) (genericDataType mps (getUnboundEntityNameHS entDef) backendT))
#else
        ''Unique
        [genericDataType mps (getUnboundEntityNameHS entDef) backendT]
#endif
        Nothing
        (fmap (mkUnique mps entityMap entDef) $ entityUniques (unboundEntityDef entDef))
        []

mkUnique :: MkPersistSettings -> EntityMap -> UnboundEntityDef -> UniqueDef -> Con
mkUnique mps entityMap entDef (UniqueDef constr _ fields attrs) =
    NormalC (mkConstraintName constr) $ toList types
  where
    types =
      fmap (go . flip lookup3 (getUnboundFieldDefs entDef) . unFieldNameHS . fst) fields

    force = "!force" `elem` attrs

    go :: (UnboundFieldDef, IsNullable) -> (Strict, Type)
    go (_, Nullable _) | not force = error nullErrMsg
    go (fd, y) = (notStrict, maybeIdType mps entityMap fd Nothing (Just y))

    lookup3 :: Text -> [UnboundFieldDef] -> (UnboundFieldDef, IsNullable)
    lookup3 s [] =
        error $ unpack $ "Column not found: " ++ s ++ " in unique " ++ unConstraintNameHS constr
    lookup3 x (fd:rest)
        | x == unFieldNameHS (unboundFieldNameHS fd) =
            (fd, isUnboundFieldNullable fd)
        | otherwise =
            lookup3 x rest

    nullErrMsg =
      mconcat [ "Error:  By default we disallow NULLables in an uniqueness "
              , "constraint.  The semantics of how NULL interacts with those "
              , "constraints is non-trivial:  two NULL values are not "
              , "considered equal for the purposes of an uniqueness "
              , "constraint.  If you understand this feature, it is possible "
              , "to use it your advantage.    *** Use a \"!force\" attribute "
              , "on the end of the line that defines your uniqueness "
              , "constraint in order to disable this check. ***" ]

-- | This function renders a Template Haskell 'Type' for an 'UnboundFieldDef'.
-- It takes care to respect the 'mpsGeneric' setting to render an Id faithfully,
-- and it also ensures that the generated Haskell type is 'Maybe' if the
-- database column has that attribute.
--
-- For a database schema with @'mpsGeneric' = False@, this is simple - it uses
-- the @ModelNameId@ type directly. This resolves just fine.
--
-- If 'mpsGeneric' is @True@, then we have to do something a bit more
-- complicated. We can't refer to a @ModelNameId@ directly, because that @Id@
-- alias hides the backend type variable. Instead, we need to refer to:
--
-- > Key (ModelNameGeneric backend)
--
-- This means that the client code will need both the term @ModelNameId@ in
-- scope, as well as the @ModelNameGeneric@ constructor, despite the fact that
-- the @ModelNameId@ is the only term explicitly used (and imported).
--
-- However, we're not guaranteed to have @ModelName@ in scope - we've only
-- referenced @ModelNameId@ in code, and so code generation *should* work even
-- without this. Consider an explicit-style import:
--
-- @
-- import Model.Foo (FooId)
--
-- mkPersistWith sqlSettings $(discoverEntities) [persistLowerCase|
--   Bar
--     foo FooId
-- |]
-- @
--
-- This looks like it ought to work, but it would fail with @mpsGeneric@ being
-- enabled. One hacky work-around is to perform a @'lookupTypeName' :: String ->
-- Q (Maybe Name)@ on the @"ModelNameId"@ type string. If the @Id@ is
-- a reference in the 'EntityMap' and @lookupTypeName@ returns @'Just' name@,
-- then that 'Name' contains the fully qualified information needed to use the
-- 'Name' without importing it at the client-site. Then we can perform a bit of
-- surgery on the 'Name' to strip the @Id@ suffix, turn it into a 'Type', and
-- apply the 'Key' constructor.
maybeIdType
    :: MkPersistSettings
    -> EntityMap
    -> UnboundFieldDef
    -> Maybe Name -- ^ backend
    -> Maybe IsNullable
    -> Type
maybeIdType mps entityMap fieldDef mbackend mnull =
    maybeTyp mayNullable idType
  where
    mayNullable =
        case mnull of
            Just (Nullable ByMaybeAttr) ->
                True
            _ ->
                maybeNullable fieldDef
    idType =
        fromMaybe (ftToType $ unboundFieldType fieldDef) $ do
            typ <- extractForeignRef entityMap fieldDef
            guard ((mpsGeneric mps))
            pure $
                ConT ''Key
                `AppT` genericDataType mps typ (VarT $ fromMaybe backendName mbackend)

    -- TODO: if we keep mpsGeneric, this needs to check 'mpsGeneric' and then
    -- append Generic to the model name, probably
    _removeIdFromTypeSuffix :: Name -> Type
    _removeIdFromTypeSuffix oldName@(Name (OccName nm) nameFlavor) =
        case stripSuffix "Id" (T.pack nm) of
            Nothing ->
                ConT oldName
            Just name ->
                ConT ''Key
                `AppT` do
                    ConT $ Name (OccName (T.unpack name)) nameFlavor

    -- | TODO: if we keep mpsGeneric, let's incorporate this behavior here, so
    -- end users don't need to import the constructor type as well as the id type
    --
    -- Returns 'Nothing' if the given text does not appear to be a table reference.
    -- In that case, do the usual thing for generating a type name.
    --
    -- Returns a @Just typ@ if the text appears to be a model name, and if the
    -- @ModelId@ type is in scope. The 'Type' is a fully qualified reference to
    -- @'Key' ModelName@ such that end users won't have to import it directly.
    _lookupReferencedTable :: EntityMap -> Text -> Q (Maybe Type)
    _lookupReferencedTable em fieldTypeText = do
        let
            mmodelIdString = do
                fieldTypeNoId <- stripSuffix "Id" fieldTypeText
                _ <- M.lookup (EntityNameHS fieldTypeNoId) em
                pure (T.unpack fieldTypeText)
        case mmodelIdString of
            Nothing ->
                pure Nothing
            Just modelIdString -> do
                mIdName <- lookupTypeName modelIdString
                pure $ fmap _removeIdFromTypeSuffix mIdName

    _fieldNameEndsWithId :: UnboundFieldDef -> Maybe String
    _fieldNameEndsWithId ufd = go (unboundFieldType ufd)
      where
        go = \case
            FTTypeCon mmodule name -> do
                a <- stripSuffix "Id" name
                pure $
                    T.unpack $ mconcat
                        [ case mmodule of
                            Nothing ->
                                ""
                            Just m ->
                                mconcat [m, "."]
                        ,  a
                        , "Id"
                        ]
            _ ->
                Nothing

backendDataType :: MkPersistSettings -> Type
backendDataType mps
    | mpsGeneric mps = backendT
    | otherwise = mpsBackend mps

-- | TODO:
--
-- if we keep mpsGeneric
-- then
--      let's make this fully qualify the generic name
-- else
--      let's delete it
genericDataType
    :: MkPersistSettings
    -> EntityNameHS
    -> Type -- ^ backend
    -> Type
genericDataType mps name backend
    | mpsGeneric mps =
        ConT (mkEntityNameHSGenericName name) `AppT` backend
    | otherwise =
        ConT $ mkEntityNameHSName name

degen :: [Clause] -> [Clause]
degen [] =
    let err = VarE 'error `AppE` LitE (StringL
                "Degenerate case, should never happen")
     in [normalClause [WildP] err]
degen x = x

-- needs:
--
-- * isEntitySum ed
--     * field accesor
-- * getEntityFields ed
--     * used in goSum, or sumConstrName
-- * mkEntityDefName ed
--     * uses entityHaskell
-- * sumConstrName ed fieldDef
--     * only needs entity name and field name
--
-- data MkToPersistFields = MkToPersistFields
--     { isEntitySum :: Bool
--     , entityHaskell :: HaskellNameHS
--     , entityFieldNames :: [FieldNameHS]
--     }
mkToPersistFields :: MkPersistSettings -> UnboundEntityDef -> Q Dec
mkToPersistFields mps ed = do
    let isSum = unboundEntitySum ed
        fields = getUnboundFieldDefs ed
    clauses <-
        if isSum
            then sequence $ zipWith goSum fields [1..]
            else fmap return go
    return $ FunD 'toPersistFields clauses
  where
    go :: Q Clause
    go = do
        xs <- sequence $ replicate fieldCount $ newName "x"
        let name = mkEntityDefName ed
            pat = ConP name $ fmap VarP xs
        sp <- [|SomePersistField|]
        let bod = ListE $ fmap (AppE sp . VarE) xs
        return $ normalClause [pat] bod

    fieldCount = length (getUnboundFieldDefs ed)

    goSum :: UnboundFieldDef -> Int -> Q Clause
    goSum fieldDef idx = do
        let name = sumConstrName mps ed fieldDef
        enull <- [|SomePersistField PersistNull|]
        let beforeCount = idx - 1
            afterCount = fieldCount - idx
            before = replicate beforeCount enull
            after = replicate afterCount enull
        x <- newName "x"
        sp <- [|SomePersistField|]
        let body = ListE $ mconcat
                [ before
                , [sp `AppE` VarE x]
                , after
                ]
        return $ normalClause [ConP name [VarP x]] body

mkToFieldNames :: [UniqueDef] -> Q Dec
mkToFieldNames pairs = do
    pairs' <- mapM go pairs
    return $ FunD 'persistUniqueToFieldNames $ degen pairs'
  where
    go (UniqueDef constr _ names _) = do
        names' <- lift names
        return $
            normalClause
                [RecP (mkConstraintName constr) []]
                names'

mkUniqueToValues :: [UniqueDef] -> Q Dec
mkUniqueToValues pairs = do
    pairs' <- mapM go pairs
    return $ FunD 'persistUniqueToValues $ degen pairs'
  where
    go :: UniqueDef -> Q Clause
    go (UniqueDef constr _ names _) = do
        xs <- mapM (const $ newName "x") names
        let pat = ConP (mkConstraintName constr) $ fmap VarP $ toList xs
        tpv <- [|toPersistValue|]
        let bod = ListE $ fmap (AppE tpv . VarE) $ toList xs
        return $ normalClause [pat] bod

isNotNull :: PersistValue -> Bool
isNotNull PersistNull = False
isNotNull _ = True

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right r) = Right r
mapLeft f (Left l)  = Left (f l)

-- needs:
--
-- * getEntityFields
--     * sumConstrName on field
-- * fromValues
-- * entityHaskell
-- * sumConstrName
-- * entityDefConE
--
--
mkFromPersistValues :: MkPersistSettings -> UnboundEntityDef -> Q [Clause]
mkFromPersistValues mps entDef
    | unboundEntitySum entDef = do
        nothing <- [|Left ("Invalid fromPersistValues input: sum type with all nulls. Entity: " `mappend` entName)|]
        clauses <- mkClauses [] $ getUnboundFieldDefs entDef
        return $ clauses `mappend` [normalClause [WildP] nothing]
    | otherwise =
        fromValues entDef "fromPersistValues" entE
        $ fmap unboundFieldNameHS
        $ filter isHaskellUnboundField
        $ getUnboundFieldDefs entDef
  where
    entName = unEntityNameHS $ getUnboundEntityNameHS entDef
    mkClauses _ [] = return []
    mkClauses before (field:after) = do
        x <- newName "x"
        let null' = ConP 'PersistNull []
            pat = ListP $ mconcat
                [ fmap (const null') before
                , [VarP x]
                , fmap (const null') after
                ]
            constr = ConE $ sumConstrName mps entDef field
        fs <- [|fromPersistValue $(return $ VarE x)|]
        let guard' = NormalG $ VarE 'isNotNull `AppE` VarE x
        let clause = Clause [pat] (GuardedB [(guard', InfixE (Just constr) fmapE (Just fs))]) []
        clauses <- mkClauses (field : before) after
        return $ clause : clauses
    entE = entityDefConE entDef


type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lensPTH :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lensPTH sa sbt afb s = fmap (sbt s) (afb $ sa s)

fmapE :: Exp
fmapE = VarE 'fmap

unboundEntitySum :: UnboundEntityDef -> Bool
unboundEntitySum = entitySum . unboundEntityDef

mkLensClauses :: MkPersistSettings -> UnboundEntityDef -> Q [Clause]
mkLensClauses mps entDef = do
    lens' <- [|lensPTH|]
    getId <- [|entityKey|]
    setId <- [|\(Entity _ value) key -> Entity key value|]
    getVal <- [|entityVal|]
    dot <- [|(.)|]
    keyVar <- newName "key"
    valName <- newName "value"
    xName <- newName "x"
    let idClause = normalClause
            [ConP (keyIdName entDef) []]
            (lens' `AppE` getId `AppE` setId)
    return $ idClause : if unboundEntitySum entDef
        then fmap (toSumClause lens' keyVar valName xName) (getUnboundFieldDefs entDef)
        else fmap (toClause lens' getVal dot keyVar valName xName) (getUnboundFieldDefs entDef)
  where
    toClause lens' getVal dot keyVar valName xName fieldDef = normalClause
        [ConP (filterConName mps entDef fieldDef) []]
        (lens' `AppE` getter `AppE` setter)
      where
        fieldName = fieldDefToRecordName mps entDef fieldDef
        getter = InfixE (Just $ VarE fieldName) dot (Just getVal)
        setter = LamE
            [ ConP 'Entity [VarP keyVar, VarP valName]
            , VarP xName
            ]
            $ ConE 'Entity `AppE` VarE keyVar `AppE` RecUpdE
                (VarE valName)
                [(fieldName, VarE xName)]

    toSumClause lens' keyVar valName xName fieldDef = normalClause
        [ConP (filterConName mps entDef fieldDef) []]
        (lens' `AppE` getter `AppE` setter)
      where
        emptyMatch = Match WildP (NormalB $ VarE 'error `AppE` LitE (StringL "Tried to use fieldLens on a Sum type")) []
        getter = LamE
            [ ConP 'Entity [WildP, VarP valName]
            ] $ CaseE (VarE valName)
            $ Match (ConP (sumConstrName mps entDef fieldDef) [VarP xName]) (NormalB $ VarE xName) []

            -- FIXME It would be nice if the types expressed that the Field is
            -- a sum type and therefore could result in Maybe.
            : if length (getUnboundFieldDefs entDef) > 1 then [emptyMatch] else []
        setter = LamE
            [ ConP 'Entity [VarP keyVar, WildP]
            , VarP xName
            ]
            $ ConE 'Entity `AppE` VarE keyVar `AppE` (ConE (sumConstrName mps entDef fieldDef) `AppE` VarE xName)

-- | declare the key type and associated instances
-- @'PathPiece'@, @'ToHttpApiData'@ and @'FromHttpApiData'@ instances are only generated for a Key with one field
mkKeyTypeDec :: MkPersistSettings -> UnboundEntityDef -> Q (Dec, [Dec])
mkKeyTypeDec mps entDef = do
    (instDecs, i) <-
      if mpsGeneric mps
        then if not useNewtype
               then do pfDec <- pfInstD
                       return (pfDec, supplement [''Generic])
               else do gi <- genericNewtypeInstances
                       return (gi, supplement [])
        else if not useNewtype
               then do pfDec <- pfInstD
                       return (pfDec, supplement [''Show, ''Read, ''Eq, ''Ord, ''Generic])
                else do
                    let allInstances = supplement [''Show, ''Read, ''Eq, ''Ord, ''PathPiece, ''ToHttpApiData, ''FromHttpApiData, ''PersistField, ''PersistFieldSql, ''ToJSON, ''FromJSON]
                    if customKeyType
                      then return ([], allInstances)
                      else do
                        bi <- backendKeyI
                        return (bi, allInstances)

    requirePersistentExtensions

    -- Always use StockStrategy for Show/Read. This means e.g. (FooKey 1) shows as ("FooKey 1"), rather than just "1"
    -- This is much better for debugging/logging purposes
    -- cf. https://github.com/yesodweb/persistent/issues/1104
    let alwaysStockStrategyTypeclasses = [''Show, ''Read]
        deriveClauses = fmap (\typeclass ->
            if (not useNewtype || typeclass `elem` alwaysStockStrategyTypeclasses)
                then DerivClause (Just StockStrategy) [(ConT typeclass)]
                else DerivClause (Just NewtypeStrategy) [(ConT typeclass)]
            ) i

#if MIN_VERSION_template_haskell(2,15,0)
    let kd = if useNewtype
               then NewtypeInstD [] Nothing (AppT (ConT k) recordType) Nothing dec deriveClauses
               else DataInstD    [] Nothing (AppT (ConT k) recordType) Nothing [dec] deriveClauses
#else
    let kd = if useNewtype
               then NewtypeInstD [] k [recordType] Nothing dec deriveClauses
               else DataInstD    [] k [recordType] Nothing [dec] deriveClauses
#endif
    return (kd, instDecs)
  where
    keyConE = keyConExp entDef
    unKeyE = unKeyExp entDef
    dec = RecC (keyConName entDef) (keyFields mps entDef)
    k = ''Key
    recordType =
        genericDataType mps (getUnboundEntityNameHS entDef) backendT
    pfInstD = -- FIXME: generate a PersistMap instead of PersistList
      [d|instance PersistField (Key $(pure recordType)) where
            toPersistValue = PersistList . keyToValues
            fromPersistValue (PersistList l) = keyFromValues l
            fromPersistValue got = error $ "fromPersistValue: expected PersistList, got: " `mappend` show got
         instance PersistFieldSql (Key $(pure recordType)) where
            sqlType _ = SqlString
         instance ToJSON (Key $(pure recordType))
         instance FromJSON (Key $(pure recordType))
      |]

    backendKeyGenericI =
        [d| instance PersistStore $(pure backendT) =>
              ToBackendKey $(pure backendT) $(pure recordType) where
                toBackendKey   = $(return unKeyE)
                fromBackendKey = $(return keyConE)
        |]
    backendKeyI = let bdt = backendDataType mps in
        [d| instance ToBackendKey $(pure bdt) $(pure recordType) where
                toBackendKey   = $(return unKeyE)
                fromBackendKey = $(return keyConE)
        |]

    genericNewtypeInstances = do
        requirePersistentExtensions

        alwaysInstances <-
          -- See the "Always use StockStrategy" comment above, on why Show/Read use "stock" here
          [d|deriving stock instance Show (BackendKey $(pure backendT)) => Show (Key $(pure recordType))
             deriving stock instance Read (BackendKey $(pure backendT)) => Read (Key $(pure recordType))
             deriving newtype instance Eq (BackendKey $(pure backendT)) => Eq (Key $(pure recordType))
             deriving newtype instance Ord (BackendKey $(pure backendT)) => Ord (Key $(pure recordType))
             deriving newtype instance ToHttpApiData (BackendKey $(pure backendT)) => ToHttpApiData (Key $(pure recordType))
             deriving newtype instance FromHttpApiData (BackendKey $(pure backendT)) => FromHttpApiData(Key $(pure recordType))
             deriving newtype instance PathPiece (BackendKey $(pure backendT)) => PathPiece (Key $(pure recordType))
             deriving newtype instance PersistField (BackendKey $(pure backendT)) => PersistField (Key $(pure recordType))
             deriving newtype instance PersistFieldSql (BackendKey $(pure backendT)) => PersistFieldSql (Key $(pure recordType))
             deriving newtype instance ToJSON (BackendKey $(pure backendT)) => ToJSON (Key $(pure recordType))
             deriving newtype instance FromJSON (BackendKey $(pure backendT)) => FromJSON (Key $(pure recordType))
              |]

        mappend alwaysInstances <$>
            if customKeyType
            then pure []
            else backendKeyGenericI

    useNewtype = pkNewtype mps entDef
    customKeyType =
        or
            [ not (defaultIdType entDef)
            , not useNewtype
            , isJust (entityPrimary (unboundEntityDef entDef))
            , not isBackendKey
            ]

    isBackendKey =
        case getImplicitIdType mps of
            ConT bk `AppT` _
                | bk == ''BackendKey ->
                    True
            _ ->
                False

    supplement :: [Name] -> [Name]
    supplement names = names <> (filter (`notElem` names) $ mpsDeriveInstances mps)

-- | Returns 'True' if the key definition has less than 2 fields.
--
-- @since 2.11.0.0
pkNewtype :: MkPersistSettings -> UnboundEntityDef -> Bool
pkNewtype mps entDef = length (keyFields mps entDef) < 2

-- | Kind of a nasty hack. Checks to see if the 'fieldType' matches what the
-- QuasiQuoter produces for an implicit ID and
defaultIdType :: UnboundEntityDef -> Bool
defaultIdType entDef =
    case unboundPrimarySpec entDef of
        DefaultKey _ ->
            True
        _ ->
            False

keyFields :: MkPersistSettings -> UnboundEntityDef -> [(Name, Strict, Type)]
keyFields mps entDef =
    case unboundPrimarySpec entDef of
        NaturalKey ucd ->
            fmap naturalKeyVar (unboundCompositeCols ucd)
        DefaultKey _ ->
            pure . idKeyVar $ getImplicitIdType mps
        SurrogateKey k ->
            pure . idKeyVar $ case unboundIdType k of
                Nothing ->
                    getImplicitIdType mps
                Just ty ->
                    ftToType ty
  where
    unboundFieldDefs =
        getUnboundFieldDefs entDef
    naturalKeyVar fieldName =
        case findField fieldName unboundFieldDefs of
            Nothing ->
                error "column not defined on entity"
            Just unboundFieldDef ->
                ( keyFieldName mps entDef (unboundFieldNameHS unboundFieldDef)
                , notStrict
                , ftToType $ unboundFieldType unboundFieldDef
                )

    idKeyVar ft =
        ( unKeyName entDef
        , notStrict
        , ft
        )

findField :: FieldNameHS -> [UnboundFieldDef] -> Maybe UnboundFieldDef
findField fieldName =
    List.find ((fieldName ==) . unboundFieldNameHS)

mkKeyToValues :: MkPersistSettings -> UnboundEntityDef -> Q Dec
mkKeyToValues mps entDef = do
    recordName <- newName "record"
    FunD 'keyToValues . pure <$>
        case unboundPrimarySpec entDef of
            NaturalKey ucd -> do
                normalClause [VarP recordName] <$>
                    toValuesPrimary recordName ucd
            _ -> do
                normalClause [] <$>
                    [|(:[]) . toPersistValue . $(pure $ unKeyExp entDef)|]
  where
    toValuesPrimary recName ucd =
        ListE <$> mapM (f recName) (unboundCompositeCols ucd)
    f recName fieldNameHS =
        [|
        toPersistValue ($(varE $ keyFieldName mps entDef fieldNameHS) $(varE recName))
        |]

normalClause :: [Pat] -> Exp -> Clause
normalClause p e = Clause p (NormalB e) []

-- needs:
--
-- * entityPrimary
-- * keyConExp entDef
mkKeyFromValues :: MkPersistSettings -> UnboundEntityDef -> Q Dec
mkKeyFromValues _mps entDef =
    FunD 'keyFromValues <$>
        case unboundPrimarySpec entDef of
            NaturalKey ucd ->
                fromValues entDef "keyFromValues" keyConE (unboundCompositeCols ucd)
            _ -> do
                e <- [|fmap $(return keyConE) . fromPersistValue . headNote|]
                return [normalClause [] e]
  where
    keyConE = keyConExp entDef

headNote :: [PersistValue] -> PersistValue
headNote = \case
  [x] -> x
  xs -> error $ "mkKeyFromValues: expected a list of one element, got: " `mappend` show xs

-- needs from entity:
--
-- * entityText entDef
--     * entityHaskell
-- * entityDB entDef
--
-- needs from fields:
--
-- * mkPersistValue
--     *  fieldHaskell
--
-- data MkFromValues = MkFromValues
--     { entityHaskell :: EntityNameHS
--     , entityDB :: EntitynameDB
--     , entityFieldNames :: [FieldNameHS]
--     }
fromValues :: UnboundEntityDef -> Text -> Exp -> [FieldNameHS] -> Q [Clause]
fromValues entDef funName constructExpr fields = do
    x <- newName "x"
    let
        funMsg =
            mconcat
                [ entityText entDef
                , ": "
                , funName
                , " failed on: "
                ]
    patternMatchFailure <-
        [|Left $ mappend funMsg (pack $ show $(return $ VarE x))|]
    suc <- patternSuccess
    return [ suc, normalClause [VarP x] patternMatchFailure ]
  where
    tableName =
        unEntityNameDB (entityDB (unboundEntityDef entDef))
    patternSuccess =
        case fields of
            [] -> do
                rightE <- [|Right|]
                return $ normalClause [ListP []] (rightE `AppE` constructExpr)
            _ -> do
                x1 <- newName "x1"
                restNames <- mapM (\i -> newName $ "x" `mappend` show i) [2..length fields]
                (fpv1:mkPersistValues) <- mapM mkPersistValue fields
                app1E <- [|(<$>)|]
                let conApp = infixFromPersistValue app1E fpv1 constructExpr x1
                applyE <- [|(<*>)|]
                let applyFromPersistValue = infixFromPersistValue applyE

                return $ normalClause
                    [ListP $ fmap VarP (x1:restNames)]
                    (foldl' (\exp (name, fpv) -> applyFromPersistValue fpv exp name) conApp (zip restNames mkPersistValues))

    infixFromPersistValue applyE fpv exp name =
        UInfixE exp applyE (fpv `AppE` VarE name)

    mkPersistValue field =
        let fieldName = unFieldNameHS field
        in [|mapLeft (fieldError tableName fieldName) . fromPersistValue|]

-- |  Render an error message based on the @tableName@ and @fieldName@ with
-- the provided message.
--
-- @since 2.8.2
fieldError :: Text -> Text -> Text -> Text
fieldError tableName fieldName err = mconcat
    [ "Couldn't parse field `"
    , fieldName
    , "` from table `"
    , tableName
    , "`. "
    , err
    ]

mkEntity :: M.Map EntityNameHS a -> EntityMap -> MkPersistSettings -> UnboundEntityDef -> Q [Dec]
mkEntity embedEntityMap entityMap mps preDef = do
    entityDefExp <- liftAndFixKeys mps embedEntityMap entityMap preDef
    let
        entDef =
            fixEntityDef preDef
    fields <- mkFields mps entityMap entDef
    let name = mkEntityDefName entDef
    let clazz = ConT ''PersistEntity `AppT` genDataType
    tpf <- mkToPersistFields mps entDef
    fpv <- mkFromPersistValues mps entDef
    utv <- mkUniqueToValues $ entityUniques $ unboundEntityDef entDef
    puk <- mkUniqueKeys entDef
    fkc <- mapM (mkForeignKeysComposite mps entDef) $ unboundForeignDefs entDef

    toFieldNames <- mkToFieldNames $ entityUniques $ unboundEntityDef entDef

    (keyTypeDec, keyInstanceDecs) <- mkKeyTypeDec mps entDef
    keyToValues' <- mkKeyToValues mps entDef
    keyFromValues' <- mkKeyFromValues mps entDef

    let addSyn -- FIXME maybe remove this
            | mpsGeneric mps = (:) $
                TySynD name [] $
                    genericDataType mps entName $ mpsBackend mps
            | otherwise = id

    lensClauses <- mkLensClauses mps entDef

    lenses <- mkLenses mps entityMap entDef
    let instanceConstraint = if not (mpsGeneric mps) then [] else
          [mkClassP ''PersistStore [backendT]]

    [keyFromRecordM'] <-
        case unboundPrimarySpec entDef of
            NaturalKey ucd -> do
                recordName <- newName "record"
                let
                    keyCon =
                        keyConName entDef
                    keyFields' =
                        fieldNameToRecordName mps entDef <$> unboundCompositeCols ucd
                    constr =
                        foldl'
                            AppE
                            (ConE keyCon)
                            (toList $ fmap
                                (\n ->
                                    VarE n `AppE` VarE recordName
                                )
                                keyFields'
                            )
                    keyFromRec = varP 'keyFromRecordM
                [d|
                    $(keyFromRec) = Just ( \ $(varP recordName) -> $(pure constr))
                    |]

            _ ->
                [d|$(varP 'keyFromRecordM) = Nothing|]

    dtd <- dataTypeDec mps entityMap entDef
    let
        allEntDefs =
            entityFieldTHCon <$> efthAllFields fields
        allEntDefClauses =
            entityFieldTHClause <$> efthAllFields fields
    return $ addSyn $
       dtd : mconcat fkc `mappend`
      ( [ TySynD (keyIdName entDef) [] $
            ConT ''Key `AppT` ConT name
      , instanceD instanceConstraint clazz
        [ uniqueTypeDec mps entityMap entDef
        , keyTypeDec
        , keyToValues'
        , keyFromValues'
        , keyFromRecordM'
        , FunD 'entityDef [normalClause [WildP] entityDefExp]
        , tpf
        , FunD 'fromPersistValues fpv
        , toFieldNames
        , utv
        , puk
#if MIN_VERSION_template_haskell(2,15,0)
        , DataInstD
            []
            Nothing
            (AppT (AppT (ConT ''EntityField) genDataType) (VarT $ mkName "typ"))
            Nothing
            allEntDefs
            []
#else
        , DataInstD
            []
            ''EntityField
            [ genDataType
            , VarT $ mkName "typ"
            ]
            Nothing
            allEntDefs
            []
#endif
        , FunD 'persistFieldDef allEntDefClauses
#if MIN_VERSION_template_haskell(2,15,0)
        , TySynInstD
            (TySynEqn
               Nothing
               (AppT (ConT ''PersistEntityBackend) genDataType)
               (backendDataType mps))
#else
        , TySynInstD
            ''PersistEntityBackend
            (TySynEqn
               [genDataType]
               (backendDataType mps))
#endif
        , FunD 'persistIdField [normalClause [] (ConE $ keyIdName entDef)]
        , FunD 'fieldLens lensClauses
        ]
      ] `mappend` lenses) `mappend` keyInstanceDecs
  where
    genDataType =
        genericDataType mps entName backendT
    entName =
        getUnboundEntityNameHS preDef

data EntityFieldsTH = EntityFieldsTH
    { entityFieldsTHPrimary :: EntityFieldTH
    , entityFieldsTHFields :: [EntityFieldTH]
    }

efthAllFields :: EntityFieldsTH -> [EntityFieldTH]
efthAllFields EntityFieldsTH{..} =
    stripIdFieldDef entityFieldsTHPrimary : entityFieldsTHFields

stripIdFieldDef :: EntityFieldTH -> EntityFieldTH
stripIdFieldDef efth = efth
    { entityFieldTHClause =
        go (entityFieldTHClause efth)
    }
  where
    go (Clause ps bdy ds) =
        Clause ps bdy' ds
      where
        bdy' =
            case bdy of
                NormalB e ->
                    NormalB $ AppE (VarE 'stripIdFieldImpl) e
                _ ->
                    bdy

-- | @persistent@ used to assume that an Id was always a single field.
--
-- This method preserves as much backwards compatibility as possible.
stripIdFieldImpl :: HasCallStack => EntityIdDef -> FieldDef
stripIdFieldImpl eid =
    case eid of
        EntityIdField fd -> fd
        EntityIdNaturalKey cd ->
            case compositeFields cd of
                (x :| xs) ->
                    case xs of
                        [] ->
                            x
                        _ ->
                            dummyFieldDef
  where
    dummyFieldDef =
        FieldDef
            { fieldHaskell =
                FieldNameHS "Id"
            , fieldDB =
                FieldNameDB "__composite_key_no_id__"
            , fieldType =
                FTTypeCon Nothing "__Composite_Key__"
            , fieldSqlType =
                SqlOther "Composite Key"
            , fieldAttrs =
                []
            , fieldStrict =
                False
            , fieldReference =
                NoReference
            , fieldCascade =
                noCascade
            , fieldComments =
                Nothing
            , fieldGenerated =
                Nothing
            , fieldIsImplicitIdColumn =
                False
            }

mkFields :: MkPersistSettings -> EntityMap -> UnboundEntityDef -> Q EntityFieldsTH
mkFields mps entityMap entDef =
    EntityFieldsTH
        <$> mkIdField mps entDef
        <*> mapM (mkField mps entityMap entDef) (getUnboundFieldDefs entDef)

mkUniqueKeyInstances :: MkPersistSettings -> UnboundEntityDef -> Q [Dec]
mkUniqueKeyInstances mps entDef = do
    requirePersistentExtensions
    case entityUniques (unboundEntityDef entDef) of
        [] -> mappend <$> typeErrorSingle <*> typeErrorAtLeastOne
        [_] -> mappend <$> singleUniqueKey <*> atLeastOneKey
        (_:_) -> mappend <$> typeErrorMultiple <*> atLeastOneKey
  where
    requireUniquesPName = 'requireUniquesP
    onlyUniquePName = 'onlyUniqueP
    typeErrorSingle = mkOnlyUniqueError typeErrorNoneCtx
    typeErrorMultiple = mkOnlyUniqueError typeErrorMultipleCtx

    withPersistStoreWriteCxt =
        if mpsGeneric mps
            then do
                write <- [t|PersistStoreWrite $(pure backendT) |]
                pure [write]
            else do
                pure []

    typeErrorNoneCtx = do
        tyErr <- [t|TypeError (NoUniqueKeysError $(pure genDataType))|]
        (tyErr :) <$> withPersistStoreWriteCxt

    typeErrorMultipleCtx = do
        tyErr <- [t|TypeError (MultipleUniqueKeysError $(pure genDataType))|]
        (tyErr :) <$> withPersistStoreWriteCxt

    mkOnlyUniqueError :: Q Cxt -> Q [Dec]
    mkOnlyUniqueError mkCtx = do
        ctx <- mkCtx
        let impl = mkImpossible onlyUniquePName
        pure [instanceD ctx onlyOneUniqueKeyClass impl]

    mkImpossible name =
        [ FunD name
            [ Clause
                [ WildP ]
                (NormalB
                    (VarE 'error `AppE` LitE (StringL "impossible"))
                )
                []
            ]
        ]

    typeErrorAtLeastOne :: Q [Dec]
    typeErrorAtLeastOne = do
        let impl = mkImpossible requireUniquesPName
        cxt <- typeErrorMultipleCtx
        pure [instanceD cxt atLeastOneUniqueKeyClass impl]

    singleUniqueKey :: Q [Dec]
    singleUniqueKey = do
        expr <- [e| head . persistUniqueKeys|]
        let impl = [FunD onlyUniquePName [Clause [] (NormalB expr) []]]
        cxt <- withPersistStoreWriteCxt
        pure [instanceD cxt onlyOneUniqueKeyClass impl]

    atLeastOneUniqueKeyClass = ConT ''AtLeastOneUniqueKey `AppT` genDataType
    onlyOneUniqueKeyClass =  ConT ''OnlyOneUniqueKey `AppT` genDataType

    atLeastOneKey :: Q [Dec]
    atLeastOneKey = do
        expr <- [e| NEL.fromList . persistUniqueKeys|]
        let impl = [FunD requireUniquesPName [Clause [] (NormalB expr) []]]
        cxt <- withPersistStoreWriteCxt
        pure [instanceD cxt atLeastOneUniqueKeyClass impl]

    genDataType =
        genericDataType mps (getUnboundEntityNameHS entDef) backendT

entityText :: UnboundEntityDef -> Text
entityText = unEntityNameHS . getUnboundEntityNameHS

mkLenses :: MkPersistSettings -> EntityMap -> UnboundEntityDef -> Q [Dec]
mkLenses mps _ _ | not (mpsGenerateLenses mps) = return []
mkLenses _ _ ent | entitySum (unboundEntityDef ent) = return []
mkLenses mps entityMap ent = fmap mconcat $ forM (getUnboundFieldDefs ent) $ \field -> do
    let lensName = mkEntityLensName mps ent field
        fieldName = fieldDefToRecordName mps ent field
    needleN <- newName "needle"
    setterN <- newName "setter"
    fN <- newName "f"
    aN <- newName "a"
    yN <- newName "y"
    let needle = VarE needleN
        setter = VarE setterN
        f = VarE fN
        a = VarE aN
        y = VarE yN
        fT = mkName "f"
        -- FIXME if we want to get really fancy, then: if this field is the
        -- *only* Id field present, then set backend1 and backend2 to different
        -- values
        backend1 = backendName
        backend2 = backendName
        aT =
            maybeIdType mps entityMap field (Just backend1) Nothing
        bT =
            maybeIdType mps entityMap field (Just backend2) Nothing
        mkST backend =
            genericDataType mps (getUnboundEntityNameHS ent) (VarT backend)
        sT = mkST backend1
        tT = mkST backend2
        t1 `arrow` t2 = ArrowT `AppT` t1 `AppT` t2
        vars = mkForallTV fT
             : (if mpsGeneric mps then [mkForallTV backend1{-, PlainTV backend2-}] else [])
    return
        [ SigD lensName $ ForallT vars [mkClassP ''Functor [VarT fT]] $
            (aT `arrow` (VarT fT `AppT` bT)) `arrow`
            (sT `arrow` (VarT fT `AppT` tT))
        , FunD lensName $ return $ Clause
            [VarP fN, VarP aN]
            (NormalB $ fmapE
                `AppE` setter
                `AppE` (f `AppE` needle))
            [ FunD needleN [normalClause [] (VarE fieldName `AppE` a)]
            , FunD setterN $ return $ normalClause
                [VarP yN]
                (RecUpdE a
                    [ (fieldName, y)
                    ])
            ]
        ]

#if MIN_VERSION_template_haskell(2,17,0)
mkPlainTV
    :: Name
    -> TyVarBndr ()
mkPlainTV n = PlainTV n ()

mkDoE :: [Stmt] -> Exp
mkDoE stmts = DoE Nothing stmts

mkForallTV :: Name -> TyVarBndr Specificity
mkForallTV n = PlainTV n SpecifiedSpec
#else

mkDoE :: [Stmt] -> Exp
mkDoE = DoE

mkPlainTV
    :: Name
    -> TyVarBndr
mkPlainTV = PlainTV

mkForallTV
    :: Name
    -> TyVarBndr
mkForallTV = mkPlainTV
#endif

mkForeignKeysComposite
    :: MkPersistSettings
    -> UnboundEntityDef
    -> UnboundForeignDef
    -> Q [Dec]
mkForeignKeysComposite mps entDef foreignDef
    | foreignToPrimary (unboundForeignDef foreignDef) = do
        let
            fieldName =
                fieldNameToRecordName mps entDef
            fname =
                fieldName $ constraintToField $ foreignConstraintNameHaskell $ unboundForeignDef foreignDef
            reftableString =
                unpack $ unEntityNameHS $ foreignRefTableHaskell $ unboundForeignDef foreignDef
            reftableKeyName =
                mkName $ reftableString `mappend` "Key"
            tablename =
                mkEntityDefName entDef
            fieldStore =
                mkFieldStore entDef

        recordName <- newName "record_mkForeignKeysComposite"

        let
            mkFldE foreignName  =
                -- using coerce here to convince SqlBackendKey to go away
                VarE 'coerce `AppE`
                (VarE (fieldName foreignName) `AppE` VarE recordName)
            mkFldR ffr =
                let
                    e =
                        mkFldE (ffrSourceField ffr)
                in
                    case ffrTargetField ffr of
                        FieldNameHS "Id" ->
                            VarE 'toBackendKey `AppE`
                                e
                        _ ->
                            e
            foreignFieldNames foreignFieldList =
                case foreignFieldList of
                    FieldListImpliedId names ->
                        names
                    FieldListHasReferences refs ->
                        fmap ffrSourceField refs

            fldsE =
                getForeignNames $ (unboundForeignFields foreignDef)
            getForeignNames = \case
                FieldListImpliedId xs ->
                   fmap mkFldE xs
                FieldListHasReferences xs ->
                    fmap mkFldR xs

            nullErr n =
               error $ "Could not find field definition for: " <> show n
            fNullable =
                setNull
                   $ fmap (\n -> fromMaybe (nullErr n) $ getFieldDef n fieldStore)
                   $ foreignFieldNames
                   $ unboundForeignFields foreignDef
            mkKeyE =
                foldl' AppE (maybeExp fNullable $ ConE reftableKeyName) fldsE
            fn =
                FunD fname [normalClause [VarP recordName] mkKeyE]

            keyTargetTable =
                maybeTyp fNullable $ ConT ''Key `AppT` ConT (mkName reftableString)

        sigTy <- [t| $(conT tablename) -> $(pure keyTargetTable) |]
        pure
            [ SigD fname sigTy
            , fn
            ]

    | otherwise =
        pure []
  where
    constraintToField = FieldNameHS . unConstraintNameHS


maybeExp :: Bool -> Exp -> Exp
maybeExp may exp | may = fmapE `AppE` exp
                 | otherwise = exp

maybeTyp :: Bool -> Type -> Type
maybeTyp may typ | may = ConT ''Maybe `AppT` typ
                 | otherwise = typ

entityToPersistValueHelper :: (PersistEntity record) => record -> PersistValue
entityToPersistValueHelper entity = PersistMap $ zip columnNames fieldsAsPersistValues
    where
        columnNames = fmap (unFieldNameHS . fieldHaskell) (getEntityFields (entityDef (Just entity)))
        fieldsAsPersistValues = fmap toPersistValue $ toPersistFields entity

entityFromPersistValueHelper
    :: (PersistEntity record)
    => [String] -- ^ Column names, as '[String]' to avoid extra calls to "pack" in the generated code
    -> PersistValue
    -> Either Text record
entityFromPersistValueHelper columnNames pv = do
    (persistMap :: [(T.Text, PersistValue)]) <- getPersistMap pv

    let columnMap = HM.fromList persistMap
        lookupPersistValueByColumnName :: String -> PersistValue
        lookupPersistValueByColumnName columnName =
            fromMaybe PersistNull (HM.lookup (pack columnName) columnMap)

    fromPersistValues $ fmap lookupPersistValueByColumnName columnNames

-- | Produce code similar to the following:
--
-- @
--   instance PersistEntity e => PersistField e where
--      toPersistValue = entityToPersistValueHelper
--      fromPersistValue = entityFromPersistValueHelper ["col1", "col2"]
--      sqlType _ = SqlString
-- @
persistFieldFromEntity :: MkPersistSettings -> UnboundEntityDef -> Q [Dec]
persistFieldFromEntity mps entDef = do
    sqlStringConstructor' <- [|SqlString|]
    toPersistValueImplementation <- [|entityToPersistValueHelper|]
    fromPersistValueImplementation <- [|entityFromPersistValueHelper columnNames|]

    return
        [ persistFieldInstanceD (mpsGeneric mps) typ
            [ FunD 'toPersistValue [ normalClause [] toPersistValueImplementation ]
            , FunD 'fromPersistValue
                [ normalClause [] fromPersistValueImplementation ]
            ]
        , persistFieldSqlInstanceD (mpsGeneric mps) typ
            [ sqlTypeFunD sqlStringConstructor'
            ]
        ]
  where
    typ =
        genericDataType mps (entityHaskell (unboundEntityDef entDef)) backendT
    entFields =
        filter isHaskellUnboundField $ getUnboundFieldDefs entDef
    columnNames =
        fmap (unpack . unFieldNameHS . unboundFieldNameHS) entFields

-- | Apply the given list of functions to the same @EntityDef@s.
--
-- This function is useful for cases such as:
--
-- >>> share [mkSave "myDefs", mkPersist sqlSettings] [persistLowerCase|...|]
share :: [[a] -> Q [Dec]] -> [a] -> Q [Dec]
share fs x = mconcat <$> mapM ($ x) fs

-- | Save the @EntityDef@s passed in under the given name.
--
-- This function was deprecated in @persistent-2.13.0.0@. It doesn't properly
-- fix foreign keys. Please refer to 'mkEntityDefList' for a replacement.
mkSave :: String -> [EntityDef] -> Q [Dec]
mkSave name' defs' = do
    let name = mkName name'
    defs <- lift defs'
    return [ SigD name $ ListT `AppT` ConT ''EntityDef
           , FunD name [normalClause [] defs]
           ]

{-# DEPRECATED mkSave "This function is broken. mkEntityDefList is a drop-in replacement that will properly handle foreign keys correctly." #-}

data Dep = Dep
    { depTarget :: EntityNameHS
    , depSourceTable :: EntityNameHS
    , depSourceField :: FieldNameHS
    , depSourceNull  :: IsNullable
    }

{-# DEPRECATED mkDeleteCascade "You can now set update and delete cascade behavior directly on the entity in the quasiquoter. This function and class are deprecated and will be removed in the next major ersion." #-}

-- | Generate a 'DeleteCascade' instance for the given @EntityDef@s.
--
-- This function is deprecated as of 2.13.0.0. You can now set cascade
-- behavior directly in the quasiquoter.
mkDeleteCascade :: MkPersistSettings -> [UnboundEntityDef] -> Q [Dec]
mkDeleteCascade mps defs = do
    let deps = concatMap getDeps defs
    mapM (go deps) defs
  where
    getDeps :: UnboundEntityDef -> [Dep]
    getDeps def =
        concatMap getDeps' $ getUnboundFieldDefs $ fixEntityDef def
      where
        getDeps' :: UnboundFieldDef -> [Dep]
        getDeps' field =
            case guessFieldReference field of
                Just name ->
                    return Dep
                        { depTarget = name
                        , depSourceTable = entityHaskell (unboundEntityDef def)
                        , depSourceField = unboundFieldNameHS field
                        , depSourceNull  = isUnboundFieldNullable field
                        }
                Nothing ->
                    []
    go :: [Dep] -> UnboundEntityDef -> Q Dec
    go allDeps ued = do
        let name = entityHaskell (unboundEntityDef ued)
        let deps = filter (\x -> depTarget x == name) allDeps
        key <- newName "key"
        let del = VarE 'delete
        let dcw = VarE 'deleteCascadeWhere
        just <- [|Just|]
        filt <- [|Filter|]
        eq <- [|Eq|]
        value <- [|FilterValue|]
        let mkStmt :: Dep -> Stmt
            mkStmt dep = NoBindS
                $ dcw `AppE`
                  ListE
                    [ filt `AppE` ConE filtName
                           `AppE` (value `AppE` val (depSourceNull dep))
                           `AppE` eq
                    ]
              where
                filtName = filterConName' mps (depSourceTable dep) (depSourceField dep)
                val (Nullable ByMaybeAttr) = just `AppE` VarE key
                val _                      =             VarE key

        let stmts :: [Stmt]
            stmts = fmap mkStmt deps `mappend`
                    [NoBindS $ del `AppE` VarE key]

        let entityT = genericDataType mps name backendT

        return $
            instanceD
            [ mkClassP ''PersistQuery [backendT]
            , mkEqualP (ConT ''PersistEntityBackend `AppT` entityT) (ConT ''BaseBackend `AppT` backendT)
            ]
            (ConT ''DeleteCascade `AppT` entityT `AppT` backendT)
            [ FunD 'deleteCascade
                [normalClause [VarP key] (mkDoE stmts)]
            ]

-- | Creates a declaration for the @['EntityDef']@ from the @persistent@
-- schema. This is necessary because the Persistent QuasiQuoter is unable
-- to know the correct type of ID fields, and assumes that they are all
-- Int64.
--
-- Provide this in the list you give to 'share', much like @'mkMigrate'@.
--
-- @
-- 'share' ['mkMigrate' "migrateAll", 'mkEntityDefList' "entityDefs"] [...]
-- @
--
-- @since 2.7.1
mkEntityDefList
    :: String
    -- ^ The name that will be given to the 'EntityDef' list.
    -> [UnboundEntityDef]
    -> Q [Dec]
mkEntityDefList entityList entityDefs = do
    let entityListName = mkName entityList
    edefs <- fmap ListE
        . forM entityDefs
        $ \entDef ->
            let entityType = entityDefConT entDef
             in [|entityDef (Proxy :: Proxy $(entityType))|]
    typ <- [t|[EntityDef]|]
    pure
        [ SigD entityListName typ
        , ValD (VarP entityListName) (NormalB edefs) []
        ]

mkUniqueKeys :: UnboundEntityDef -> Q Dec
mkUniqueKeys def | entitySum (unboundEntityDef def) =
    return $ FunD 'persistUniqueKeys [normalClause [WildP] (ListE [])]
mkUniqueKeys def = do
    c <- clause
    return $ FunD 'persistUniqueKeys [c]
  where
    clause = do
        xs <- forM (getUnboundFieldDefs def) $ \fieldDef -> do
            let x = unboundFieldNameHS fieldDef
            x' <- newName $ '_' : unpack (unFieldNameHS x)
            return (x, x')
        let pcs = fmap (go xs) $ entityUniques $ unboundEntityDef def
        let pat = ConP
                (mkEntityDefName def)
                (fmap (VarP . snd) xs)
        return $ normalClause [pat] (ListE pcs)

    go :: [(FieldNameHS, Name)] -> UniqueDef -> Exp
    go xs (UniqueDef name _ cols _) =
        foldl' (go' xs) (ConE (mkConstraintName name)) (toList $ fmap fst cols)

    go' :: [(FieldNameHS, Name)] -> Exp -> FieldNameHS -> Exp
    go' xs front col =
        let Just col' = lookup col xs
         in front `AppE` VarE col'

sqlTypeFunD :: Exp -> Dec
sqlTypeFunD st = FunD 'sqlType
                [ normalClause [WildP] st ]

typeInstanceD
    :: Name
    -> Bool -- ^ include PersistStore backend constraint
    -> Type
    -> [Dec]
    -> Dec
typeInstanceD clazz hasBackend typ =
    instanceD ctx (ConT clazz `AppT` typ)
  where
    ctx
        | hasBackend = [mkClassP ''PersistStore [backendT]]
        | otherwise = []

persistFieldInstanceD :: Bool -- ^ include PersistStore backend constraint
                      -> Type -> [Dec] -> Dec
persistFieldInstanceD = typeInstanceD ''PersistField

persistFieldSqlInstanceD :: Bool -- ^ include PersistStore backend constraint
                         -> Type -> [Dec] -> Dec
persistFieldSqlInstanceD = typeInstanceD ''PersistFieldSql

-- | Automatically creates a valid 'PersistField' instance for any datatype
-- that has valid 'Show' and 'Read' instances. Can be very convenient for
-- 'Enum' types.
derivePersistField :: String -> Q [Dec]
derivePersistField s = do
    ss <- [|SqlString|]
    tpv <- [|PersistText . pack . show|]
    fpv <- [|\dt v ->
                case fromPersistValue v of
                    Left e -> Left e
                    Right s' ->
                        case reads $ unpack s' of
                            (x, _):_ -> Right x
                            [] -> Left $ pack "Invalid " ++ pack dt ++ pack ": " ++ s'|]
    return
        [ persistFieldInstanceD False (ConT $ mkName s)
            [ FunD 'toPersistValue
                [ normalClause [] tpv
                ]
            , FunD 'fromPersistValue
                [ normalClause [] (fpv `AppE` LitE (StringL s))
                ]
            ]
        , persistFieldSqlInstanceD False (ConT $ mkName s)
            [ sqlTypeFunD ss
            ]
        ]

-- | Automatically creates a valid 'PersistField' instance for any datatype
-- that has valid 'ToJSON' and 'FromJSON' instances. For a datatype @T@ it
-- generates instances similar to these:
--
-- @
--    instance PersistField T where
--        toPersistValue = PersistByteString . L.toStrict . encode
--        fromPersistValue = (left T.pack) . eitherDecodeStrict' <=< fromPersistValue
--    instance PersistFieldSql T where
--        sqlType _ = SqlString
-- @
derivePersistFieldJSON :: String -> Q [Dec]
derivePersistFieldJSON s = do
    ss <- [|SqlString|]
    tpv <- [|PersistText . toJsonText|]
    fpv <- [|\dt v -> do
                text <- fromPersistValue v
                let bs' = TE.encodeUtf8 text
                case eitherDecodeStrict' bs' of
                    Left e -> Left $ pack "JSON decoding error for " ++ pack dt ++ pack ": " ++ pack e ++ pack ". On Input: " ++ decodeUtf8 bs'
                    Right x -> Right x|]
    return
        [ persistFieldInstanceD False (ConT $ mkName s)
            [ FunD 'toPersistValue
                [ normalClause [] tpv
                ]
            , FunD 'fromPersistValue
                [ normalClause [] (fpv `AppE` LitE (StringL s))
                ]
            ]
        , persistFieldSqlInstanceD False (ConT $ mkName s)
            [ sqlTypeFunD ss
            ]
        ]

-- | The basic function for migrating models, no Template Haskell required.
--
-- It's probably best to use this in concert with 'mkEntityDefList', and then
-- call 'migrateModels' with the result from that function.
--
-- @
-- share [mkPersist sqlSettings, mkEntityDefList "entities"] [persistLowerCase| ... |]
--
-- migrateAll = 'migrateModels' entities
-- @
--
-- The function 'mkMigrate' currently implements exactly this behavior now. If
-- you're splitting up the entity definitions into separate files, then it is
-- better to use the entity definition list and the concatenate all the models
-- together into a big list to call with 'migrateModels'.
--
-- @
-- module Foo where
--
--     share [mkPersist s, mkEntityDefList "fooModels"] ...
--
--
-- module Bar where
--
--     share [mkPersist s, mkEntityDefList "barModels"] ...
--
-- module Migration where
--
--     import Foo
--     import Bar
--
--     migrateAll = migrateModels (fooModels <> barModels)
-- @
--
-- @since 2.13.0.0
migrateModels :: [EntityDef] -> Migration
migrateModels defs=
    forM_ (filter isMigrated defs) $ \def ->
        migrate defs def
  where
    isMigrated def = pack "no-migrate" `notElem` entityAttrs def

-- | Creates a single function to perform all migrations for the entities
-- defined here. One thing to be aware of is dependencies: if you have entities
-- with foreign references, make sure to place those definitions after the
-- entities they reference.
--
-- In @persistent-2.13.0.0@, this was changed to *ignore* the input entity def
-- list, and instead defer to 'mkEntityDefList' to get the correct entities.
-- This avoids problems where the QuasiQuoter is unable to know what the right
-- reference types are. This sets 'mkPersist' to be the "single source of truth"
-- for entity definitions.
mkMigrate :: String -> [UnboundEntityDef] -> Q [Dec]
mkMigrate fun eds = do
    let entityDefListName = ("entityDefListFor" <> fun)
    body <- [| migrateModels $(varE (mkName entityDefListName)) |]
    edList <- mkEntityDefList entityDefListName eds
    pure $ edList <>
        [ SigD (mkName fun) (ConT ''Migration)
        , FunD (mkName fun) [normalClause [] body]
        ]

data EntityFieldTH = EntityFieldTH
    { entityFieldTHCon :: Con
    , entityFieldTHClause :: Clause
    }

-- Ent
--   fieldName FieldType
--
-- forall . typ ~ FieldType => EntFieldName
--
-- EntFieldName = FieldDef ....
--
-- Field Def Accessors Required:
mkField :: MkPersistSettings -> EntityMap -> UnboundEntityDef -> UnboundFieldDef -> Q EntityFieldTH
mkField mps entityMap et fieldDef = do
    let
        con =
            ForallC
                []
                [mkEqualP (VarT $ mkName "typ") fieldT]
                $ NormalC name []
        fieldT =
            maybeIdType mps entityMap fieldDef Nothing Nothing
    bod <- mkLookupEntityField et (unboundFieldNameHS fieldDef)
    let cla = normalClause
                [ConP name []]
                bod
    return $ EntityFieldTH con cla
  where
    name = filterConName mps et fieldDef

mkIdField :: MkPersistSettings -> UnboundEntityDef -> Q EntityFieldTH
mkIdField mps ued = do
    let
        entityName =
            getUnboundEntityNameHS ued
        entityIdType
            | mpsGeneric mps =
                ConT ''Key `AppT` (
                    ConT (mkEntityNameHSGenericName entityName)
                    `AppT` backendT
                )
            | otherwise =
                ConT $ mkName $ (T.unpack $ unEntityNameHS entityName) ++ "Id"
        name =
            filterConName' mps entityName (FieldNameHS "Id")
    clause  <-
        fixPrimarySpec mps ued
    pure EntityFieldTH
        { entityFieldTHCon =
            ForallC
                []
                [mkEqualP (VarT $ mkName "typ") entityIdType]
                $ NormalC name []
        , entityFieldTHClause =
            normalClause [ConP name []] clause
        }

lookupEntityField
    :: PersistEntity entity
    => Proxy entity
    -> FieldNameHS
    -> FieldDef
lookupEntityField prxy fieldNameHS =
    fromMaybe boom $ List.find ((fieldNameHS ==) . fieldHaskell) $ entityFields $ entityDef prxy
  where
    boom =
        error "Database.Persist.TH.Internal.lookupEntityField: failed to find entity field with database name"

mkLookupEntityField
    :: UnboundEntityDef
    -> FieldNameHS
    -> Q Exp
mkLookupEntityField ued ufd =
    [|
        lookupEntityField
            (Proxy :: Proxy $(conT entityName))
            $(lift ufd)
    |]
  where
    entityName = mkEntityNameHSName (getUnboundEntityNameHS ued)

maybeNullable :: UnboundFieldDef -> Bool
maybeNullable fd = isUnboundFieldNullable fd == Nullable ByMaybeAttr

ftToType :: FieldType -> Type
ftToType = \case
    FTTypeCon Nothing t ->
        ConT $ mkName $ T.unpack t
    -- This type is generated from the Quasi-Quoter.
    -- Adding this special case avoids users needing to import Data.Int
    FTTypeCon (Just "Data.Int") "Int64" ->
        ConT ''Int64
    FTTypeCon (Just m) t ->
        ConT $ mkName $ unpack $ concat [m, ".", t]
    FTTypePromoted t ->
        PromotedT $ mkName $ T.unpack t
    FTApp x y ->
        ftToType x `AppT` ftToType y
    FTList x ->
        ListT `AppT` ftToType x

infixr 5 ++
(++) :: Monoid m => m -> m -> m
(++) = mappend

mkJSON :: MkPersistSettings -> UnboundEntityDef -> Q [Dec]
mkJSON _ def | ("json" `notElem` entityAttrs (unboundEntityDef def)) = return []
mkJSON mps (fixEntityDef -> def) = do
    requireExtensions [[FlexibleInstances]]
    pureE <- [|pure|]
    apE' <- [|(<*>)|]
    packE <- [|pack|]
    dotEqualE <- [|(.=)|]
    dotColonE <- [|(.:)|]
    dotColonQE <- [|(.:?)|]
    objectE <- [|object|]
    obj <- newName "obj"
    mzeroE <- [|mzero|]
    let
        fields =
            getUnboundFieldDefs def

    xs <- mapM fieldToJSONValName fields

    let
        conName =
            mkEntityDefName def
        typ =
            genericDataType mps (entityHaskell (unboundEntityDef def)) backendT
        toJSONI =
            typeInstanceD ''ToJSON (mpsGeneric mps) typ [toJSON']
          where
            toJSON' = FunD 'toJSON $ return $ normalClause
                [ConP conName $ fmap VarP xs]
                (objectE `AppE` ListE pairs)
              where
                pairs = zipWith toPair fields xs
                toPair f x = InfixE
                    (Just (packE `AppE` LitE (StringL $ unpack $ unFieldNameHS $ unboundFieldNameHS f)))
                    dotEqualE
                    (Just $ VarE x)
        fromJSONI =
            typeInstanceD ''FromJSON (mpsGeneric mps) typ [parseJSON']
          where
            parseJSON' = FunD 'parseJSON
                [ normalClause [ConP 'Object [VarP obj]]
                    (foldl'
                        (\x y -> InfixE (Just x) apE' (Just y))
                        (pureE `AppE` ConE conName)
                        pulls
                    )
                , normalClause [WildP] mzeroE
                ]
              where
                pulls =
                    fmap toPull fields
                toPull f = InfixE
                    (Just $ VarE obj)
                    (if maybeNullable f then dotColonQE else dotColonE)
                    (Just $ AppE packE $ LitE $ StringL $ unpack $ unFieldNameHS $ unboundFieldNameHS f)
    case mpsEntityJSON mps of
        Nothing ->
            return [toJSONI, fromJSONI]
        Just entityJSON -> do
            entityJSONIs <- if mpsGeneric mps
              then [d|
                instance PersistStore $(pure backendT) => ToJSON (Entity $(pure typ)) where
                    toJSON = $(varE (entityToJSON entityJSON))
                instance PersistStore $(pure backendT) => FromJSON (Entity $(pure typ)) where
                    parseJSON = $(varE (entityFromJSON entityJSON))
                |]
              else [d|
                instance ToJSON (Entity $(pure typ)) where
                    toJSON = $(varE (entityToJSON entityJSON))
                instance FromJSON (Entity $(pure typ)) where
                    parseJSON = $(varE (entityFromJSON entityJSON))
                |]
            return $ toJSONI : fromJSONI : entityJSONIs

mkClassP :: Name -> [Type] -> Pred
mkClassP cla tys = foldl AppT (ConT cla) tys

mkEqualP :: Type -> Type -> Pred
mkEqualP tleft tright = foldl AppT EqualityT [tleft, tright]

notStrict :: Bang
notStrict = Bang NoSourceUnpackedness NoSourceStrictness

isStrict :: Bang
isStrict = Bang NoSourceUnpackedness SourceStrict

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

-- | Check that all of Persistent's required extensions are enabled, or else fail compilation
--
-- This function should be called before any code that depends on one of the required extensions being enabled.
requirePersistentExtensions :: Q ()
requirePersistentExtensions = requireExtensions requiredExtensions
  where
    requiredExtensions = fmap pure
        [ DerivingStrategies
        , GeneralizedNewtypeDeriving
        , StandaloneDeriving
        , UndecidableInstances
        , MultiParamTypeClasses
        ]

mkSymbolToFieldInstances :: MkPersistSettings -> EntityMap -> UnboundEntityDef -> Q [Dec]
mkSymbolToFieldInstances mps entityMap (fixEntityDef -> ed) = do
    let
        entityHaskellName =
            getEntityHaskellName $ unboundEntityDef ed
        allFields =
            getUnboundFieldDefs ed
        mkEntityFieldConstr fieldHaskellName =
            conE $ filterConName' mps entityHaskellName fieldHaskellName
                :: Q Exp
    regularFields <- forM (toList allFields) $ \fieldDef -> do
        let
            fieldHaskellName =
                unboundFieldNameHS fieldDef

        let fieldNameT :: Q Type
            fieldNameT =
                litT $ strTyLit
                    $ T.unpack $ lowerFirstIfId
                    $ unFieldNameHS fieldHaskellName

            lowerFirstIfId "Id" = "id"
            lowerFirstIfId xs = xs

            fieldTypeT
                | fieldHaskellName == FieldNameHS "Id" =
                    conT ''Key `appT` recordNameT
                | otherwise =
                    pure $ maybeIdType mps entityMap fieldDef Nothing Nothing
            entityFieldConstr =
                mkEntityFieldConstr fieldHaskellName
        mkInstance fieldNameT fieldTypeT entityFieldConstr

    mkey <-
        case unboundPrimarySpec ed of
            NaturalKey _ ->
                pure []
            _ -> do
                let
                    fieldHaskellName =
                        FieldNameHS "Id"
                    entityFieldConstr =
                        mkEntityFieldConstr fieldHaskellName
                    fieldTypeT =
                        conT ''Key `appT` recordNameT
                mkInstance [t|"id"|] fieldTypeT entityFieldConstr

    pure (mkey <> join regularFields)
  where
    nameG =
        mkEntityDefGenericName ed
    recordNameT
        | mpsGeneric mps =
            conT nameG `appT` varT backendName
        | otherwise =
            entityDefConT ed
    mkInstance fieldNameT fieldTypeT entityFieldConstr =
        [d|
            instance SymbolToField $(fieldNameT) $(recordNameT) $(fieldTypeT) where
                symbolToField = $(entityFieldConstr)
            |]

-- | Pass in a list of lists of extensions, where any of the given
-- extensions will satisfy it. For example, you might need either GADTs or
-- ExistentialQuantification, so you'd write:
--
-- > requireExtensions [[GADTs, ExistentialQuantification]]
--
-- But if you need TypeFamilies and MultiParamTypeClasses, then you'd
-- write:
--
-- > requireExtensions [[TypeFamilies], [MultiParamTypeClasses]]
requireExtensions :: [[Extension]] -> Q ()
requireExtensions requiredExtensions = do
  -- isExtEnabled breaks the persistent-template benchmark with the following error:
  -- Template Haskell error: Can't do `isExtEnabled' in the IO monad
  -- You can workaround this by replacing isExtEnabled with (pure . const True)
  unenabledExtensions <- filterM (fmap (not . or) . traverse isExtEnabled) requiredExtensions

  case mapMaybe listToMaybe unenabledExtensions of
    [] -> pure ()
    [extension] -> fail $ mconcat
                     [ "Generating Persistent entities now requires the "
                     , show extension
                     , " language extension. Please enable it by copy/pasting this line to the top of your file:\n\n"
                     , extensionToPragma extension
                     ]
    extensions -> fail $ mconcat
                    [ "Generating Persistent entities now requires the following language extensions:\n\n"
                    , List.intercalate "\n" (fmap show extensions)
                    , "\n\nPlease enable the extensions by copy/pasting these lines into the top of your file:\n\n"
                    , List.intercalate "\n" (fmap extensionToPragma extensions)
                    ]

  where
    extensionToPragma ext = "{-# LANGUAGE " <> show ext <> " #-}"

-- | creates a TH Name for use in the ToJSON instance
fieldToJSONValName :: UnboundFieldDef -> Q Name
fieldToJSONValName =
    newName . T.unpack . unFieldNameHSForJSON . unboundFieldNameHS

-- | This special-cases "type_" and strips out its underscore. When
-- used for JSON serialization and deserialization, it works around
-- <https://github.com/yesodweb/persistent/issues/412>
unFieldNameHSForJSON :: FieldNameHS -> Text
unFieldNameHSForJSON = fixTypeUnderscore . unFieldNameHS
  where
    fixTypeUnderscore = \case
        "type" -> "type_"
        name -> name

entityDefConK :: UnboundEntityDef -> Kind
entityDefConK = conK . mkEntityDefName

entityDefConT :: UnboundEntityDef -> Q Type
entityDefConT = pure . entityDefConK

entityDefConE :: UnboundEntityDef -> Exp
entityDefConE = ConE . mkEntityDefName

-- | creates a TH Name for an entity's field, based on the entity
-- name and the field name, so for example:
--
-- Customer
--   name Text
--
-- This would generate `customerName` as a TH Name
fieldNameToRecordName :: MkPersistSettings -> UnboundEntityDef -> FieldNameHS -> Name
fieldNameToRecordName mps entDef fieldName =
    mkRecordName mps mUnderscore (entityHaskell (unboundEntityDef entDef)) fieldName
  where
    mUnderscore
        | mpsGenerateLenses mps = Just "_"
        | otherwise = Nothing

-- | as above, only takes a `FieldDef`
fieldDefToRecordName :: MkPersistSettings -> UnboundEntityDef -> UnboundFieldDef -> Name
fieldDefToRecordName mps entDef fieldDef =
    fieldNameToRecordName mps entDef (unboundFieldNameHS fieldDef)

-- | creates a TH Name for a lens on an entity's field, based on the entity
-- name and the field name, so as above but for the Lens
--
-- Customer
--   name Text
--
-- Generates a lens `customerName` when `mpsGenerateLenses` is true
-- while `fieldNameToRecordName` generates a prefixed function
-- `_customerName`
mkEntityLensName :: MkPersistSettings -> UnboundEntityDef -> UnboundFieldDef -> Name
mkEntityLensName mps entDef fieldDef =
    mkRecordName mps Nothing (entityHaskell (unboundEntityDef entDef)) (unboundFieldNameHS fieldDef)

mkRecordName :: MkPersistSettings -> Maybe Text -> EntityNameHS -> FieldNameHS -> Name
mkRecordName mps prefix entNameHS fieldNameHS =
    mkName $ T.unpack $ fromMaybe "" prefix <> lowerFirst recName
  where
    recName :: Text
    recName
      | mpsPrefixFields mps = mpsFieldLabelModifier mps entityNameText (upperFirst fieldNameText)
      | otherwise           = fieldNameText

    entityNameText :: Text
    entityNameText =
      unEntityNameHS entNameHS

    fieldNameText :: Text
    fieldNameText =
        unFieldNameHS fieldNameHS

-- | Construct a list of TH Names for the typeclasses of an EntityDef's `entityDerives`
mkEntityDefDeriveNames :: MkPersistSettings -> UnboundEntityDef -> [Name]
mkEntityDefDeriveNames mps entDef =
    let
        entityInstances =
            mkName . T.unpack <$> entityDerives (unboundEntityDef entDef)
        additionalInstances =
            filter (`notElem` entityInstances) $ mpsDeriveInstances mps
     in
        entityInstances <> additionalInstances

-- | Make a TH Name for the EntityDef's Haskell type
mkEntityNameHSName :: EntityNameHS -> Name
mkEntityNameHSName =
    mkName . T.unpack . unEntityNameHS

-- | As above only taking an `EntityDef`
mkEntityDefName :: UnboundEntityDef -> Name
mkEntityDefName =
    mkEntityNameHSName . entityHaskell . unboundEntityDef

-- | Make a TH Name for the EntityDef's Haskell type, when using mpsGeneric
mkEntityDefGenericName :: UnboundEntityDef -> Name
mkEntityDefGenericName =
    mkEntityNameHSGenericName . entityHaskell . unboundEntityDef

mkEntityNameHSGenericName :: EntityNameHS -> Name
mkEntityNameHSGenericName name =
    mkName $ T.unpack (unEntityNameHS name <> "Generic")

-- needs:
--
-- * entityHaskell
--     * field on EntityDef
-- * fieldHaskell
--     * field on FieldDef
--
sumConstrName :: MkPersistSettings -> UnboundEntityDef -> UnboundFieldDef -> Name
sumConstrName mps entDef unboundFieldDef =
    mkName $ T.unpack name
  where
    name
        | mpsPrefixFields mps = modifiedName ++ "Sum"
        | otherwise           = fieldName ++ "Sum"
    fieldNameHS =
        unboundFieldNameHS unboundFieldDef
    modifiedName =
        mpsConstraintLabelModifier mps entityName fieldName
    entityName =
        unEntityNameHS $ getUnboundEntityNameHS entDef
    fieldName =
        upperFirst $ unFieldNameHS fieldNameHS

-- | Turn a ConstraintName into a TH Name
mkConstraintName :: ConstraintNameHS -> Name
mkConstraintName (ConstraintNameHS name) =
    mkName (T.unpack name)

keyIdName :: UnboundEntityDef -> Name
keyIdName = mkName . T.unpack . keyIdText

keyIdText :: UnboundEntityDef -> Text
keyIdText entDef = unEntityNameHS (getUnboundEntityNameHS entDef) `mappend` "Id"

unKeyName :: UnboundEntityDef -> Name
unKeyName entDef = mkName $ T.unpack $ "un" `mappend` keyText entDef

unKeyExp :: UnboundEntityDef -> Exp
unKeyExp = VarE . unKeyName

backendT :: Type
backendT = VarT backendName

backendName :: Name
backendName = mkName "backend"

-- needs:
--
-- * keyText
--     * entityNameHaskell
--  * fields
--      * fieldHaskell
--
-- keyConName :: EntityNameHS -> [FieldHaskell] -> Name
keyConName :: UnboundEntityDef -> Name
keyConName entDef =
    keyConName'
        (getUnboundEntityNameHS entDef)
        (unboundFieldNameHS <$> unboundEntityFields (entDef))


keyConName' :: EntityNameHS -> [FieldNameHS] -> Name
keyConName' entName entFields = mkName $ T.unpack $ resolveConflict $ keyText' entName
  where
    resolveConflict kn = if conflict then kn `mappend` "'" else kn
    conflict = any (== FieldNameHS "key") entFields

-- keyConExp :: EntityNameHS -> [FieldNameHS] -> Exp
keyConExp :: UnboundEntityDef -> Exp
keyConExp ed = ConE $ keyConName ed

keyText :: UnboundEntityDef -> Text
keyText entDef = unEntityNameHS (getUnboundEntityNameHS entDef) ++ "Key"

keyText' :: EntityNameHS -> Text
keyText' entName = unEntityNameHS entName ++ "Key"

keyFieldName :: MkPersistSettings -> UnboundEntityDef -> FieldNameHS -> Name
keyFieldName mps entDef fieldDef
    | pkNewtype mps entDef =
        unKeyName entDef
    | otherwise =
        mkName $ T.unpack $ lowerFirst (keyText entDef) `mappend` unFieldNameHS fieldDef

filterConName
    :: MkPersistSettings
    -> UnboundEntityDef
    -> UnboundFieldDef
    -> Name
filterConName mps (unboundEntityDef -> entity) field =
    filterConName' mps (entityHaskell entity) (unboundFieldNameHS field)

filterConName'
    :: MkPersistSettings
    -> EntityNameHS
    -> FieldNameHS
    -> Name
filterConName' mps entity field = mkName $ T.unpack name
    where
        name
            | field == FieldNameHS "Id" = entityName ++ fieldName
            | mpsPrefixFields mps       = modifiedName
            | otherwise                 = fieldName

        modifiedName = mpsConstraintLabelModifier mps entityName fieldName
        entityName = unEntityNameHS entity
        fieldName = upperFirst $ unFieldNameHS field

-- | Splice in a list of all 'EntityDef' in scope. This is useful when running
-- 'mkPersist' to ensure that all entity definitions are available for setting
-- foreign keys, and for performing migrations with all entities available.
--
-- 'mkPersist' has the type @MkPersistSettings -> [EntityDef] -> DecsQ@. So, to
-- account for entities defined elsewhere, you'll @mappend $(discoverEntities)@.
--
-- For example,
--
-- @
-- share
--   [ mkPersistWith sqlSettings $(discoverEntities)
--   ]
--   [persistLowerCase| ... |]
-- @
--
-- Likewise, to run migrations with all entity instances in scope, you'd write:
--
-- @
-- migrateAll = migrateModels $(discoverEntities)
-- @
--
-- Note that there is some odd behavior with Template Haskell and splicing
-- groups. If you call 'discoverEntities' in the same module that defines
-- 'PersistEntity' instances, you need to ensure they are in different top-level
-- binding groups. You can write @$(pure [])@ at the top level to do this.
--
-- @
-- -- Foo and Bar both export an instance of PersistEntity
-- import Foo
-- import Bar
--
-- -- Since Foo and Bar are both imported, discoverEntities can find them here.
-- mkPersistWith sqlSettings $(discoverEntities) [persistLowerCase|
--   User
--     name Text
--     age  Int
--   |]
--
-- -- onlyFooBar is defined in the same 'top level group' as the above generated
-- -- instance for User, so it isn't present in this list.
-- onlyFooBar :: [EntityDef]
-- onlyFooBar = $(discoverEntities)
--
-- -- We can manually create a new binding group with this, which splices an
-- -- empty list of declarations in.
-- $(pure [])
--
-- -- fooBarUser is able to see the 'User' instance.
-- fooBarUser :: [EntityDef]
-- fooBarUser = $(discoverEntities)
-- @
--
-- @since 2.13.0.0
discoverEntities :: Q Exp
discoverEntities = do
    instances <- reifyInstances ''PersistEntity [VarT (mkName "a")]
    let
        types =
            mapMaybe getDecType instances
        getDecType dec =
            case dec of
                InstanceD _moverlap _cxt typ _decs ->
                    stripPersistEntity typ
                _ ->
                    Nothing
        stripPersistEntity typ =
            case typ of
                AppT (ConT tyName) t | tyName == ''PersistEntity ->
                    Just t
                _ ->
                    Nothing

    fmap ListE $
        forM types $ \typ -> do
            [e| entityDef (Proxy :: Proxy $(pure typ)) |]

setNull :: NonEmpty UnboundFieldDef -> Bool
setNull (fd :| fds) =
    let
        nullSetting =
            isNull fd
        isNull =
            (NotNullable /=) . isUnboundFieldNullable
    in
        if all ((nullSetting ==) . isNull) fds
        then nullSetting
        else error $
            "foreign key columns must all be nullable or non-nullable"
           ++ show (fmap (unFieldNameHS . unboundFieldNameHS) (fd:fds))
