{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-fields #-}

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
      -- * Various other TH functions
    , mkMigrate
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
    ) where

-- Development Tip: See persistent-template/README.md for advice on seeing generated Template Haskell code
-- It's highly recommended to check the diff between master and your PR's generated code.

import Prelude hiding ((++), take, concat, splitAt, exp)

import Data.Either
import Control.Monad (forM, mzero, filterM, guard, unless)
import Data.Aeson
    ( ToJSON (toJSON), FromJSON (parseJSON), (.=), object
    , Value (Object), (.:), (.:?)
    , eitherDecodeStrict'
    )
import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import Data.Ix (Ix)
import Data.Data (Data)
import Data.Char (toLower, toUpper)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.List (foldl')
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe (isJust, listToMaybe, mapMaybe, fromMaybe)
import Data.Monoid ((<>), mappend, mconcat)
import Data.Proxy (Proxy (Proxy))
import Data.Text (pack, Text, append, unpack, concat, uncons, cons, stripPrefix, stripSuffix)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import GHC.TypeLits
import Instances.TH.Lift ()
    -- Bring `Lift (Map k v)` instance into scope, as well as `Lift Text`
    -- instance on pre-1.2.4 versions of `text`
import Language.Haskell.TH.Lib (conT, varE, varP)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Web.PathPieces (PathPiece(..))
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..))
import qualified Data.Set as Set

import Database.Persist
import Database.Persist.Sql (Migration, PersistFieldSql, SqlBackend, migrate, sqlType)
import Database.Persist.Quasi

-- | This special-cases "type_" and strips out its underscore. When
-- used for JSON serialization and deserialization, it works around
-- <https://github.com/yesodweb/persistent/issues/412>
unHaskellNameForJSON :: HaskellName -> Text
unHaskellNameForJSON = fixTypeUnderscore . unHaskellName
  where
    fixTypeUnderscore = \case
        "type" -> "type_"
        name -> name

-- | Converts a quasi-quoted syntax into a list of entity definitions, to be
-- used as input to the template haskell generation code (mkPersist).
persistWith :: PersistSettings -> QuasiQuoter
persistWith ps = QuasiQuoter
    { quoteExp = parseReferences ps . pack
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
embedEntityDefs :: [EntityDef] -> [EntityDef]
embedEntityDefs = snd . embedEntityDefsMap

embedEntityDefsMap :: [EntityDef] -> (M.Map HaskellName EmbedEntityDef, [EntityDef])
embedEntityDefsMap rawEnts = (embedEntityMap, noCycleEnts)
  where
    noCycleEnts = map breakCycleEnt entsWithEmbeds
    -- every EntityDef could reference each-other (as an EmbedRef)
    -- let Haskell tie the knot
    embedEntityMap = constructEmbedEntityMap entsWithEmbeds
    entsWithEmbeds = map setEmbedEntity rawEnts
    setEmbedEntity ent = ent
        { entityFields = map (setEmbedField (entityHaskell ent) embedEntityMap) $ entityFields ent
        }

    -- self references are already broken
    -- look at every emFieldEmbed to see if it refers to an already seen HaskellName
    -- so start with entityHaskell ent and accumulate embeddedHaskell em
    breakCycleEnt entDef =
        let entName = entityHaskell entDef
         in entDef { entityFields = map (breakCycleField entName) $ entityFields entDef }

    breakCycleField entName f = case f of
        FieldDef { fieldReference = EmbedRef em } ->
            f { fieldReference = EmbedRef $ breakCycleEmbed [entName] em }
        _ ->
            f

    breakCycleEmbed ancestors em =
        em { embeddedFields = breakCycleEmField (emName : ancestors) <$> embeddedFields em
           }
        where
            emName = embeddedHaskell em

    breakCycleEmField ancestors emf = case embeddedHaskell <$> membed of
        Nothing -> emf
        Just embName -> if embName `elem` ancestors
            then emf { emFieldEmbed = Nothing, emFieldCycle = Just embName }
            else emf { emFieldEmbed = breakCycleEmbed ancestors <$> membed }
        where
            membed = emFieldEmbed emf

-- calls parse to Quasi.parse individual entities in isolation
-- afterwards, sets references to other entities
-- | @since 2.5.3
parseReferences :: PersistSettings -> Text -> Q Exp
parseReferences ps s = lift $
    map (mkEntityDefSqlTypeExp embedEntityMap entityMap) noCycleEnts
  where
    (embedEntityMap, noCycleEnts) = embedEntityDefsMap $ parse ps s
    entityMap = constructEntityMap noCycleEnts

stripId :: FieldType -> Maybe Text
stripId (FTTypeCon Nothing t) = stripSuffix "Id" t
stripId _ = Nothing

foreignReference :: FieldDef -> Maybe HaskellName
foreignReference field = case fieldReference field of
    ForeignRef ref _ -> Just ref
    _              -> Nothing


-- fieldSqlType at parse time can be an Exp
-- This helps delay setting fieldSqlType until lift time
data EntityDefSqlTypeExp
    = EntityDefSqlTypeExp EntityDef SqlTypeExp [SqlTypeExp]
    deriving Show

data SqlTypeExp
    = SqlTypeExp FieldType
    | SqlType' SqlType
    deriving Show

instance Lift SqlTypeExp where
    lift (SqlType' t)       = lift t
    lift (SqlTypeExp ftype) = return st
        where
            typ = ftToType ftype
            mtyp = ConT ''Proxy `AppT` typ
            typedNothing = SigE (ConE 'Proxy) mtyp
            st = VarE 'sqlType `AppE` typedNothing
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = unsafeTExpCoerce . lift
#endif

data FieldsSqlTypeExp = FieldsSqlTypeExp [FieldDef] [SqlTypeExp]

instance Lift FieldsSqlTypeExp where
    lift (FieldsSqlTypeExp fields sqlTypeExps) =
        lift $ zipWith FieldSqlTypeExp fields sqlTypeExps
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = unsafeTExpCoerce . lift
#endif

data FieldSqlTypeExp = FieldSqlTypeExp FieldDef SqlTypeExp

instance Lift FieldSqlTypeExp where
    lift (FieldSqlTypeExp FieldDef{..} sqlTypeExp) =
        [|FieldDef fieldHaskell fieldDB fieldType $(lift sqlTypeExp) fieldAttrs fieldStrict fieldReference fieldComments|]
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = unsafeTExpCoerce . lift
#endif

instance Lift EntityDefSqlTypeExp where
    lift (EntityDefSqlTypeExp ent sqlTypeExp sqlTypeExps) =
        [|ent { entityFields = $(lift $ FieldsSqlTypeExp (entityFields ent) sqlTypeExps)
              , entityId = $(lift $ FieldSqlTypeExp (entityId ent) sqlTypeExp)
              }
        |]
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = unsafeTExpCoerce . lift
#endif

deriving instance Lift ReferenceDef

deriving instance Lift EmbedEntityDef

deriving instance Lift EmbedFieldDef

type EmbedEntityMap = M.Map HaskellName EmbedEntityDef

constructEmbedEntityMap :: [EntityDef] -> EmbedEntityMap
constructEmbedEntityMap =
    M.fromList . fmap (\ent -> (entityHaskell ent, toEmbedEntityDef ent))

type EntityMap = M.Map HaskellName EntityDef

constructEntityMap :: [EntityDef] -> EntityMap
constructEntityMap =
    M.fromList . fmap (\ent -> (entityHaskell ent, ent))

data FTTypeConDescr = FTKeyCon deriving Show

mEmbedded :: EmbedEntityMap -> FieldType -> Either (Maybe FTTypeConDescr) EmbedEntityDef
mEmbedded _ (FTTypeCon Just{} _) = Left Nothing
mEmbedded ents (FTTypeCon Nothing n) =
    let name = HaskellName n
     in maybe (Left Nothing) Right $ M.lookup name ents
mEmbedded ents (FTList x) = mEmbedded ents x
mEmbedded ents (FTApp x y) =
    -- Key converts an Record to a RecordId
    -- special casing this is obviously a hack
    -- This problem may not be solvable with the current QuasiQuoted approach though
    if x == FTTypeCon Nothing "Key"
        then Left $ Just FTKeyCon
        else mEmbedded ents y

setEmbedField :: HaskellName -> EmbedEntityMap -> FieldDef -> FieldDef
setEmbedField entName allEntities field = field
    { fieldReference =
        case fieldReference field of
            NoReference ->
                case mEmbedded allEntities (fieldType field) of
                    Left _ ->
                        case stripId $ fieldType field of
                            Nothing -> NoReference
                            Just name ->
                                case M.lookup (HaskellName name) allEntities of
                                    Nothing -> NoReference
                                    Just _ -> ForeignRef (HaskellName name)
                                        -- This can get corrected in mkEntityDefSqlTypeExp
                                        (FTTypeCon (Just "Data.Int") "Int64")
                    Right em ->
                        if embeddedHaskell em /= entName
                             then EmbedRef em
                        else if maybeNullable field
                             then SelfReference
                        else case fieldType field of
                                 FTList _ -> SelfReference
                                 _ -> error $ unpack $ unHaskellName entName <> ": a self reference must be a Maybe"
            existing -> existing
  }

mkEntityDefSqlTypeExp :: EmbedEntityMap -> EntityMap -> EntityDef -> EntityDefSqlTypeExp
mkEntityDefSqlTypeExp emEntities entityMap ent =
    EntityDefSqlTypeExp ent (getSqlType $ entityId ent) (map getSqlType $ entityFields ent)
  where
    getSqlType field =
        maybe
            (defaultSqlTypeExp field)
            (SqlType' . SqlOther)
            (listToMaybe $ mapMaybe (stripPrefix "sqltype=") $ fieldAttrs field)

    -- In the case of embedding, there won't be any datatype created yet.
    -- We just use SqlString, as the data will be serialized to JSON.
    defaultSqlTypeExp field =
        case mEmbedded emEntities ftype of
            Right _ -> SqlType' SqlString
            Left (Just FTKeyCon) -> SqlType' SqlString
            Left Nothing -> case fieldReference field of
                ForeignRef refName ft  -> case M.lookup refName entityMap of
                    Nothing  -> SqlTypeExp ft
                    -- A ForeignRef is blindly set to an Int64 in setEmbedField
                    -- correct that now
                    Just ent' -> case entityPrimary ent' of
                        Nothing -> SqlTypeExp ft
                        Just pdef -> case compositeFields pdef of
                            [] -> error "mkEntityDefSqlTypeExp: no composite fields"
                            [x] -> SqlTypeExp $ fieldType x
                            _ -> SqlType' $ SqlOther "Composite Reference"
                CompositeRef _  -> SqlType' $ SqlOther "Composite Reference"
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
                        FTList _ -> SqlType' SqlString
                        _ -> SqlTypeExp ftype
        where
            ftype = fieldType field

-- | Create data types and appropriate 'PersistEntity' instances for the given
-- 'EntityDef's. Works well with the persist quasi-quoter.
mkPersist :: MkPersistSettings -> [EntityDef] -> Q [Dec]
mkPersist mps ents' = do
    requireExtensions [[TypeFamilies], [GADTs, ExistentialQuantification]]
    x <- fmap Data.Monoid.mconcat $ mapM (persistFieldFromEntity mps) ents
    y <- fmap mconcat $ mapM (mkEntity entityMap mps) ents
    z <- fmap mconcat $ mapM (mkJSON mps) ents
    uniqueKeyInstances <- fmap mconcat $ mapM (mkUniqueKeyInstances mps) ents
    return $ mconcat [x, y, z, uniqueKeyInstances]
  where
    ents = map fixEntityDef ents'
    entityMap = constructEntityMap ents

-- | Implement special preprocessing on EntityDef as necessary for 'mkPersist'.
-- For example, strip out any fields marked as MigrationOnly.
fixEntityDef :: EntityDef -> EntityDef
fixEntityDef ed =
    ed { entityFields = filter keepField $ entityFields ed }
  where
    keepField fd = "MigrationOnly" `notElem` fieldAttrs fd &&
                   "SafeToRemove" `notElem` fieldAttrs fd

-- | Settings to be passed to the 'mkPersist' function.
data MkPersistSettings = MkPersistSettings
    { mpsBackend :: Type
    -- ^ Which database backend we\'re using.
    --
    -- When generating data types, each type is given a generic version- which
    -- works with any backend- and a type synonym for the commonly used
    -- backend. This is where you specify that commonly used backend.
    , mpsGeneric :: Bool
    -- ^ Create generic types that can be used with multiple backends. Good for
    -- reusable code, but makes error messages harder to understand. Default:
    -- False.
    , mpsPrefixFields :: Bool
    -- ^ Prefix field names with the model name. Default: True.
    --
    -- Note: this field is deprecated. Use the mpsFieldLabelModifier  and mpsConstraintLabelModifier instead.
    , mpsFieldLabelModifier :: Text -> Text -> Text
    -- ^ Customise the field accessors and lens names using the entity and field name.
    -- Both arguments are upper cased.
    --
    -- Default: appends entity and field.
    --
    -- Note: this setting is ignored if mpsPrefixFields is set to False.
    -- @since 2.11.0.0
    , mpsConstraintLabelModifier :: Text -> Text -> Text
    -- ^ Customise the Constraint names using the entity and field name. The result
    -- should be a valid haskell type (start with an upper cased letter).
    --
    -- Default: appends entity and field
    --
    -- Note: this setting is ignored if mpsPrefixFields is set to False.
    -- @since 2.11.0.0
    , mpsEntityJSON :: Maybe EntityJSON
    -- ^ Generate @ToJSON@/@FromJSON@ instances for each model types. If it's
    -- @Nothing@, no instances will be generated. Default:
    --
    -- @
    --  Just EntityJSON
    --      { entityToJSON = 'entityIdToJSON
    --      , entityFromJSON = 'entityIdFromJSON
    --      }
    -- @
    , mpsGenerateLenses :: !Bool
    -- ^ Instead of generating normal field accessors, generator lens-style accessors.
    --
    -- Default: False
    --
    -- @since 1.3.1
    , mpsDeriveInstances :: ![Name]
    -- ^ Automatically derive these typeclass instances for all record and key types.
    --
    -- Default: []
    --
    -- @since 2.8.1
    }

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
mkPersistSettings t = MkPersistSettings
    { mpsBackend = t
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
    }

-- | Use the 'SqlPersist' backend.
sqlSettings :: MkPersistSettings
sqlSettings = mkPersistSettings $ ConT ''SqlBackend

recNameNoUnderscore :: MkPersistSettings -> HaskellName -> HaskellName -> Text
recNameNoUnderscore mps dt f
  | mpsPrefixFields mps = lowerFirst $ modifier (unHaskellName dt) (upperFirst ft)
  | otherwise           = lowerFirst ft
  where
    modifier = mpsFieldLabelModifier mps
    ft = unHaskellName f

recName :: MkPersistSettings -> HaskellName -> HaskellName -> Text
recName mps dt f =
    addUnderscore $ recNameNoUnderscore mps dt f
  where
    addUnderscore
        | mpsGenerateLenses mps = ("_" ++)
        | otherwise = id

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

dataTypeDec :: MkPersistSettings -> EntityDef -> Q Dec
dataTypeDec mps t = do
    let entityInstances     = map (mkName . unpack) $ entityDerives t
        additionalInstances = filter (`notElem` entityInstances) $ mpsDeriveInstances mps
        names               = entityInstances <> additionalInstances

    let (stocks, anyclasses) = partitionEithers (map stratFor names)
    let stockDerives = do
            guard (not (null stocks))
            pure (DerivClause (Just StockStrategy) (map ConT stocks))
        anyclassDerives = do
            guard (not (null anyclasses))
            pure (DerivClause (Just AnyclassStrategy) (map ConT anyclasses))
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
        Set.fromList (map mkName
        [ "Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Generic", "Data", "Typeable"
        ] <> [''Eq, ''Ord, ''Show, ''Read, ''Bounded, ''Enum, ''Ix, ''Generic, ''Data, ''Typeable
        ]
        )
    mkCol x fd@FieldDef {..} =
        (mkName $ unpack $ recName mps x fieldHaskell,
         if fieldStrict then isStrict else notStrict,
         maybeIdType mps fd Nothing Nothing
        )
    (nameFinal, paramsFinal)
        | mpsGeneric mps = (nameG, [PlainTV backend])
        | otherwise = (name, [])
    nameG = mkName $ unpack $ unHaskellName (entityHaskell t) ++ "Generic"
    name = mkName $ unpack $ unHaskellName $ entityHaskell t
    cols = map (mkCol $ entityHaskell t) $ entityFields t
    backend = backendName

    constrs
        | entitySum t = map sumCon $ entityFields t
        | otherwise = [RecC name cols]

    sumCon fd = NormalC
        (sumConstrName mps t fd)
        [(notStrict, maybeIdType mps fd Nothing Nothing)]

sumConstrName :: MkPersistSettings -> EntityDef -> FieldDef -> Name
sumConstrName mps t FieldDef {..} = mkName $ unpack name
    where
        name
            | mpsPrefixFields mps = modifiedName ++ "Sum"
            | otherwise           = fieldName ++ "Sum"
        modifiedName = mpsConstraintLabelModifier mps entityName fieldName
        entityName   = unHaskellName $ entityHaskell t
        fieldName    = upperFirst $ unHaskellName fieldHaskell

uniqueTypeDec :: MkPersistSettings -> EntityDef -> Dec
uniqueTypeDec mps t =
#if MIN_VERSION_template_haskell(2,15,0)
    DataInstD [] Nothing
        (AppT (ConT ''Unique) (genericDataType mps (entityHaskell t) backendT))
            Nothing
            (map (mkUnique mps t) $ entityUniques t)
            (derivClause $ entityUniques t)
#else
    DataInstD [] ''Unique
        [genericDataType mps (entityHaskell t) backendT]
            Nothing
            (map (mkUnique mps t) $ entityUniques t)
            (derivClause $ entityUniques t)
#endif
  where
    derivClause [] = []
    derivClause _  = [DerivClause Nothing [ConT ''Show]]

mkUnique :: MkPersistSettings -> EntityDef -> UniqueDef -> Con
mkUnique mps t (UniqueDef (HaskellName constr) _ fields attrs) =
    NormalC (mkName $ unpack constr) types
  where
    types =
      map (go . flip lookup3 (entityFields t) . unHaskellName . fst) fields

    force = "!force" `elem` attrs

    go :: (FieldDef, IsNullable) -> (Strict, Type)
    go (_, Nullable _) | not force = error nullErrMsg
    go (fd, y) = (notStrict, maybeIdType mps fd Nothing (Just y))

    lookup3 :: Text -> [FieldDef] -> (FieldDef, IsNullable)
    lookup3 s [] =
        error $ unpack $ "Column not found: " ++ s ++ " in unique " ++ constr
    lookup3 x (fd@FieldDef {..}:rest)
        | x == unHaskellName fieldHaskell = (fd, nullable fieldAttrs)
        | otherwise = lookup3 x rest

    nullErrMsg =
      mconcat [ "Error:  By default we disallow NULLables in an uniqueness "
              , "constraint.  The semantics of how NULL interacts with those "
              , "constraints is non-trivial:  two NULL values are not "
              , "considered equal for the purposes of an uniqueness "
              , "constraint.  If you understand this feature, it is possible "
              , "to use it your advantage.    *** Use a \"!force\" attribute "
              , "on the end of the line that defines your uniqueness "
              , "constraint in order to disable this check. ***" ]

maybeIdType :: MkPersistSettings
           -> FieldDef
           -> Maybe Name -- ^ backend
           -> Maybe IsNullable
           -> Type
maybeIdType mps fd mbackend mnull = maybeTyp mayNullable idtyp
  where
    mayNullable = case mnull of
        (Just (Nullable ByMaybeAttr)) -> True
        _ -> maybeNullable fd
    idtyp = idType mps fd mbackend

backendDataType :: MkPersistSettings -> Type
backendDataType mps
    | mpsGeneric mps = backendT
    | otherwise = mpsBackend mps

genericDataType :: MkPersistSettings
                -> HaskellName -- ^ entity name
                -> Type -- ^ backend
                -> Type
genericDataType mps (HaskellName typ') backend
    | mpsGeneric mps = ConT (mkName $ unpack $ typ' ++ "Generic") `AppT` backend
    | otherwise = ConT $ mkName $ unpack typ'

idType :: MkPersistSettings -> FieldDef -> Maybe Name -> Type
idType mps fd mbackend =
    case foreignReference fd of
        Just typ ->
            ConT ''Key
            `AppT` genericDataType mps typ (VarT $ fromMaybe backendName mbackend)
        Nothing -> ftToType $ fieldType fd

degen :: [Clause] -> [Clause]
degen [] =
    let err = VarE 'error `AppE` LitE (StringL
                "Degenerate case, should never happen")
     in [normalClause [WildP] err]
degen x = x

mkToPersistFields :: MkPersistSettings -> String -> EntityDef -> Q Dec
mkToPersistFields mps constr ed@EntityDef { entitySum = isSum, entityFields = fields } = do
    clauses <-
        if isSum
            then sequence $ zipWith goSum fields [1..]
            else fmap return go
    return $ FunD 'toPersistFields clauses
  where
    go :: Q Clause
    go = do
        xs <- sequence $ replicate fieldCount $ newName "x"
        let pat = ConP (mkName constr) $ map VarP xs
        sp <- [|SomePersistField|]
        let bod = ListE $ map (AppE sp . VarE) xs
        return $ normalClause [pat] bod

    fieldCount = length fields

    goSum :: FieldDef -> Int -> Q Clause
    goSum fd idx = do
        let name = sumConstrName mps ed fd
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
                [RecP (mkName $ unpack $ unHaskellName constr) []]
                names'

mkUniqueToValues :: [UniqueDef] -> Q Dec
mkUniqueToValues pairs = do
    pairs' <- mapM go pairs
    return $ FunD 'persistUniqueToValues $ degen pairs'
  where
    go :: UniqueDef -> Q Clause
    go (UniqueDef constr _ names _) = do
        xs <- mapM (const $ newName "x") names
        let pat = ConP (mkName $ unpack $ unHaskellName constr) $ map VarP xs
        tpv <- [|toPersistValue|]
        let bod = ListE $ map (AppE tpv . VarE) xs
        return $ normalClause [pat] bod

isNotNull :: PersistValue -> Bool
isNotNull PersistNull = False
isNotNull _ = True

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right r) = Right r
mapLeft f (Left l)  = Left (f l)

mkFromPersistValues :: MkPersistSettings -> EntityDef -> Q [Clause]
mkFromPersistValues _ t@(EntityDef { entitySum = False }) =
    fromValues t "fromPersistValues" entE $ entityFields t
  where
    entE = ConE $ mkName $ unpack entName
    entName = unHaskellName $ entityHaskell t

mkFromPersistValues mps t@(EntityDef { entitySum = True }) = do
    nothing <- [|Left ("Invalid fromPersistValues input: sum type with all nulls. Entity: " `mappend` entName)|]
    clauses <- mkClauses [] $ entityFields t
    return $ clauses `mappend` [normalClause [WildP] nothing]
  where
    entName = unHaskellName $ entityHaskell t
    mkClauses _ [] = return []
    mkClauses before (field:after) = do
        x <- newName "x"
        let null' = ConP 'PersistNull []
            pat = ListP $ mconcat
                [ map (const null') before
                , [VarP x]
                , map (const null') after
                ]
            constr = ConE $ sumConstrName mps t field
        fs <- [|fromPersistValue $(return $ VarE x)|]
        let guard' = NormalG $ VarE 'isNotNull `AppE` VarE x
        let clause = Clause [pat] (GuardedB [(guard', InfixE (Just constr) fmapE (Just fs))]) []
        clauses <- mkClauses (field : before) after
        return $ clause : clauses

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lensPTH :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lensPTH sa sbt afb s = fmap (sbt s) (afb $ sa s)

fmapE :: Exp
fmapE = VarE 'fmap

mkLensClauses :: MkPersistSettings -> EntityDef -> Q [Clause]
mkLensClauses mps t = do
    lens' <- [|lensPTH|]
    getId <- [|entityKey|]
    setId <- [|\(Entity _ value) key -> Entity key value|]
    getVal <- [|entityVal|]
    dot <- [|(.)|]
    keyVar <- newName "key"
    valName <- newName "value"
    xName <- newName "x"
    let idClause = normalClause
            [ConP (keyIdName t) []]
            (lens' `AppE` getId `AppE` setId)
    if entitySum t
        then return $ idClause : map (toSumClause lens' keyVar valName xName) (entityFields t)
        else return $ idClause : map (toClause lens' getVal dot keyVar valName xName) (entityFields t)
  where
    toClause lens' getVal dot keyVar valName xName f = normalClause
        [ConP (filterConName mps t f) []]
        (lens' `AppE` getter `AppE` setter)
      where
        fieldName = mkName $ unpack $ recName mps (entityHaskell t) (fieldHaskell f)
        getter = InfixE (Just $ VarE fieldName) dot (Just getVal)
        setter = LamE
            [ ConP 'Entity [VarP keyVar, VarP valName]
            , VarP xName
            ]
            $ ConE 'Entity `AppE` VarE keyVar `AppE` RecUpdE
                (VarE valName)
                [(fieldName, VarE xName)]

    toSumClause lens' keyVar valName xName f = normalClause
        [ConP (filterConName mps t f) []]
        (lens' `AppE` getter `AppE` setter)
      where
        emptyMatch = Match WildP (NormalB $ VarE 'error `AppE` LitE (StringL "Tried to use fieldLens on a Sum type")) []
        getter = LamE
            [ ConP 'Entity [WildP, VarP valName]
            ] $ CaseE (VarE valName)
            $ Match (ConP (sumConstrName mps t f) [VarP xName]) (NormalB $ VarE xName) []

            -- FIXME It would be nice if the types expressed that the Field is
            -- a sum type and therefore could result in Maybe.
            : if length (entityFields t) > 1 then [emptyMatch] else []
        setter = LamE
            [ ConP 'Entity [VarP keyVar, WildP]
            , VarP xName
            ]
            $ ConE 'Entity `AppE` VarE keyVar `AppE` (ConE (sumConstrName mps t f) `AppE` VarE xName)

-- | declare the key type and associated instances
-- @'PathPiece'@, @'ToHttpApiData'@ and @'FromHttpApiData'@ instances are only generated for a Key with one field
mkKeyTypeDec :: MkPersistSettings -> EntityDef -> Q (Dec, [Dec])
mkKeyTypeDec mps t = do
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

#if MIN_VERSION_template_haskell(2,15,0)
    cxti <- mapM conT i
    let kd = if useNewtype
               then NewtypeInstD [] Nothing (AppT (ConT k) recordType) Nothing dec [DerivClause (Just NewtypeStrategy) cxti]
               else DataInstD    [] Nothing (AppT (ConT k) recordType) Nothing [dec] [DerivClause (Just StockStrategy) cxti]
#else
    cxti <- mapM conT i
    let kd = if useNewtype
               then NewtypeInstD [] k [recordType] Nothing dec [DerivClause (Just NewtypeStrategy) cxti]
               else DataInstD    [] k [recordType] Nothing [dec] [DerivClause (Just StockStrategy) cxti]
#endif
    return (kd, instDecs)
  where
    keyConE = keyConExp t
    unKeyE = unKeyExp t
    dec = RecC (keyConName t) (keyFields mps t)
    k = ''Key
    recordType = genericDataType mps (entityHaskell t) backendT
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

      instances <- do
        alwaysInstances <-
          [d|deriving newtype instance Show (BackendKey $(pure backendT)) => Show (Key $(pure recordType))
             deriving newtype instance Read (BackendKey $(pure backendT)) => Read (Key $(pure recordType))
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

        if customKeyType then return alwaysInstances
          else fmap (alwaysInstances `mappend`) backendKeyGenericI
      return instances

    useNewtype = pkNewtype mps t
    customKeyType = not (defaultIdType t) || not useNewtype || isJust (entityPrimary t)

    supplement :: [Name] -> [Name]
    supplement names = names <> (filter (`notElem` names) $ mpsDeriveInstances mps)

keyIdName :: EntityDef -> Name
keyIdName = mkName . unpack . keyIdText

keyIdText :: EntityDef -> Text
keyIdText t = unHaskellName (entityHaskell t) `mappend` "Id"

unKeyName :: EntityDef -> Name
unKeyName t = mkName $ "un" `mappend` keyString t

unKeyExp :: EntityDef -> Exp
unKeyExp = VarE . unKeyName

backendT :: Type
backendT = VarT backendName

backendName :: Name
backendName = mkName "backend"

keyConName :: EntityDef -> Name
keyConName t = mkName $ resolveConflict $ keyString t
  where
    resolveConflict kn = if conflict then kn `mappend` "'" else kn
    conflict = any ((== HaskellName "key") . fieldHaskell) $ entityFields t

keyConExp :: EntityDef -> Exp
keyConExp = ConE . keyConName

keyString :: EntityDef -> String
keyString = unpack . keyText

keyText :: EntityDef -> Text
keyText t = unHaskellName (entityHaskell t) ++ "Key"

pkNewtype :: MkPersistSettings -> EntityDef -> Bool
pkNewtype mps t = length (keyFields mps t) < 2

defaultIdType :: EntityDef -> Bool
defaultIdType t = fieldType (entityId t) == FTTypeCon Nothing (keyIdText t)

keyFields :: MkPersistSettings -> EntityDef -> [(Name, Strict, Type)]
keyFields mps t = case entityPrimary t of
  Just pdef -> map primaryKeyVar (compositeFields pdef)
  Nothing   -> if defaultIdType t
    then [idKeyVar backendKeyType]
    else [idKeyVar $ ftToType $ fieldType $ entityId t]
  where
    backendKeyType
        | mpsGeneric mps = ConT ''BackendKey `AppT` backendT
        | otherwise      = ConT ''BackendKey `AppT` mpsBackend mps
    idKeyVar ft = (unKeyName t, notStrict, ft)
    primaryKeyVar fd = ( keyFieldName mps t fd
                       , notStrict
                       , ftToType $ fieldType fd
                       )

keyFieldName :: MkPersistSettings -> EntityDef -> FieldDef -> Name
keyFieldName mps t fd
  | pkNewtype mps t = unKeyName t
  | otherwise = mkName $ unpack $ lowerFirst (keyText t) `mappend` unHaskellName (fieldHaskell fd)

mkKeyToValues :: MkPersistSettings -> EntityDef -> Q Dec
mkKeyToValues mps t = do
    (p, e) <- case entityPrimary t of
        Nothing  ->
          ([],) <$> [|(:[]) . toPersistValue . $(return $ unKeyExp t)|]
        Just pdef ->
          return $ toValuesPrimary pdef
    return $ FunD 'keyToValues $ return $ normalClause p e
  where
    toValuesPrimary pdef =
      ( [VarP recordName]
      , ListE $ map (\fd -> VarE 'toPersistValue `AppE` (VarE (keyFieldName mps t fd) `AppE` VarE recordName)) $ compositeFields pdef
      )
    recordName = mkName "record"

normalClause :: [Pat] -> Exp -> Clause
normalClause p e = Clause p (NormalB e) []

mkKeyFromValues :: MkPersistSettings -> EntityDef -> Q Dec
mkKeyFromValues _mps t = do
    clauses <- case entityPrimary t of
        Nothing  -> do
            e <- [|fmap $(return keyConE) . fromPersistValue . headNote|]
            return [normalClause [] e]
        Just pdef ->
            fromValues t "keyFromValues" keyConE (compositeFields pdef)
    return $ FunD 'keyFromValues clauses
  where
    keyConE = keyConExp t

headNote :: [PersistValue] -> PersistValue
headNote = \case
  [x] -> x
  xs -> error $ "mkKeyFromValues: expected a list of one element, got: " `mappend` show xs

fromValues :: EntityDef -> Text -> Exp -> [FieldDef] -> Q [Clause]
fromValues t funName conE fields = do
    x <- newName "x"
    let funMsg = entityText t `mappend` ": " `mappend` funName `mappend` " failed on: "
    patternMatchFailure <- [|Left $ mappend funMsg (pack $ show $(return $ VarE x))|]
    suc <- patternSuccess
    return [ suc, normalClause [VarP x] patternMatchFailure ]
  where
    tableName = unDBName (entityDB t)
    patternSuccess =
        case fields of
            [] -> do
                rightE <- [|Right|]
                return $ normalClause [ListP []] (rightE `AppE` conE)
            _ -> do
                x1 <- newName "x1"
                restNames <- mapM (\i -> newName $ "x" `mappend` show i) [2..length fields]
                (fpv1:mkPersistValues) <- mapM mkPersistValue fields
                app1E <- [|(<$>)|]
                let conApp = infixFromPersistValue app1E fpv1 conE x1
                applyE <- [|(<*>)|]
                let applyFromPersistValue = infixFromPersistValue applyE

                return $ normalClause
                    [ListP $ map VarP (x1:restNames)]
                    (foldl' (\exp (name, fpv) -> applyFromPersistValue fpv exp name) conApp (zip restNames mkPersistValues))

    infixFromPersistValue applyE fpv exp name =
        UInfixE exp applyE (fpv `AppE` VarE name)

    mkPersistValue field =
        let fieldName = (unHaskellName (fieldHaskell field))
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

mkEntity :: EntityMap -> MkPersistSettings -> EntityDef -> Q [Dec]
mkEntity entityMap mps t = do
    t' <- liftAndFixKeys entityMap t
    let nameT = unHaskellName entName
    let nameS = unpack nameT
    let clazz = ConT ''PersistEntity `AppT` genDataType
    tpf <- mkToPersistFields mps nameS t
    fpv <- mkFromPersistValues mps t
    utv <- mkUniqueToValues $ entityUniques t
    puk <- mkUniqueKeys t
    fkc <- mapM (mkForeignKeysComposite mps t) $ entityForeigns t

    let primaryField = entityId t

    fields <- mapM (mkField mps t) $ primaryField : entityFields t
    toFieldNames <- mkToFieldNames $ entityUniques t

    (keyTypeDec, keyInstanceDecs) <- mkKeyTypeDec mps t
    keyToValues' <- mkKeyToValues mps t
    keyFromValues' <- mkKeyFromValues mps t

    let addSyn -- FIXME maybe remove this
            | mpsGeneric mps = (:) $
                TySynD (mkName nameS) [] $
                    genericDataType mps entName $ mpsBackend mps
            | otherwise = id

    lensClauses <- mkLensClauses mps t

    lenses <- mkLenses mps t
    let instanceConstraint = if not (mpsGeneric mps) then [] else
          [mkClassP ''PersistStore [backendT]]

    [keyFromRecordM'] <-
        case entityPrimary t of
            Just prim -> do
                recordName <- newName "record"
                let fields = map fieldHaskell (compositeFields prim)
                    keyCon = keyConName t
                    keyFields' =
                        map (mkName . T.unpack . recName mps entName . fieldHaskell)
                            (compositeFields prim)
                    constr =
                        foldl'
                            AppE
                            (ConE keyCon)
                            (map
                                (\n ->
                                    VarE n `AppE` VarE recordName
                                )
                                keyFields'
                            )
                    keyFromRec = varP 'keyFromRecordM
                [d|$(keyFromRec) = Just ( \ $(varP recordName) -> $(pure constr)) |]

            Nothing ->
                [d|$(varP 'keyFromRecordM) = Nothing|]

    dtd <- dataTypeDec mps t
    return $ addSyn $
       dtd : mconcat fkc `mappend`
      ([ TySynD (keyIdName t) [] $
            ConT ''Key `AppT` ConT (mkName nameS)
      , instanceD instanceConstraint clazz
        [ uniqueTypeDec mps t
        , keyTypeDec
        , keyToValues'
        , keyFromValues'
        , keyFromRecordM'
        , FunD 'entityDef [normalClause [WildP] t']
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
            (map fst fields)
            []
#else
        , DataInstD
            []
            ''EntityField
            [ genDataType
            , VarT $ mkName "typ"
            ]
            Nothing
            (map fst fields)
            []
#endif
        , FunD 'persistFieldDef (map snd fields)
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
        , FunD 'persistIdField [normalClause [] (ConE $ keyIdName t)]
        , FunD 'fieldLens lensClauses
        ]
      ] `mappend` lenses) `mappend` keyInstanceDecs
  where
    genDataType = genericDataType mps entName backendT
    entName = entityHaskell t

mkUniqueKeyInstances :: MkPersistSettings -> EntityDef -> Q [Dec]
mkUniqueKeyInstances mps t = do
    requirePersistentExtensions
    case entityUniques t of
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
                write <- [t|PersistStoreWrite $(pure (VarT $ mkName "backend")) |]
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

    genDataType = genericDataType mps (entityHaskell t) backendT


entityText :: EntityDef -> Text
entityText = unHaskellName . entityHaskell

mkLenses :: MkPersistSettings -> EntityDef -> Q [Dec]
mkLenses mps _ | not (mpsGenerateLenses mps) = return []
mkLenses _ ent | entitySum ent = return []
mkLenses mps ent = fmap mconcat $ forM (entityFields ent) $ \field -> do
    let lensName' = recNameNoUnderscore mps (entityHaskell ent) (fieldHaskell field)
        lensName = mkName $ unpack lensName'
        fieldName = mkName $ unpack $ "_" ++ lensName'
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
        aT = maybeIdType mps field (Just backend1) Nothing
        bT = maybeIdType mps field (Just backend2) Nothing
        mkST backend = genericDataType mps (entityHaskell ent) (VarT backend)
        sT = mkST backend1
        tT = mkST backend2
        t1 `arrow` t2 = ArrowT `AppT` t1 `AppT` t2
        vars = PlainTV fT
             : (if mpsGeneric mps then [PlainTV backend1{-, PlainTV backend2-}] else [])
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

mkForeignKeysComposite :: MkPersistSettings -> EntityDef -> ForeignDef -> Q [Dec]
mkForeignKeysComposite mps t ForeignDef {..} = do
    let fieldName f = mkName $ unpack $ recName mps (entityHaskell t) f
    let fname = fieldName foreignConstraintNameHaskell
    let reftableString = unpack $ unHaskellName foreignRefTableHaskell
    let reftableKeyName = mkName $ reftableString `mappend` "Key"
    let tablename = mkName $ unpack $ entityText t
    recordName <- newName "record"

    let fldsE = map (\((foreignName, _),_) -> VarE (fieldName foreignName)
                  `AppE` VarE recordName) foreignFields
    let mkKeyE = foldl' AppE (maybeExp foreignNullable $ ConE reftableKeyName) fldsE
    let fn = FunD fname [normalClause [VarP recordName] mkKeyE]

    let t2 = maybeTyp foreignNullable $ ConT ''Key `AppT` ConT (mkName reftableString)
    let sig = SigD fname $ (ArrowT `AppT` (ConT tablename)) `AppT` t2
    return [sig, fn]

maybeExp :: Bool -> Exp -> Exp
maybeExp may exp | may = fmapE `AppE` exp
                 | otherwise = exp
maybeTyp :: Bool -> Type -> Type
maybeTyp may typ | may = ConT ''Maybe `AppT` typ
                 | otherwise = typ



entityToPersistValueHelper :: (PersistEntity record) => record -> PersistValue
entityToPersistValueHelper entity = PersistMap $ zip columnNames fieldsAsPersistValues
    where
        columnNames = map (unHaskellName . fieldHaskell) (entityFields (entityDef (Just entity)))
        fieldsAsPersistValues = map toPersistValue $ toPersistFields entity

entityFromPersistValueHelper :: (PersistEntity record)
                             => [String] -- ^ Column names, as '[String]' to avoid extra calls to "pack" in the generated code
                             -> PersistValue
                             -> Either Text record
entityFromPersistValueHelper columnNames pv = do
    (persistMap :: [(T.Text, PersistValue)]) <- getPersistMap pv

    let columnMap = HM.fromList persistMap
        lookupPersistValueByColumnName :: String -> PersistValue
        lookupPersistValueByColumnName columnName =
            fromMaybe PersistNull (HM.lookup (pack columnName) columnMap)

    fromPersistValues $ map lookupPersistValueByColumnName columnNames

-- | Produce code similar to the following:
--
-- @
--   instance PersistEntity e => PersistField e where
--      toPersistValue = entityToPersistValueHelper
--      fromPersistValue = entityFromPersistValueHelper ["col1", "col2"]
--      sqlType _ = SqlString
-- @
persistFieldFromEntity :: MkPersistSettings -> EntityDef -> Q [Dec]
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
    typ = genericDataType mps (entityHaskell entDef) backendT
    entFields = entityFields entDef
    columnNames = map (unpack . unHaskellName . fieldHaskell) entFields

-- | Apply the given list of functions to the same @EntityDef@s.
--
-- This function is useful for cases such as:
--
-- >>> share [mkSave "myDefs", mkPersist sqlSettings] [persistLowerCase|...|]
share :: [[EntityDef] -> Q [Dec]] -> [EntityDef] -> Q [Dec]
share fs x = mconcat <$> mapM ($ x) fs

-- | Save the @EntityDef@s passed in under the given name.
mkSave :: String -> [EntityDef] -> Q [Dec]
mkSave name' defs' = do
    let name = mkName name'
    defs <- lift defs'
    return [ SigD name $ ListT `AppT` ConT ''EntityDef
           , FunD name [normalClause [] defs]
           ]

data Dep = Dep
    { depTarget :: HaskellName
    , depSourceTable :: HaskellName
    , depSourceField :: HaskellName
    , depSourceNull  :: IsNullable
    }

-- | Generate a 'DeleteCascade' instance for the given @EntityDef@s.
mkDeleteCascade :: MkPersistSettings -> [EntityDef] -> Q [Dec]
mkDeleteCascade mps defs = do
    let deps = concatMap getDeps defs
    mapM (go deps) defs
  where
    getDeps :: EntityDef -> [Dep]
    getDeps def =
        concatMap getDeps' $ entityFields $ fixEntityDef def
      where
        getDeps' :: FieldDef -> [Dep]
        getDeps' field@FieldDef {..} =
            case foreignReference field of
                Just name ->
                     return Dep
                        { depTarget = name
                        , depSourceTable = entityHaskell def
                        , depSourceField = fieldHaskell
                        , depSourceNull  = nullable fieldAttrs
                        }
                Nothing -> []
    go :: [Dep] -> EntityDef -> Q Dec
    go allDeps EntityDef{entityHaskell = name} = do
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
            stmts = map mkStmt deps `mappend`
                    [NoBindS $ del `AppE` VarE key]

        let entityT = genericDataType mps name backendT

        return $
            instanceD
            [ mkClassP ''PersistQuery [backendT]
            , mkEqualP (ConT ''PersistEntityBackend `AppT` entityT) (ConT ''BaseBackend `AppT` backendT)
            ]
            (ConT ''DeleteCascade `AppT` entityT `AppT` backendT)
            [ FunD 'deleteCascade
                [normalClause [VarP key] (DoE stmts)]
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
    -> [EntityDef]
    -> Q [Dec]
mkEntityDefList entityList entityDefs = do
    let entityListName = mkName entityList
    edefs <- fmap ListE
        . forM entityDefs
        $ \(EntityDef { entityHaskell = HaskellName haskellName }) ->
            let entityType = conT (mkName (T.unpack haskellName))
             in [|entityDef (Proxy :: Proxy $(entityType))|]
    typ <- [t|[EntityDef]|]
    pure
        [ SigD entityListName typ
        , ValD (VarP entityListName) (NormalB edefs) []
        ]

mkUniqueKeys :: EntityDef -> Q Dec
mkUniqueKeys def | entitySum def =
    return $ FunD 'persistUniqueKeys [normalClause [WildP] (ListE [])]
mkUniqueKeys def = do
    c <- clause
    return $ FunD 'persistUniqueKeys [c]
  where
    clause = do
        xs <- forM (entityFields def) $ \fd -> do
            let x = fieldHaskell fd
            x' <- newName $ '_' : unpack (unHaskellName x)
            return (x, x')
        let pcs = map (go xs) $ entityUniques def
        let pat = ConP
                (mkName $ unpack $ unHaskellName $ entityHaskell def)
                (map (VarP . snd) xs)
        return $ normalClause [pat] (ListE pcs)

    go :: [(HaskellName, Name)] -> UniqueDef -> Exp
    go xs (UniqueDef name _ cols _) =
        foldl' (go' xs) (ConE (mkName $ unpack $ unHaskellName name)) (map fst cols)

    go' :: [(HaskellName, Name)] -> Exp -> HaskellName -> Exp
    go' xs front col =
        let Just col' = lookup col xs
         in front `AppE` VarE col'

sqlTypeFunD :: Exp -> Dec
sqlTypeFunD st = FunD 'sqlType
                [ normalClause [WildP] st ]

typeInstanceD :: Name
              -> Bool -- ^ include PersistStore backend constraint
              -> Type -> [Dec] -> Dec
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

-- | Creates a single function to perform all migrations for the entities
-- defined here. One thing to be aware of is dependencies: if you have entities
-- with foreign references, make sure to place those definitions after the
-- entities they reference.
mkMigrate :: String -> [EntityDef] -> Q [Dec]
mkMigrate fun allDefs = do
    body' <- body
    return
        [ SigD (mkName fun) typ
        , FunD (mkName fun) [normalClause [] body']
        ]
  where
    defs = filter isMigrated allDefs
    isMigrated def = "no-migrate" `notElem` entityAttrs def
    typ = ConT ''Migration
    entityMap = constructEntityMap allDefs
    body :: Q Exp
    body =
        case defs of
            [] -> [|return ()|]
            _  -> do
              defsName <- newName "defs"
              defsStmt <- do
                defs' <- mapM (liftAndFixKeys entityMap) defs
                let defsExp = ListE defs'
                return $ LetS [ValD (VarP defsName) (NormalB defsExp) []]
              stmts <- mapM (toStmt $ VarE defsName) defs
              return (DoE $ defsStmt : stmts)
    toStmt :: Exp -> EntityDef -> Q Stmt
    toStmt defsExp ed = do
        u <- liftAndFixKeys entityMap ed
        m <- [|migrate|]
        return $ NoBindS $ m `AppE` defsExp `AppE` u

liftAndFixKeys :: EntityMap -> EntityDef -> Q Exp
liftAndFixKeys entityMap EntityDef{..} =
    [|EntityDef
        entityHaskell
        entityDB
        entityId
        entityAttrs
        $(ListE <$> mapM (liftAndFixKey entityMap) entityFields)
        entityUniques
        entityForeigns
        entityDerives
        entityExtra
        entitySum
        entityComments
    |]

liftAndFixKey :: EntityMap -> FieldDef -> Q Exp
liftAndFixKey entityMap (FieldDef a b c sqlTyp e f fieldRef mcomments) =
    [|FieldDef a b c $(sqlTyp') e f fieldRef' mcomments|]
  where
    (fieldRef', sqlTyp') = fromMaybe (fieldRef, lift sqlTyp) $
      case fieldRef of
        ForeignRef refName _ft -> case M.lookup refName entityMap of
          Nothing -> Nothing
          Just ent ->
            case fieldReference $ entityId ent of
              fr@(ForeignRef _Name ft) -> Just (fr, lift $ SqlTypeExp ft)
              _ -> Nothing
        _ -> Nothing

deriving instance Lift EntityDef

deriving instance Lift FieldDef

deriving instance Lift UniqueDef

deriving instance Lift CompositeDef

deriving instance Lift ForeignDef

-- |
--
-- @since 2.8.3.0
deriving instance Lift FieldCascade

-- |
--
-- @since 2.8.3.0
deriving instance Lift CascadeAction

deriving instance Lift HaskellName
deriving instance Lift DBName
deriving instance Lift FieldType

deriving instance Lift PersistFilter

deriving instance Lift PersistUpdate

deriving instance Lift SqlType

-- Ent
--   fieldName FieldType
--
-- forall . typ ~ FieldType => EntFieldName
--
-- EntFieldName = FieldDef ....
mkField :: MkPersistSettings -> EntityDef -> FieldDef -> Q (Con, Clause)
mkField mps et cd = do
    let con = ForallC
                []
                [mkEqualP (VarT $ mkName "typ") $ maybeIdType mps cd Nothing Nothing]
                $ NormalC name []
    bod <- lift cd
    let cla = normalClause
                [ConP name []]
                bod
    return (con, cla)
  where
    name = filterConName mps et cd

maybeNullable :: FieldDef -> Bool
maybeNullable fd = nullable (fieldAttrs fd) == Nullable ByMaybeAttr

filterConName :: MkPersistSettings
              -> EntityDef
              -> FieldDef
              -> Name
filterConName mps entity field = filterConName' mps (entityHaskell entity) (fieldHaskell field)

filterConName' :: MkPersistSettings
               -> HaskellName -- ^ table
               -> HaskellName -- ^ field
               -> Name
filterConName' mps entity field = mkName $ unpack name
    where
        name
            | field == HaskellName "Id" = entityName ++ fieldName
            | mpsPrefixFields mps       = modifiedName
            | otherwise                 = fieldName
        modifiedName = mpsConstraintLabelModifier mps entityName fieldName
        entityName   = unHaskellName entity
        fieldName    = upperFirst $ unHaskellName field

ftToType :: FieldType -> Type
ftToType (FTTypeCon Nothing t) = ConT $ mkName $ unpack t
-- This type is generated from the Quasi-Quoter.
-- Adding this special case avoids users needing to import Data.Int
ftToType (FTTypeCon (Just "Data.Int") "Int64") = ConT ''Int64
ftToType (FTTypeCon (Just m) t) = ConT $ mkName $ unpack $ concat [m, ".", t]
ftToType (FTApp x y) = ftToType x `AppT` ftToType y
ftToType (FTList x) = ListT `AppT` ftToType x

infixr 5 ++
(++) :: Text -> Text -> Text
(++) = append

mkJSON :: MkPersistSettings -> EntityDef -> Q [Dec]
mkJSON _ def | ("json" `notElem` entityAttrs def) = return []
mkJSON mps def = do
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

    xs <- mapM (newName . unpack . unHaskellNameForJSON . fieldHaskell)
        $ entityFields def

    let conName = mkName $ unpack $ unHaskellName $ entityHaskell def
        typ = genericDataType mps (entityHaskell def) backendT
        toJSONI = typeInstanceD ''ToJSON (mpsGeneric mps) typ [toJSON']
        toJSON' = FunD 'toJSON $ return $ normalClause
            [ConP conName $ map VarP xs]
            (objectE `AppE` ListE pairs)
        pairs = zipWith toPair (entityFields def) xs
        toPair f x = InfixE
            (Just (packE `AppE` LitE (StringL $ unpack $ unHaskellName $ fieldHaskell f)))
            dotEqualE
            (Just $ VarE x)
        fromJSONI = typeInstanceD ''FromJSON (mpsGeneric mps) typ [parseJSON']
        parseJSON' = FunD 'parseJSON
            [ normalClause [ConP 'Object [VarP obj]]
                (foldl'
                    (\x y -> InfixE (Just x) apE' (Just y))
                    (pureE `AppE` ConE conName)
                    pulls
                )
            , normalClause [WildP] mzeroE
            ]
        pulls = map toPull $ entityFields def
        toPull f = InfixE
            (Just $ VarE obj)
            (if maybeNullable f then dotColonQE else dotColonE)
            (Just $ AppE packE $ LitE $ StringL $ unpack $ unHaskellName $ fieldHaskell f)
    case mpsEntityJSON mps of
        Nothing -> return [toJSONI, fromJSONI]
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

-- entityUpdates :: EntityDef -> [(HaskellName, FieldType, IsNullable, PersistUpdate)]
-- entityUpdates =
--     concatMap go . entityFields
--   where
--     go FieldDef {..} = map (\a -> (fieldHaskell, fieldType, nullable fieldAttrs, a)) [minBound..maxBound]

-- mkToUpdate :: String -> [(String, PersistUpdate)] -> Q Dec
-- mkToUpdate name pairs = do
--     pairs' <- mapM go pairs
--     return $ FunD (mkName name) $ degen pairs'
--   where
--     go (constr, pu) = do
--         pu' <- lift pu
--         return $ normalClause [RecP (mkName constr) []] pu'


-- mkToFieldName :: String -> [(String, String)] -> Dec
-- mkToFieldName func pairs =
--         FunD (mkName func) $ degen $ map go pairs
--   where
--     go (constr, name) =
--         normalClause [RecP (mkName constr) []] (LitE $ StringL name)

-- mkToValue :: String -> [String] -> Dec
-- mkToValue func = FunD (mkName func) . degen . map go
--   where
--     go constr =
--         let x = mkName "x"
--          in normalClause [ConP (mkName constr) [VarP x]]
--                    (VarE 'toPersistValue `AppE` VarE x)

-- | Check that all of Persistent's required extensions are enabled, or else fail compilation
--
-- This function should be called before any code that depends on one of the required extensions being enabled.
requirePersistentExtensions :: Q ()
requirePersistentExtensions = requireExtensions requiredExtensions
  where
    requiredExtensions = map pure
        [ DerivingStrategies
        , GeneralizedNewtypeDeriving
        , StandaloneDeriving
        , UndecidableInstances
        ]

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
                    , List.intercalate "\n" (map show extensions)
                    , "\n\nPlease enable the extensions by copy/pasting these lines into the top of your file:\n\n"
                    , List.intercalate "\n" (map extensionToPragma extensions)
                    ]

  where
    extensionToPragma ext = "{-# LANGUAGE " <> show ext <> " #-}"
