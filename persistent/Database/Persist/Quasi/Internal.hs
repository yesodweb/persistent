{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This @Internal@ module may have breaking changes that will not be reflected
-- in major version bumps. Please use "Database.Persist.Quasi" instead. If you
-- need something in this module, please file an issue on GitHub.
--
-- @since 2.13.0.0
module Database.Persist.Quasi.Internal
    ( parse
    , PersistSettings (..)
    , upperCaseSettings
    , lowerCaseSettings
    , Attribute (..)
    , SourceLoc (..)
    , sourceLocFromTHLoc
    , parseFieldType
    , takeColsEx
    , CumulativeParseResult
    , renderErrors
    , parserWarningMessage

      -- * UnboundEntityDef
    , UnboundEntityDef (..)
    , getUnboundEntityNameHS
    , unbindEntityDef
    , getUnboundFieldDefs
    , UnboundForeignDef (..)
    , getSqlNameOr
    , UnboundFieldDef (..)
    , UnboundCompositeDef (..)
    , UnboundIdDef (..)
    , unbindFieldDef
    , isUnboundFieldNullable
    , unboundIdDefToFieldDef
    , PrimarySpec (..)
    , mkAutoIdField'
    , UnboundForeignFieldList (..)
    , ForeignFieldReference (..)
    , mkKeyConType
    , isHaskellUnboundField
    , FieldTypeLit (..)
    ) where

import Prelude hiding (lines)

import Control.Applicative (Alternative ((<|>)))
import Control.Monad
import Data.Char (isDigit, isLower, isSpace, isUpper, toLower)
import Data.Foldable (toList)
import Data.List (find, foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.EntityDef.Internal
import Database.Persist.Quasi.Internal.ModelParser
import Database.Persist.Quasi.PersistSettings
import Database.Persist.Quasi.PersistSettings.Internal
    ( psIdName
    , psStrictFields
    , psToDBName
    , psToFKName
    )
import Database.Persist.Types
import Database.Persist.Types.Base
import Language.Haskell.TH.Syntax (Lift, Loc (..))
import qualified Text.Read as R

data ParseState a = PSDone | PSFail String | PSSuccess a Text deriving (Show)

parseFieldType :: Text -> Either String FieldType
parseFieldType t0 =
    case parseApplyFT t0 of
        PSSuccess ft t'
            | T.all isSpace t' -> Right ft
        PSFail err -> Left $ "PSFail " ++ err
        other -> Left $ show other
  where
    parseApplyFT :: Text -> ParseState FieldType
    parseApplyFT t =
        case goMany id t of
            PSSuccess (ft : fts) t' -> PSSuccess (foldl' FTApp ft fts) t'
            PSSuccess [] _ -> PSFail "empty"
            PSFail err -> PSFail err
            PSDone -> PSDone

    parseEnclosed
        :: Char -> (FieldType -> FieldType) -> Text -> ParseState FieldType
    parseEnclosed end ftMod t =
        let
            (a, b) = T.break (== end) t
         in
            case parseApplyFT a of
                PSSuccess ft t' -> case (T.dropWhile isSpace t', T.uncons b) of
                    ("", Just (c, t'')) | c == end -> PSSuccess (ftMod ft) (t'' `Data.Monoid.mappend` t')
                    (x, y) -> PSFail $ show (b, x, y)
                x -> PSFail $ show x

    parse1 :: Text -> ParseState FieldType
    parse1 t = fromMaybe (PSFail (show t)) $ do
        case T.uncons t of
            Nothing -> pure PSDone
            Just (x, xs) ->
                parseSpace x xs
                    <|> parseParenEnclosed x xs
                    <|> parseList x xs
                    <|> parseNumericLit x xs
                    <|> parseTextLit x xs
                    <|> parseTypeCon x xs

    parseSpace :: Char -> Text -> Maybe (ParseState FieldType)
    parseSpace c t = do
        guard (isSpace c)
        pure $ parse1 (T.dropWhile isSpace t)

    parseParenEnclosed c t = do
        guard (c == '(')
        pure $ parseEnclosed ')' id t

    parseList c t = do
        guard (c == '[')
        pure $ parseEnclosed ']' FTList t

    parseTextLit :: Char -> Text -> Maybe (ParseState FieldType)
    parseTextLit c t = do
        guard (c == '"')
        let
            (a, b) = T.break (== '"') t
            lit = FTLit (TextTypeLit a)
        pure $ PSSuccess lit (T.drop 1 b)

    parseNumericLit :: Char -> Text -> Maybe (ParseState FieldType)
    parseNumericLit c t = do
        guard (isDigit c && T.all isDigit t)
        let
            (a, b) = breakAtNextSpace t
        lit <- FTLit . IntTypeLit <$> readMaybe (T.cons c a)
        pure $ PSSuccess lit b

    parseTypeCon c t = do
        guard (isUpper c || c == '\'')
        let
            (a, b) = breakAtNextSpace t
        pure $ PSSuccess (parseFieldTypePiece c a) b

    goMany :: ([FieldType] -> a) -> Text -> ParseState a
    goMany front t =
        case parse1 t of
            PSSuccess x t' -> goMany (front . (x :)) t'
            PSFail err -> PSFail err
            PSDone -> PSSuccess (front []) t

breakAtNextSpace :: Text -> (Text, Text)
breakAtNextSpace =
    T.break isSpace

parseFieldTypePiece :: Char -> Text -> FieldType
parseFieldTypePiece fstChar rest =
    case fstChar of
        '\'' ->
            FTTypePromoted rest
        _ ->
            let
                t = T.cons fstChar rest
             in
                case T.breakOnEnd "." t of
                    (_, "") -> FTTypeCon Nothing t
                    ("", _) -> FTTypeCon Nothing t
                    (a, b) -> FTTypeCon (Just $ T.init a) b

sourceLocFromTHLoc :: Loc -> SourceLoc
sourceLocFromTHLoc Loc{loc_filename = filename, loc_start = start} =
    SourceLoc
        { locFile = T.pack filename
        , locStartLine = fst start
        , locStartCol = snd start
        }

-- | Parses a quasi-quoted syntax into a list of entity definitions.
parse
    :: PersistSettings
    -> [(Maybe SourceLoc, Text)]
    -> CumulativeParseResult [UnboundEntityDef]
parse ps chunks = toCumulativeParseResult $ map parseChunk chunks
  where
    parseChunk :: (Maybe SourceLoc, Text) -> ParseResult [UnboundEntityDef]
    parseChunk (mSourceLoc, source) =
        (fmap . fmap) (mkUnboundEntityDef ps) <$> parseSource ps mSourceLoc source

entityNamesFromParsedDef
    :: PersistSettings -> ParsedEntityDef -> (EntityNameHS, EntityNameDB)
entityNamesFromParsedDef ps parsedEntDef = (entNameHS, entNameDB)
  where
    entNameHS =
        parsedEntityDefEntityName parsedEntDef

    entNameDB =
        EntityNameDB $
            getDbName
                ps
                (unEntityNameHS entNameHS)
                (attributeContent <$> parsedEntityDefEntityAttributes parsedEntDef)

-- | This type represents an @Id@ declaration in the QuasiQuoted syntax.
--
-- > Id
--
-- This uses the implied settings, and is equivalent to omitting the @Id@
-- statement entirely.
--
-- > Id Text
--
-- This will set the field type of the ID to be 'Text'.
--
-- > Id Text sql=foo_id
--
-- This will set the field type of the Id to be 'Text' and the SQL DB name to be @foo_id@.
--
-- > Id FooId
--
-- This results in a shared primary key - the @FooId@ refers to a @Foo@ table.
--
-- > Id FooId OnDelete Cascade
--
-- You can set a cascade behavior on an ID column.
--
-- @since 2.13.0.0
data UnboundIdDef = UnboundIdDef
    { unboundIdEntityName :: EntityNameHS
    , unboundIdDBName :: !FieldNameDB
    , unboundIdAttrs :: [FieldAttr]
    , unboundIdCascade :: FieldCascade
    , unboundIdType :: Maybe FieldType
    }
    deriving (Eq, Ord, Show, Lift)

-- | An 'EntityDef' produced by the QuasiQuoter. It contains information that
-- the QuasiQuoter is capable of knowing about the entities. It is inherently
-- unfinished, though - there are many other @Unbound@ datatypes that also
-- contain partial information.
--
-- The 'unboundEntityDef' is not complete or reliable - to know which fields are
-- safe to use, consult the parsing code.
--
-- This type was completely internal until 2.13.0.0, when it was exposed as part
-- of the "Database.Persist.Quasi.Internal" module.
--
-- TODO: refactor this so we can expose it for consumers.
--
-- @since 2.13.0.0
data UnboundEntityDef
    = UnboundEntityDef
    { unboundForeignDefs :: [UnboundForeignDef]
    -- ^ A list of foreign definitions on the parsed entity.
    --
    -- @since 2.13.0.0
    , unboundPrimarySpec :: PrimarySpec
    -- ^ The specification for the primary key of the unbound entity.
    --
    -- @since 2.13.0.0
    , unboundEntityDef :: EntityDef
    -- ^ The incomplete and partial 'EntityDef' that we're defining. We re-use
    -- the type here to prevent duplication, but several of the fields are unset
    -- and left to defaults.
    --
    -- @since 2.13.0.0
    , unboundEntityFields :: [UnboundFieldDef]
    -- ^ The list of fields for the entity. We're not capable of knowing
    -- information like "is this a reference?" or "what's the underlying type of
    -- the field?" yet, so we defer those to the Template Haskell execution.
    --
    -- @since 2.13.0.0
    , unboundEntityDefSpan :: Maybe SourceSpan
    -- ^ The source code span of this entity in the models file.
    --
    -- @since 2.15.0.0
    }
    deriving (Eq, Ord, Show, Lift)

-- | Convert an 'EntityDef' into an 'UnboundEntityDef'. This "forgets"
-- information about the 'EntityDef', but it is all kept present on the
-- 'unboundEntityDef' field if necessary.
--
-- @since 2.13.0.0
unbindEntityDef :: EntityDef -> UnboundEntityDef
unbindEntityDef ed =
    UnboundEntityDef
        { unboundForeignDefs =
            map unbindForeignDef (entityForeigns ed)
        , unboundPrimarySpec =
            case entityId ed of
                EntityIdField fd ->
                    SurrogateKey (unbindIdDef (entityHaskell ed) fd)
                EntityIdNaturalKey cd ->
                    NaturalKey (unbindCompositeDef cd)
        , unboundEntityDef =
            ed
        , unboundEntityFields =
            map unbindFieldDef (entityFields ed)
        , unboundEntityDefSpan = entitySpan ed
        }

-- | Returns the @['UnboundFieldDef']@ for an 'UnboundEntityDef'. This returns
-- all fields defined on the entity.
--
-- @since 2.13.0.0
getUnboundFieldDefs :: UnboundEntityDef -> [UnboundFieldDef]
getUnboundFieldDefs = unboundEntityFields

-- | This function forgets information about the 'CompositeDef' so that it can
-- be remembered through Template Haskell.
--
-- @since 2.13.0.0
unbindCompositeDef :: CompositeDef -> UnboundCompositeDef
unbindCompositeDef cd =
    UnboundCompositeDef
        { unboundCompositeCols =
            fmap fieldHaskell (compositeFields cd)
        , unboundCompositeAttrs =
            compositeAttrs cd
        }

-- | A representation of a database column, with everything that can be known at
-- parse time.
--
-- @since 2.13.0.0
data UnboundFieldDef
    = UnboundFieldDef
    { unboundFieldNameHS :: FieldNameHS
    -- ^  The Haskell name of the field. This is parsed directly from the
    -- definition, and is used to generate the Haskell record field and the
    -- 'EntityField' definition.
    --
    -- @since 2.13.0.0
    , unboundFieldNameDB :: FieldNameDB
    -- ^ The database name of the field. By default, this is determined by the
    -- 'PersistSettings' record at parse time. You can customize this with
    -- a @sql=@ attribute:
    --
    -- @
    --     name Text  sql=foo_name
    -- @
    --
    -- @since 2.13.0.0
    , unboundFieldAttrs :: [FieldAttr]
    -- ^ The attributes present on the field. For rules on parsing and utility,
    -- see the comments on the datatype.
    --
    -- @since 2.13.0.0
    , unboundFieldStrict :: Bool
    -- ^ Whether or not the field should be strict in the generated Haskell
    -- code.
    --
    -- @since 2.13.0.0
    , unboundFieldType :: FieldType
    -- ^ The type of the field, as far as is known at parse time.
    --
    -- The TemplateHaskell code will reconstruct a 'Type' out of this, but the
    -- names will be imported as-is.
    --
    -- @since 2.13.0.0
    , unboundFieldCascade :: FieldCascade
    -- ^ We parse if there's a 'FieldCascade' on the field. If the field is not
    -- a reference, this information is ignored.
    --
    -- @
    -- Post
    --    user UserId OnDeleteCascade
    -- @
    --
    -- @since 2.13.0.0
    , unboundFieldGenerated :: Maybe Text
    -- ^ Contains an expression to generate the column. If this is present, then
    -- the column will not be written to the database, but generated by the
    -- expression every time.
    --
    -- @
    -- Item
    --     subtotal Int
    --     taxRate  Rational
    --     total    Int      generated="subtotal * tax_rate"
    -- @
    --
    -- @since 2.13.0.0
    , unboundFieldComments :: Maybe Text
    -- ^ Any comments present on the field. Documentation comments use
    -- a Haskell-like syntax, and must be present before the field in question.
    --
    -- @
    -- Post
    --     -- | This is the blog post title.
    --     title Text
    --     -- | You can have multi-line comments.
    --     -- | But each line must have the pipe character.
    --     author UserId
    -- @
    --
    -- @since 2.13.0.0
    }
    deriving (Eq, Ord, Show, Lift)

-- | Forget innformation about a 'FieldDef' so it can beused as an
-- 'UnboundFieldDef'.
--
-- @since 2.13.0.0
unbindFieldDef :: FieldDef -> UnboundFieldDef
unbindFieldDef fd =
    UnboundFieldDef
        { unboundFieldNameHS =
            fieldHaskell fd
        , unboundFieldNameDB =
            fieldDB fd
        , unboundFieldAttrs =
            fieldAttrs fd
        , unboundFieldType =
            fieldType fd
        , unboundFieldStrict =
            fieldStrict fd
        , unboundFieldCascade =
            fieldCascade fd
        , unboundFieldComments =
            fieldComments fd
        , unboundFieldGenerated =
            fieldGenerated fd
        }

isUnboundFieldNullable :: UnboundFieldDef -> IsNullable
isUnboundFieldNullable =
    fieldAttrsContainsNullable . unboundFieldAttrs

-- | The specification for how an entity's primary key should be formed.
--
-- Persistent requires that every table have a primary key. By default, an
-- implied ID is assigned, based on the 'mpsImplicitIdDef' field on
-- 'MkPersistSettings'. Because we can't access that type at parse-time, we
-- defer that decision until later.
--
-- @since 2.13.0.0
data PrimarySpec
    = -- | A 'NaturalKey' contains columns that are defined on the datatype
      -- itself. This is defined using the @Primary@ keyword and given a non-empty
      -- list of columns.
      --
      -- @
      -- User
      --     name    Text
      --     email   Text
      --
      --     Primary name email
      -- @
      --
      -- A natural key may also contain only a single column. A natural key with
      -- multiple columns is called a 'composite key'.
      --
      -- @since 2.13.0.0
      NaturalKey UnboundCompositeDef
    | -- | A surrogate key is not part of the domain model for a database table.
      -- You can specify a custom surro
      --
      -- You can specify a custom surrogate key using the @Id@ syntax.
      --
      -- @
      -- User
      --     Id    Text
      --     name  Text
      -- @
      --
      -- Note that you must provide a @default=@ expression when using this in
      -- order to use 'insert' or related functions. The 'insertKey' function can
      -- be used instead, as it allows you to specify a key directly. Fixing this
      -- issue is tracked in #1247 on GitHub.
      --
      -- @since 2.13.0.0
      SurrogateKey UnboundIdDef
    | -- | The default key for the entity using the settings in
      -- 'MkPersistSettings'.
      --
      -- This is implicit - a table without an @Id@ or @Primary@ declaration will
      -- have a 'DefaultKey'.
      --
      -- @since 2.13.0.0
      DefaultKey FieldNameDB
    deriving (Eq, Ord, Show, Lift)

-- | Construct an entity definition.
mkUnboundEntityDef
    :: PersistSettings
    -> ParsedEntityDef
    -- ^ parsed entity definition
    -> UnboundEntityDef
mkUnboundEntityDef ps parsedEntDef =
    UnboundEntityDef
        { unboundForeignDefs =
            entityConstraintDefsForeignsList entityConstraintDefs
        , unboundPrimarySpec =
            case (idField, primaryComposite) of
                (Just{}, Just{}) ->
                    error "Specified both an ID field and a Primary field"
                (Just a, Nothing) ->
                    if unboundIdType a == Just (mkKeyConType (unboundIdEntityName a))
                        then
                            DefaultKey (FieldNameDB $ psIdName ps)
                        else
                            SurrogateKey a
                (Nothing, Just a) ->
                    NaturalKey a
                (Nothing, Nothing) ->
                    DefaultKey (FieldNameDB $ psIdName ps)
        , unboundEntityFields =
            cols
        , unboundEntityDefSpan = parsedEntityDefSpan parsedEntDef
        , unboundEntityDef =
            EntityDef
                { entityHaskell = entNameHS
                , entityDB = entNameDB
                , -- idField is the user-specified Id
                  -- otherwise useAutoIdField
                  -- but, adjust it if the user specified a Primary
                  entityId =
                    EntityIdField $
                        maybe autoIdField (unboundIdDefToFieldDef (defaultIdName ps) entNameHS) idField
                , entityAttrs =
                    attributeContent <$> parsedEntityDefEntityAttributes parsedEntDef
                , entityFields = []
                , entityUniques = entityConstraintDefsUniquesList entityConstraintDefs
                , entityForeigns = []
                , entityDerives = concat $ mapMaybe takeDerives (textFields ++ textDirectives)
                , entityExtra = parsedEntityDefExtras parsedEntDef
                , entitySum = parsedEntityDefIsSum parsedEntDef
                , entityComments =
                    case parsedEntityDefComments parsedEntDef of
                        [] -> Nothing
                        comments -> Just (T.unlines comments)
                , entitySpan = parsedEntityDefSpan parsedEntDef
                }
        }
  where
    (entNameHS, entNameDB) =
        entityNamesFromParsedDef ps parsedEntDef

    fields = parsedEntityDefFields parsedEntDef
    directives = parsedEntityDefDirectives parsedEntDef

    cols :: [UnboundFieldDef]
    cols = foldMap (toList . commentedField ps) fields

    textFields :: [[Text]]
    textFields = entityFieldContent . fst <$> fields

    textDirectives :: [[Text]]
    textDirectives = directiveContent . fst <$> directives

    entityConstraintDefs =
        foldMap
            (maybe mempty (takeConstraint ps entNameHS cols) . NEL.nonEmpty)
            (textFields ++ textDirectives)

    idField =
        case entityConstraintDefsIdField entityConstraintDefs of
            SetMoreThanOnce -> error "expected only one Id declaration per entity"
            SetOnce a -> Just a
            NotSet -> Nothing

    primaryComposite =
        case entityConstraintDefsPrimaryComposite entityConstraintDefs of
            SetMoreThanOnce -> error "expected only one Primary declaration per entity"
            SetOnce a -> Just a
            NotSet -> Nothing

    commentedField
        :: PersistSettings
        -> (EntityField, Maybe Text)
        -> Maybe UnboundFieldDef
    commentedField s (field, mCommentText) = do
        unb <- takeColsEx s (entityFieldContent field)
        pure $ unb{unboundFieldComments = mCommentText}

    autoIdField :: FieldDef
    autoIdField =
        mkAutoIdField ps entNameHS idSqlType

    idSqlType :: SqlType
    idSqlType =
        maybe SqlInt64 (const $ SqlOther "Primary Key") primaryComposite

defaultIdName :: PersistSettings -> FieldNameDB
defaultIdName = FieldNameDB . psIdName

-- | Convert an 'UnboundIdDef' into a 'FieldDef' suitable for use in the
-- 'EntityIdField' constructor.
--
-- @since 2.13.0.0
unboundIdDefToFieldDef
    :: FieldNameDB
    -> EntityNameHS
    -> UnboundIdDef
    -> FieldDef
unboundIdDefToFieldDef dbField entNameHS uid =
    FieldDef
        { fieldHaskell =
            FieldNameHS "Id"
        , fieldDB =
            getSqlNameOr dbField (unboundIdAttrs uid)
        , fieldType =
            fromMaybe (mkKeyConType entNameHS) $ unboundIdType uid
        , fieldSqlType =
            SqlOther "SqlType unset for Id"
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

-- | Convert an 'EntityNameHS' into 'FieldType' that will get parsed into the ID
-- type for the entity.
--
-- @
-- >>> mkKeyConType (EntityNameHS "Hello)
-- FTTypeCon Nothing "HelloId"
-- @
--
-- @since 2.13.0.0
mkKeyConType :: EntityNameHS -> FieldType
mkKeyConType entNameHs =
    FTTypeCon Nothing (keyConName entNameHs)

-- | Assuming that the provided 'FieldDef' is an ID field, this converts it into
-- an 'UnboundIdDef'.
--
-- @since 2.13.0.0
unbindIdDef :: EntityNameHS -> FieldDef -> UnboundIdDef
unbindIdDef entityName fd =
    UnboundIdDef
        { unboundIdEntityName =
            entityName
        , unboundIdDBName =
            fieldDB fd
        , unboundIdAttrs =
            fieldAttrs fd
        , unboundIdCascade =
            fieldCascade fd
        , unboundIdType =
            Just $ fieldType fd
        }

mkAutoIdField :: PersistSettings -> EntityNameHS -> SqlType -> FieldDef
mkAutoIdField ps =
    mkAutoIdField' (FieldNameDB $ psIdName ps)

mkAutoIdField' :: FieldNameDB -> EntityNameHS -> SqlType -> FieldDef
mkAutoIdField' dbName entName idSqlType =
    FieldDef
        { fieldHaskell = FieldNameHS "Id"
        , fieldDB = dbName
        , fieldType = FTTypeCon Nothing $ keyConName entName
        , fieldSqlType = idSqlType
        , fieldReference = NoReference
        , fieldAttrs = []
        , fieldStrict = True
        , fieldComments = Nothing
        , fieldCascade = noCascade
        , fieldGenerated = Nothing
        , fieldIsImplicitIdColumn = True
        }

keyConName :: EntityNameHS -> Text
keyConName entName = unEntityNameHS entName `mappend` "Id"

isCapitalizedText :: Text -> Bool
isCapitalizedText t =
    not (T.null t) && isUpper (T.head t)

takeColsEx :: PersistSettings -> [Text] -> Maybe UnboundFieldDef
takeColsEx =
    takeCols
        (\ft perr -> error $ "Invalid field type " ++ show ft ++ " " ++ perr)

takeCols
    :: (Text -> String -> Maybe UnboundFieldDef)
    -> PersistSettings
    -> [Text]
    -> Maybe UnboundFieldDef
takeCols _ _ ("deriving" : _) = Nothing
takeCols onErr ps (n' : typ : rest')
    | not (T.null n) && isLower (T.head n) =
        case parseFieldType typ of
            Left err -> onErr typ err
            Right ft ->
                Just
                    UnboundFieldDef
                        { unboundFieldNameHS =
                            FieldNameHS n
                        , unboundFieldNameDB =
                            getDbName' ps n fieldAttrs_
                        , unboundFieldType =
                            ft
                        , unboundFieldAttrs =
                            fieldAttrs_
                        , unboundFieldStrict =
                            fromMaybe (psStrictFields ps) mstrict
                        , unboundFieldComments =
                            Nothing
                        , unboundFieldCascade =
                            cascade_
                        , unboundFieldGenerated =
                            generated_
                        }
  where
    fieldAttrs_ = parseFieldAttrs attrs_
    generated_ = parseGenerated attrs_
    (cascade_, attrs_) = parseCascade rest'
    (mstrict, n)
        | Just x <- T.stripPrefix "!" n' = (Just True, x)
        | Just x <- T.stripPrefix "~" n' = (Just False, x)
        | otherwise = (Nothing, n')
takeCols _ _ _ = Nothing

parseGenerated :: [Text] -> Maybe Text
parseGenerated = foldl' (\acc x -> acc <|> T.stripPrefix "generated=" x) Nothing

getDbName :: PersistSettings -> Text -> [Text] -> Text
getDbName ps n =
    fromMaybe (psToDBName ps n) . listToMaybe . mapMaybe (T.stripPrefix "sql=")

getDbName' :: PersistSettings -> Text -> [FieldAttr] -> FieldNameDB
getDbName' ps n =
    getSqlNameOr (FieldNameDB $ psToDBName ps n)

getSqlNameOr
    :: FieldNameDB
    -> [FieldAttr]
    -> FieldNameDB
getSqlNameOr def =
    maybe def FieldNameDB . findAttrSql
  where
    findAttrSql =
        listToMaybe . mapMaybe isAttrSql
    isAttrSql attr =
        case attr of
            FieldAttrSql t ->
                Just t
            _ ->
                Nothing

data SetOnceAtMost a
    = NotSet
    | SetOnce a
    | SetMoreThanOnce

instance Semigroup (SetOnceAtMost a) where
    a <> b =
        case (a, b) of
            (_, NotSet) -> a
            (NotSet, _) -> b
            (SetOnce _, SetOnce _) -> SetMoreThanOnce
            _ -> a

instance Monoid (SetOnceAtMost a) where
    mempty =
        NotSet

data EntityConstraintDefs = EntityConstraintDefs
    { entityConstraintDefsIdField :: SetOnceAtMost UnboundIdDef
    , entityConstraintDefsPrimaryComposite :: SetOnceAtMost UnboundCompositeDef
    , entityConstraintDefsUniques :: Maybe (NonEmpty UniqueDef)
    , entityConstraintDefsForeigns :: Maybe (NonEmpty UnboundForeignDef)
    }

instance Semigroup EntityConstraintDefs where
    a <> b =
        EntityConstraintDefs
            { entityConstraintDefsIdField =
                entityConstraintDefsIdField a <> entityConstraintDefsIdField b
            , entityConstraintDefsPrimaryComposite =
                entityConstraintDefsPrimaryComposite a <> entityConstraintDefsPrimaryComposite b
            , entityConstraintDefsUniques =
                entityConstraintDefsUniques a <> entityConstraintDefsUniques b
            , entityConstraintDefsForeigns =
                entityConstraintDefsForeigns a <> entityConstraintDefsForeigns b
            }

instance Monoid EntityConstraintDefs where
    mempty =
        EntityConstraintDefs mempty mempty Nothing Nothing

entityConstraintDefsUniquesList :: EntityConstraintDefs -> [UniqueDef]
entityConstraintDefsUniquesList = foldMap NEL.toList . entityConstraintDefsUniques

entityConstraintDefsForeignsList :: EntityConstraintDefs -> [UnboundForeignDef]
entityConstraintDefsForeignsList = foldMap NEL.toList . entityConstraintDefsForeigns

takeConstraint
    :: PersistSettings
    -> EntityNameHS
    -> [UnboundFieldDef]
    -> NonEmpty Text
    -> EntityConstraintDefs
takeConstraint ps entityName defs (n :| rest) =
    case n of
        "Unique" ->
            mempty
                { entityConstraintDefsUniques =
                    pure <$> takeUniq ps (unEntityNameHS entityName) defs rest
                }
        "Foreign" ->
            mempty
                { entityConstraintDefsForeigns =
                    Just $ pure (takeForeign ps entityName rest)
                }
        "Primary" ->
            let
                unboundComposite =
                    takeComposite (unboundFieldNameHS <$> defs) rest
             in
                mempty
                    { entityConstraintDefsPrimaryComposite =
                        SetOnce unboundComposite
                    , entityConstraintDefsUniques =
                        Just $ pure $ compositeToUniqueDef entityName defs unboundComposite
                    }
        "Id" ->
            mempty
                { entityConstraintDefsIdField =
                    SetOnce (takeId ps entityName rest)
                }
        _
            | isCapitalizedText n ->
                mempty
                    { entityConstraintDefsUniques =
                        pure <$> takeUniq ps "" defs (n : rest)
                    }
        _ ->
            mempty

-- TODO: this is hacky (the double takeCols, the setFieldDef stuff, and setIdName.
-- need to re-work takeCols function
takeId :: PersistSettings -> EntityNameHS -> [Text] -> UnboundIdDef
takeId ps entityName texts =
    UnboundIdDef
        { unboundIdDBName =
            FieldNameDB $ psIdName ps
        , unboundIdEntityName =
            entityName
        , unboundIdCascade =
            cascade_
        , unboundIdAttrs =
            parseFieldAttrs attrs_
        , unboundIdType =
            typ
        }
  where
    typ =
        case texts of
            [] ->
                Nothing
            (t : _) ->
                case parseFieldType t of
                    Left _ ->
                        Nothing
                    Right ft ->
                        Just ft
    (cascade_, attrs_) = parseCascade texts

-- | A definition for a composite primary key.
--
-- @since.2.13.0.0
data UnboundCompositeDef = UnboundCompositeDef
    { unboundCompositeCols :: NonEmpty FieldNameHS
    -- ^ The field names for the primary key.
    --
    -- @since 2.13.0.0
    , unboundCompositeAttrs :: [Attr]
    -- ^ A list of attributes defined on the primary key. This is anything that
    -- occurs after a @!@ character.
    --
    -- @since 2.13.0.0
    }
    deriving (Eq, Ord, Show, Lift)

compositeToUniqueDef
    :: EntityNameHS -> [UnboundFieldDef] -> UnboundCompositeDef -> UniqueDef
compositeToUniqueDef entityName fields UnboundCompositeDef{..} =
    UniqueDef
        { uniqueHaskell =
            ConstraintNameHS (unEntityNameHS entityName <> "PrimaryKey")
        , uniqueDBName =
            ConstraintNameDB "primary_key"
        , uniqueFields =
            fmap (\hsName -> (hsName, getDbNameFor hsName)) unboundCompositeCols
        , uniqueAttrs =
            unboundCompositeAttrs
        }
  where
    getDbNameFor hsName =
        case mapMaybe (matchHsName hsName) fields of
            [] ->
                error "Unable to find `hsName` in fields"
            (a : _) ->
                a
    matchHsName hsName UnboundFieldDef{..} = do
        guard $ unboundFieldNameHS == hsName
        pure unboundFieldNameDB

takeComposite
    :: [FieldNameHS]
    -> [Text]
    -> UnboundCompositeDef
takeComposite fields pkcols =
    UnboundCompositeDef
        { unboundCompositeCols =
            fmap (getDef fields) neCols
        , unboundCompositeAttrs =
            attrs
        }
  where
    neCols =
        case NEL.nonEmpty cols of
            Nothing ->
                error "No fields provided for primary key"
            Just xs ->
                xs
    (cols, attrs) = break ("!" `T.isPrefixOf`) pkcols
    getDef [] t = error $ "Unknown column in primary key constraint: " ++ show t
    getDef (d : ds) t
        | d == FieldNameHS t =
            -- TODO: check for nullability in later step
            -- if nullable (fieldAttrs d) /= NotNullable
            --     then error $ "primary key column cannot be nullable: " ++ show t ++ show fields
            d
        | otherwise =
            getDef ds t

-- Unique UppercaseConstraintName list of lowercasefields terminated
-- by ! or sql= such that a unique constraint can look like:
-- `UniqueTestNull fieldA fieldB sql=ConstraintNameInDatabase !force`
-- Here using sql= sets the name of the constraint.
takeUniq
    :: PersistSettings
    -> Text
    -> [UnboundFieldDef]
    -> [Text]
    -> Maybe UniqueDef
takeUniq ps tableName defs (n : rest)
    | isCapitalizedText n = do
        fields <- mfields
        pure
            UniqueDef
                { uniqueHaskell =
                    ConstraintNameHS n
                , uniqueDBName =
                    dbName
                , uniqueFields =
                    fmap (\a -> (FieldNameHS a, getDBName defs a)) fields
                , uniqueAttrs =
                    attrs
                }
  where
    isAttr a =
        "!" `T.isPrefixOf` a
    isSqlName a =
        "sql=" `T.isPrefixOf` a
    isNonField a =
        isAttr a || isSqlName a
    (fieldsList, nonFields) =
        break isNonField rest
    mfields =
        NEL.nonEmpty fieldsList

    attrs = filter isAttr nonFields

    usualDbName =
        ConstraintNameDB $ psToDBName ps (tableName `T.append` n)
    sqlName :: Maybe ConstraintNameDB
    sqlName =
        case find isSqlName nonFields of
            Nothing ->
                Nothing
            (Just t) ->
                case drop 1 $ T.splitOn "=" t of
                    (x : _) -> Just (ConstraintNameDB x)
                    _ -> Nothing
    dbName = fromMaybe usualDbName sqlName

    getDBName [] t = error $ T.unpack (unknownUniqueColumnError t defs n)
    getDBName (d : ds) t
        | unboundFieldNameHS d == FieldNameHS t =
            unboundFieldNameDB d
        | otherwise =
            getDBName ds t
takeUniq _ tableName _ xs =
    error $
        "invalid unique constraint on table["
            ++ show tableName
            ++ "] expecting an uppercase constraint name xs="
            ++ show xs

unknownUniqueColumnError :: Text -> [UnboundFieldDef] -> Text -> Text
unknownUniqueColumnError t defs n =
    "Unknown column in \""
        <> n
        <> "\" constraint: \""
        <> t
        <> "\""
        <> " possible fields: "
        <> T.pack (show (toFieldName <$> defs))
  where
    toFieldName :: UnboundFieldDef -> Text
    toFieldName fd =
        unFieldNameHS (unboundFieldNameHS fd)

-- | Define an explicit foreign key reference.
--
-- @
-- User
--     name Text
--     email Text
--
--     Primary name email
--
-- Dog
--     ownerName Text
--     ownerEmail Text
--
--     Foreign User fk_dog_user ownerName ownerEmail
-- @
--
-- @since 2.13.0.0
data UnboundForeignDef
    = UnboundForeignDef
    { unboundForeignFields :: UnboundForeignFieldList
    -- ^ Fields in the source entity.
    --
    -- @since 2.13.0.0
    , unboundForeignDef :: ForeignDef
    -- ^ The 'ForeignDef' which needs information filled in.
    --
    -- This value is unreliable. See the parsing code to see what data is filled
    -- in here.
    --
    -- @since 2.13.0.0
    }
    deriving (Eq, Ord, Show, Lift)

-- | A list of fields present on the foreign reference.
data UnboundForeignFieldList
    = -- | If no @References@ keyword is supplied, then it is assumed that you are
      -- referring to the @Primary@ key or @Id@ of the target entity.
      --
      -- @since 2.13.0.0
      FieldListImpliedId (NonEmpty FieldNameHS)
    | -- | You can specify the exact columns you're referring to here, if they
      -- aren't part of a primary key. Most databases expect a unique index on the
      -- columns you refer to, but Persistent doesnt' check that.
      --
      -- @
      -- User
      --     Id           UUID default="uuid_generate_v1mc()"
      --     name         Text
      --
      --     UniqueName name
      --
      -- Dog
      --     ownerName    Text
      --
      --     Foreign User fk_dog_user ownerName References name
      -- @
      --
      -- @since 2.13.0.0
      FieldListHasReferences (NonEmpty ForeignFieldReference)
    deriving (Eq, Ord, Show, Lift)

-- | A pairing of the 'FieldNameHS' for the source table to the 'FieldNameHS'
-- for the target table.
--
-- @since 2.13.0.0
data ForeignFieldReference
    = ForeignFieldReference
    { ffrSourceField :: FieldNameHS
    -- ^ The column on the source table.
    --
    -- @since 2.13.0.0
    , ffrTargetField :: FieldNameHS
    -- ^ The column on the target table.
    --
    -- @since 2.13.0.0
    }
    deriving (Eq, Ord, Show, Lift)

unbindForeignDef :: ForeignDef -> UnboundForeignDef
unbindForeignDef fd =
    UnboundForeignDef
        { unboundForeignFields =
            FieldListHasReferences $ NEL.fromList $ fmap mk (foreignFields fd)
        , unboundForeignDef =
            fd
        }
  where
    mk ((fH, _), (pH, _)) =
        ForeignFieldReference
            { ffrSourceField = fH
            , ffrTargetField = pH
            }

mkUnboundForeignFieldList
    :: [Text]
    -> [Text]
    -> Either String UnboundForeignFieldList
mkUnboundForeignFieldList (fmap FieldNameHS -> source) (fmap FieldNameHS -> target) =
    case NEL.nonEmpty source of
        Nothing ->
            Left "No fields on foreign reference."
        Just sources ->
            case NEL.nonEmpty target of
                Nothing ->
                    Right $ FieldListImpliedId sources
                Just targets ->
                    if length targets /= length sources
                        then
                            Left "Target and source length differe on foreign reference."
                        else
                            Right $
                                FieldListHasReferences $
                                    NEL.zipWith ForeignFieldReference sources targets

takeForeign
    :: PersistSettings
    -> EntityNameHS
    -> [Text]
    -> UnboundForeignDef
takeForeign ps entityName = takeRefTable
  where
    errorPrefix :: String
    errorPrefix =
        "invalid foreign key constraint on table["
            ++ show (unEntityNameHS entityName)
            ++ "] "

    takeRefTable :: [Text] -> UnboundForeignDef
    takeRefTable [] =
        error $ errorPrefix ++ " expecting foreign table name"
    takeRefTable (refTableName : restLine) =
        go restLine Nothing Nothing
      where
        go :: [Text] -> Maybe CascadeAction -> Maybe CascadeAction -> UnboundForeignDef
        go (constraintNameText : rest) onDelete onUpdate
            | not (T.null constraintNameText) && isLower (T.head constraintNameText) =
                UnboundForeignDef
                    { unboundForeignFields =
                        either error id $ mkUnboundForeignFieldList foreignFields parentFields
                    , unboundForeignDef =
                        ForeignDef
                            { foreignRefTableHaskell =
                                EntityNameHS refTableName
                            , foreignRefTableDBName =
                                EntityNameDB $ psToDBName ps refTableName
                            , foreignConstraintNameHaskell =
                                constraintName
                            , foreignConstraintNameDBName =
                                toFKConstraintNameDB ps entityName constraintName
                            , foreignFieldCascade =
                                FieldCascade
                                    { fcOnDelete = onDelete
                                    , fcOnUpdate = onUpdate
                                    }
                            , foreignAttrs =
                                attrs
                            , foreignFields =
                                []
                            , foreignNullable =
                                False
                            , foreignToPrimary =
                                null parentFields
                            }
                    }
          where
            constraintName =
                ConstraintNameHS constraintNameText

            (fields, attrs) =
                break ("!" `T.isPrefixOf`) rest
            (foreignFields, parentFields) =
                case break (== "References") fields of
                    (ffs, []) ->
                        (ffs, [])
                    (ffs, _ : pfs) ->
                        case (length ffs, length pfs) of
                            (flen, plen)
                                | flen == plen ->
                                    (ffs, pfs)
                            (flen, plen) ->
                                error $
                                    errorPrefix
                                        ++ concat
                                            [ "Found "
                                            , show flen
                                            , " foreign fields but "
                                            , show plen
                                            , " parent fields"
                                            ]
        go ((parseCascadeAction CascadeDelete -> Just cascadingAction) : rest) onDelete' onUpdate =
            case onDelete' of
                Nothing ->
                    go rest (Just cascadingAction) onUpdate
                Just _ ->
                    error $ errorPrefix ++ "found more than one OnDelete actions"
        go ((parseCascadeAction CascadeUpdate -> Just cascadingAction) : rest) onDelete onUpdate' =
            case onUpdate' of
                Nothing ->
                    go rest onDelete (Just cascadingAction)
                Just _ ->
                    error $ errorPrefix ++ "found more than one OnUpdate actions"
        go xs _ _ =
            error $
                errorPrefix
                    ++ "expecting a lower case constraint name or a cascading action xs="
                    ++ show xs

toFKConstraintNameDB
    :: PersistSettings -> EntityNameHS -> ConstraintNameHS -> ConstraintNameDB
toFKConstraintNameDB ps entityName constraintName =
    ConstraintNameDB $ psToDBName ps (psToFKName ps entityName constraintName)
data CascadePrefix = CascadeUpdate | CascadeDelete

parseCascade :: [Text] -> (FieldCascade, [Text])
parseCascade allTokens =
    go [] Nothing Nothing allTokens
  where
    go acc mupd mdel tokens_ =
        case tokens_ of
            [] ->
                ( FieldCascade
                    { fcOnDelete = mdel
                    , fcOnUpdate = mupd
                    }
                , acc
                )
            this : rest ->
                case parseCascadeAction CascadeUpdate this of
                    Just cascUpd ->
                        case mupd of
                            Nothing ->
                                go acc (Just cascUpd) mdel rest
                            Just _ ->
                                nope "found more than one OnUpdate action"
                    Nothing ->
                        case parseCascadeAction CascadeDelete this of
                            Just cascDel ->
                                case mdel of
                                    Nothing ->
                                        go acc mupd (Just cascDel) rest
                                    Just _ ->
                                        nope "found more than one OnDelete action"
                            Nothing ->
                                go (this : acc) mupd mdel rest
    nope msg =
        error $ msg <> ", tokens: " <> show allTokens

parseCascadeAction
    :: CascadePrefix
    -> Text
    -> Maybe CascadeAction
parseCascadeAction prfx text = do
    cascadeStr <- T.stripPrefix ("On" <> toPrefix prfx) text
    readMaybe cascadeStr
  where
    toPrefix cp =
        case cp of
            CascadeUpdate -> "Update"
            CascadeDelete -> "Delete"

takeDerives :: [Text] -> Maybe [Text]
takeDerives ("deriving" : rest) = Just rest
takeDerives _ = Nothing

-- | Returns 'True' if the 'UnboundFieldDef' does not have a 'MigrationOnly' or
-- 'SafeToRemove' flag from the QuasiQuoter.
--
-- @since 2.13.0.0
isHaskellUnboundField :: UnboundFieldDef -> Bool
isHaskellUnboundField fd =
    FieldAttrMigrationOnly `notElem` unboundFieldAttrs fd
        && FieldAttrSafeToRemove `notElem` unboundFieldAttrs fd

-- |  Return the 'EntityNameHS' for an 'UnboundEntityDef'.
--
-- @since 2.13.0.0
getUnboundEntityNameHS :: UnboundEntityDef -> EntityNameHS
getUnboundEntityNameHS = entityHaskell . unboundEntityDef

readMaybe :: (Read a) => Text -> Maybe a
readMaybe = R.readMaybe . T.unpack
