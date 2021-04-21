{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    , nullable
    , Token (..)
    , Line (..)
    , preparse
    , parseLine
    , parseFieldType
    , associateLines
    , LinesWithComments(..)
    , splitExtras
    , takeColsEx
    ) where

import Prelude hiding (lines)

import Control.Applicative ( Alternative((<|>)) )
import Control.Arrow ((&&&))
import Control.Monad (msum, mplus)
import Data.Char ( isLower, isSpace, isUpper, toLower )
import Data.List (find, foldl')
import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe, maybeToList, listToMaybe)
import Data.Monoid (mappend)
#if !MIN_VERSION_base(4,11,0)
-- This can be removed when GHC < 8.2.2 isn't supported anymore
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Types
import Text.Read (readEither)
import Database.Persist.EntityDef.Internal

data ParseState a = PSDone | PSFail String | PSSuccess a Text deriving Show

parseFieldType :: Text -> Either String FieldType
parseFieldType t0 =
    case parseApplyFT t0 of
        PSSuccess ft t'
            | T.all isSpace t' -> Right ft
        PSFail err -> Left $ "PSFail " ++ err
        other -> Left $ show other
  where
    parseApplyFT t =
        case goMany id t of
            PSSuccess (ft:fts) t' -> PSSuccess (foldl' FTApp ft fts) t'
            PSSuccess [] _ -> PSFail "empty"
            PSFail err -> PSFail err
            PSDone -> PSDone

    parseEnclosed :: Char -> (FieldType -> FieldType) -> Text -> ParseState FieldType
    parseEnclosed end ftMod t =
      let (a, b) = T.break (== end) t
      in case parseApplyFT a of
          PSSuccess ft t' -> case (T.dropWhile isSpace t', T.uncons b) of
              ("", Just (c, t'')) | c == end -> PSSuccess (ftMod ft) (t'' `Data.Monoid.mappend` t')
              (x, y) -> PSFail $ show (b, x, y)
          x -> PSFail $ show x

    parse1 t =
        case T.uncons t of
            Nothing -> PSDone
            Just (c, t')
                | isSpace c -> parse1 $ T.dropWhile isSpace t'
                | c == '(' -> parseEnclosed ')' id t'
                | c == '[' -> parseEnclosed ']' FTList t'
                | isUpper c ->
                    let (a, b) = T.break (\x -> isSpace x || x `elem` ("()[]"::String)) t
                     in PSSuccess (getCon a) b
                | otherwise -> PSFail $ show (c, t')
    getCon t =
        case T.breakOnEnd "." t of
            (_, "") -> FTTypeCon Nothing t
            ("", _) -> FTTypeCon Nothing t
            (a, b) -> FTTypeCon (Just $ T.init a) b
    goMany front t =
        case parse1 t of
            PSSuccess x t' -> goMany (front . (x:)) t'
            PSFail err -> PSFail err
            PSDone -> PSSuccess (front []) t
            -- _ ->

data PersistSettings = PersistSettings
    { psToDBName :: !(Text -> Text)
    -- ^ Modify the Haskell-style name into a database-style name.
    , psStrictFields :: !Bool
    -- ^ Whether fields are by default strict. Default value: @True@.
    --
    -- @since 1.2
    , psIdName :: !Text
    -- ^ The name of the id column. Default value: @id@
    -- The name of the id column can also be changed on a per-model basis
    -- <https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax>
    --
    -- @since 2.0
    }

defaultPersistSettings, upperCaseSettings, lowerCaseSettings :: PersistSettings
defaultPersistSettings = PersistSettings
    { psToDBName = id
    , psStrictFields = True
    , psIdName       = "id"
    }

upperCaseSettings = defaultPersistSettings

lowerCaseSettings = defaultPersistSettings
    { psToDBName =
        let go c
                | isUpper c = T.pack ['_', toLower c]
                | otherwise = T.singleton c
         in T.dropWhile (== '_') . T.concatMap go
    }

-- | Parses a quasi-quoted syntax into a list of entity definitions.
parse :: PersistSettings -> Text -> [EntityDef]
parse ps = maybe [] (parseLines ps) . preparse

preparse :: Text -> Maybe (NonEmpty Line)
preparse txt = do
    lns <- NEL.nonEmpty (T.lines txt)
    NEL.nonEmpty $ mapMaybe parseLine (NEL.toList lns)

parseLine :: Text -> Maybe Line
parseLine txt = do
    Line (parseIndentationAmount txt) <$> NEL.nonEmpty (tokenize txt)

-- | A token used by the parser.
data Token = Token Text    -- ^ @Token tok@ is token @tok@ already unquoted.
           | DocComment Text -- ^ @DocComment@ is a documentation comment, unmodified.
  deriving (Show, Eq)

tokenText :: Token -> Text
tokenText tok =
    case tok of
        Token t -> t
        DocComment t -> "-- | " <> t

parseIndentationAmount :: Text -> Int
parseIndentationAmount txt =
    let (spaces, _) = T.span isSpace txt
     in T.length spaces

-- | Tokenize a string.
tokenize :: Text -> [Token]
tokenize t
    | T.null t = []
    | Just txt <- T.stripPrefix "-- | " t = [DocComment txt]
    | "--" `T.isPrefixOf` t = [] -- Comment until the end of the line.
    | "#" `T.isPrefixOf` t = [] -- Also comment to the end of the line, needed for a CPP bug (#110)
    | T.head t == '"' = quotes (T.tail t) id
    | T.head t == '(' = parens 1 (T.tail t) id
    | isSpace (T.head t) =
        tokenize (T.dropWhile isSpace t)

    -- support mid-token quotes and parens
    | Just (beforeEquals, afterEquals) <- findMidToken t
    , not (T.any isSpace beforeEquals)
    , Token next : rest <- tokenize afterEquals =
        Token (T.concat [beforeEquals, "=", next]) : rest

    | otherwise =
        let (token, rest) = T.break isSpace t
         in Token token : tokenize rest
  where
    findMidToken t' =
        case T.break (== '=') t' of
            (x, T.drop 1 -> y)
                | "\"" `T.isPrefixOf` y || "(" `T.isPrefixOf` y -> Just (x, y)
            _ -> Nothing

    quotes t' front
        | T.null t' = error $ T.unpack $ T.concat $
            "Unterminated quoted string starting with " : front []
        | T.head t' == '"' = Token (T.concat $ front []) : tokenize (T.tail t')
        | T.head t' == '\\' && T.length t' > 1 =
            quotes (T.drop 2 t') (front . (T.take 1 (T.drop 1 t'):))
        | otherwise =
            let (x, y) = T.break (`elem` ['\\','\"']) t'
             in quotes y (front . (x:))
    parens count t' front
        | T.null t' = error $ T.unpack $ T.concat $
            "Unterminated parens string starting with " : front []
        | T.head t' == ')' =
            if count == (1 :: Int)
                then Token (T.concat $ front []) : tokenize (T.tail t')
                else parens (count - 1) (T.tail t') (front . (")":))
        | T.head t' == '(' =
            parens (count + 1) (T.tail t') (front . ("(":))
        | T.head t' == '\\' && T.length t' > 1 =
            parens count (T.drop 2 t') (front . (T.take 1 (T.drop 1 t'):))
        | otherwise =
            let (x, y) = T.break (`elem` ['\\','(',')']) t'
             in parens count y (front . (x:))

-- | A line of parsed tokens
data Line = Line
    { lineIndent   :: Int
    , tokens       :: NonEmpty Token
    } deriving (Eq, Show)

lineText :: Line -> NonEmpty Text
lineText = fmap tokenText . tokens

lowestIndent :: NonEmpty Line -> Int
lowestIndent = minimum . fmap lineIndent

-- | Divide lines into blocks and make entity definitions.
parseLines :: PersistSettings -> NonEmpty Line -> [EntityDef]
parseLines ps =
    fixForeignKeysAll . map mk . associateLines
  where
    mk :: LinesWithComments -> UnboundEntityDef
    mk lwc =
        let ln :| rest = lwcLines lwc
            (name :| entAttribs) = lineText ln
         in setComments (lwcComments lwc) $ mkEntityDef ps name entAttribs rest

isDocComment :: Token -> Maybe Text
isDocComment tok =
    case tok of
        DocComment txt -> Just txt
        _ -> Nothing

data LinesWithComments = LinesWithComments
    { lwcLines :: NonEmpty Line
    , lwcComments :: [Text]
    } deriving (Eq, Show)

-- TODO: drop this and use <> when 8.2 isn't supported anymore so the
-- monoid/semigroup nonsense isn't annoying
appendLwc :: LinesWithComments -> LinesWithComments -> LinesWithComments
appendLwc a b =
    LinesWithComments (foldr NEL.cons (lwcLines b) (lwcLines a)) (lwcComments a `mappend` lwcComments b)

newLine :: Line -> LinesWithComments
newLine l = LinesWithComments (pure l) []

firstLine :: LinesWithComments -> Line
firstLine = NEL.head . lwcLines

consLine :: Line -> LinesWithComments -> LinesWithComments
consLine l lwc = lwc { lwcLines = NEL.cons l (lwcLines lwc) }

consComment :: Text -> LinesWithComments -> LinesWithComments
consComment l lwc = lwc { lwcComments = l : lwcComments lwc }

associateLines :: NonEmpty Line -> [LinesWithComments]
associateLines lines =
    foldr combine [] $
    foldr toLinesWithComments [] lines
  where
    toLinesWithComments :: Line -> [LinesWithComments] -> [LinesWithComments]
    toLinesWithComments line linesWithComments =
        case linesWithComments of
            [] ->
                [newLine line]
            (lwc : lwcs) ->
                case isDocComment (NEL.head (tokens line)) of
                    Just comment
                        | lineIndent line == lowestIndent lines ->
                        consComment comment lwc : lwcs
                    _ ->
                        if lineIndent line <= lineIndent (firstLine lwc)
                            && lineIndent (firstLine lwc) /= lowestIndent lines
                        then
                            consLine line lwc : lwcs
                        else
                            newLine line : lwc : lwcs

    combine :: LinesWithComments -> [LinesWithComments] -> [LinesWithComments]
    combine lwc [] =
        [lwc]
    combine lwc (lwc' : lwcs) =
        let minIndent = minimumIndentOf lwc
            otherIndent = minimumIndentOf lwc'
         in
            if minIndent < otherIndent then
                appendLwc lwc lwc' : lwcs
            else
                lwc : lwc' : lwcs


    minimumIndentOf = lowestIndent . lwcLines

setComments :: [Text] -> UnboundEntityDef -> UnboundEntityDef
setComments [] = id
setComments comments =
    overUnboundEntityDef (\ed -> ed { entityComments = Just (T.unlines comments) })

fixForeignKeysAll :: [UnboundEntityDef] -> [EntityDef]
fixForeignKeysAll unEnts = map fixForeignKeys unEnts
  where
    ents = map unboundEntityDef unEnts
    entLookup = M.fromList $ map (\e -> (entityHaskell e, e)) ents

    fixForeignKeys :: UnboundEntityDef -> EntityDef
    fixForeignKeys (UnboundEntityDef foreigns ent) =
      ent { entityForeigns = map (fixForeignKey ent) foreigns }

    -- check the count and the sqltypes match and update the foreignFields with
    -- the names of the referenced columns
    fixForeignKey :: EntityDef -> UnboundForeignDef -> ForeignDef
    fixForeignKey ent (UnboundForeignDef foreignFieldTexts parentFieldTexts fdef) =
        case mfdefs of
             Just fdefs ->
                 if length foreignFieldTexts /= length fdefs
                 then
                     lengthError fdefs
                 else
                     let
                         fds_ffs =
                             zipWith toForeignFields
                                 foreignFieldTexts
                                 fdefs
                         dbname =
                             unEntityNameDB (entityDB pent)
                         oldDbName =
                             unEntityNameDB (foreignRefTableDBName fdef)
                      in fdef
                         { foreignFields = map snd fds_ffs
                         , foreignNullable = setNull $ map fst fds_ffs
                         , foreignRefTableDBName =
                             EntityNameDB dbname
                         , foreignConstraintNameDBName =
                             ConstraintNameDB
                             . T.replace oldDbName dbname . unConstraintNameDB
                             $ foreignConstraintNameDBName fdef
                         }
             Nothing ->
                 error $ "no primary key found fdef="++show fdef++ " ent="++show ent
      where
        pentError =
            error $ "could not find table " ++ show (foreignRefTableHaskell fdef)
            ++ " fdef=" ++ show fdef ++ " allnames="
            ++ show (map (unEntityNameHS . entityHaskell . unboundEntityDef) unEnts)
            ++ "\n\nents=" ++ show ents
        pent =
            fromMaybe pentError $ M.lookup (foreignRefTableHaskell fdef) entLookup
        mfdefs = case parentFieldTexts of
            [] -> entitiesPrimary pent
            _  -> Just $ map (getFd pent . FieldNameHS) parentFieldTexts

        setNull :: [FieldDef] -> Bool
        setNull [] = error "setNull: impossible!"
        setNull (fd:fds) = let nullSetting = isNull fd in
          if all ((nullSetting ==) . isNull) fds then nullSetting
            else error $ "foreign key columns must all be nullable or non-nullable"
                   ++ show (map (unFieldNameHS . fieldHaskell) (fd:fds))
        isNull = (NotNullable /=) . nullable . fieldAttrs

        toForeignFields :: Text -> FieldDef
            -> (FieldDef, (ForeignFieldDef, ForeignFieldDef))
        toForeignFields fieldText pfd =
           case chktypes fd haskellField pfd of
               Just err -> error err
               Nothing -> (fd, ((haskellField, fieldDB fd), (pfh, pfdb)))
          where
            fd = getFd ent haskellField

            haskellField = FieldNameHS fieldText
            (pfh, pfdb) = (fieldHaskell pfd, fieldDB pfd)

            chktypes ffld _fkey pfld =
                if fieldType ffld == fieldType pfld then Nothing
                  else Just $ "fieldType mismatch: " ++ show (fieldType ffld) ++ ", " ++ show (fieldType pfld)

        getFd :: EntityDef -> FieldNameHS -> FieldDef
        getFd entity t = go (keyAndEntityFields entity)
          where
            go [] = error $ "foreign key constraint for: " ++ show (unEntityNameHS $ entityHaskell entity)
                       ++ " unknown column: " ++ show t
            go (f:fs)
                | fieldHaskell f == t = f
                | otherwise = go fs

        lengthError pdef = error $ "found " ++ show (length foreignFieldTexts) ++ " fkeys and " ++ show (length pdef) ++ " pkeys: fdef=" ++ show fdef ++ " pdef=" ++ show pdef


data UnboundEntityDef
    = UnboundEntityDef
    { _unboundForeignDefs :: [UnboundForeignDef]
    , unboundEntityDef :: EntityDef
    }

overUnboundEntityDef
    :: (EntityDef -> EntityDef) -> UnboundEntityDef -> UnboundEntityDef
overUnboundEntityDef f ubed =
    ubed { unboundEntityDef = f (unboundEntityDef ubed) }

lookupKeyVal :: Text -> [Text] -> Maybe Text
lookupKeyVal key = lookupPrefix $ key `mappend` "="

lookupPrefix :: Text -> [Text] -> Maybe Text
lookupPrefix prefix = msum . map (T.stripPrefix prefix)

-- | Construct an entity definition.
mkEntityDef
    :: PersistSettings
    -> Text -- ^ name
    -> [Attr] -- ^ entity attributes
    -> [Line] -- ^ indented lines
    -> UnboundEntityDef
mkEntityDef ps name entattribs lines =
    UnboundEntityDef foreigns $
        EntityDef
            { entityHaskell = entName
            , entityDB = EntityNameDB $ getDbName ps name' entattribs
            -- idField is the user-specified Id
            -- otherwise useAutoIdField
            -- but, adjust it if the user specified a Primary
            , entityId = setComposite primaryComposite $ fromMaybe autoIdField idField
            , entityAttrs = entattribs
            , entityFields = cols
            , entityUniques = uniqs
            , entityForeigns = []
            , entityDerives = concat $ mapMaybe takeDerives textAttribs
            , entityExtra = extras
            , entitySum = isSum
            , entityComments = Nothing
            }
  where
    entName = EntityNameHS name'
    (isSum, name') =
        case T.uncons name of
            Just ('+', x) -> (True, x)
            _ -> (False, name)
    (attribs, extras) = splitExtras lines

    textAttribs :: [[Text]]
    textAttribs =
        fmap tokenText <$> attribs

    attribPrefix = flip lookupKeyVal entattribs
    idName | Just _ <- attribPrefix "id" = error "id= is deprecated, ad a field named 'Id' and use sql="
           | otherwise = Nothing

    (idField, primaryComposite, uniqs, foreigns) = foldl' (\(mid, mp, us, fs) attr ->
        let (i, p, u, f) = takeConstraint ps name' cols attr
            squish xs m = xs `mappend` maybeToList m
        in (just1 mid i, just1 mp p, squish us u, squish fs f)) (Nothing, Nothing, [],[]) textAttribs

    cols :: [FieldDef]
    cols = reverse . fst . foldr k ([], []) $ reverse attribs

    k x (!acc, !comments) =
        case listToMaybe x of
            Just (DocComment comment) ->
                (acc, comment : comments)
            _ ->
                case (setFieldComments comments <$> takeColsEx ps (tokenText <$> x)) of
                  Just sm ->
                      (sm : acc, [])
                  Nothing ->
                      (acc, [])

    autoIdField = mkAutoIdField ps entName (FieldNameDB `fmap` idName) idSqlType
    idSqlType = maybe SqlInt64 (const $ SqlOther "Primary Key") primaryComposite

    setComposite Nothing fd = fd
    setComposite (Just c) fd = fd
        { fieldReference = CompositeRef c
        }

setFieldComments :: [Text] -> FieldDef -> FieldDef
setFieldComments xs fld =
    case xs of
        [] -> fld
        _ -> fld { fieldComments = Just (T.unlines xs) }

just1 :: (Show x) => Maybe x -> Maybe x -> Maybe x
just1 (Just x) (Just y) = error $ "expected only one of: "
  `mappend` show x `mappend` " " `mappend` show y
just1 x y = x `mplus` y

mkAutoIdField :: PersistSettings -> EntityNameHS -> Maybe FieldNameDB -> SqlType -> FieldDef
mkAutoIdField ps entName idName idSqlType =
    FieldDef
        { fieldHaskell = FieldNameHS "Id"
        -- this should be modeled as a Maybe
        -- but that sucks for non-ID field
        -- TODO: use a sumtype FieldDef | IdFieldDef
        , fieldDB = fromMaybe (FieldNameDB $ psIdName ps) idName
        , fieldType = FTTypeCon Nothing $ keyConName $ unEntityNameHS entName
        , fieldSqlType = idSqlType
        -- the primary field is actually a reference to the entity
        , fieldReference = ForeignRef entName defaultReferenceTypeCon
        , fieldAttrs = []
        , fieldStrict = True
        , fieldComments = Nothing
        , fieldCascade = noCascade
        , fieldGenerated = Nothing
        }

defaultReferenceTypeCon :: FieldType
defaultReferenceTypeCon = FTTypeCon (Just "Data.Int") "Int64"

keyConName :: Text -> Text
keyConName entName = entName `mappend` "Id"

splitExtras
    :: [Line]
    -> ( [[Token]]
       , M.Map Text [ExtraLine]
       )
splitExtras lns =
    case lns of
        [] -> ([], M.empty)
        (line : rest) ->
            case NEL.toList (tokens line) of
                [Token name]
                  | isCapitalizedText name ->
                    let indent = lineIndent line
                        (children, rest') = span ((> indent) . lineIndent) rest
                        (x, y) = splitExtras rest'
                     in (x, M.insert name (NEL.toList . lineText <$> children) y)
                ts ->
                    let (x, y) = splitExtras rest
                     in (ts:x, y)

isCapitalizedText :: Text -> Bool
isCapitalizedText t =
    not (T.null t) && isUpper (T.head t)

takeColsEx :: PersistSettings -> [Text] -> Maybe FieldDef
takeColsEx =
    takeCols
        (\ft perr -> error $ "Invalid field type " ++ show ft ++ " " ++ perr)

takeCols
    :: (Text -> String -> Maybe FieldDef)
    -> PersistSettings
    -> [Text]
    -> Maybe FieldDef
takeCols _ _ ("deriving":_) = Nothing
takeCols onErr ps (n':typ:rest')
    | not (T.null n) && isLower (T.head n) =
        case parseFieldType typ of
            Left err -> onErr typ err
            Right ft -> Just FieldDef
                { fieldHaskell = FieldNameHS n
                , fieldDB = FieldNameDB $ getDbName ps n attrs_
                , fieldType = ft
                , fieldSqlType = SqlOther $ "SqlType unset for " `mappend` n
                , fieldAttrs = fieldAttrs_
                , fieldStrict = fromMaybe (psStrictFields ps) mstrict
                , fieldReference = NoReference
                , fieldComments = Nothing
                , fieldCascade = cascade_
                , fieldGenerated = generated_
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
getDbName ps n [] = psToDBName ps n
getDbName ps n (a:as) = fromMaybe (getDbName ps n as) $ T.stripPrefix "sql=" a

takeConstraint :: PersistSettings
          -> Text
          -> [FieldDef]
          -> [Text]
          -> (Maybe FieldDef, Maybe CompositeDef, Maybe UniqueDef, Maybe UnboundForeignDef)
takeConstraint ps tableName defs (n:rest) | isCapitalizedText n = takeConstraint'
    where
      takeConstraint'
            | n == "Unique"  = (Nothing, Nothing, Just $ takeUniq ps tableName defs rest, Nothing)
            | n == "Foreign" = (Nothing, Nothing, Nothing, Just $ takeForeign ps tableName defs rest)
            | n == "Primary" = (Nothing, Just $ takeComposite defs rest, Nothing, Nothing)
            | n == "Id"      = (Just $ takeId ps tableName (n:rest), Nothing, Nothing, Nothing)
            | otherwise      = (Nothing, Nothing, Just $ takeUniq ps "" defs (n:rest), Nothing) -- retain compatibility with original unique constraint
takeConstraint _ _ _ _ = (Nothing, Nothing, Nothing, Nothing)

-- TODO: this is hacky (the double takeCols, the setFieldDef stuff, and setIdName.
-- need to re-work takeCols function
takeId :: PersistSettings -> Text -> [Text] -> FieldDef
takeId ps tableName (n:rest) =
    setFieldDef
    $ fromMaybe (error "takeId: impossible!")
    $ takeCols (\_ _ -> addDefaultIdType) ps (field:rest) -- `mappend` setIdName)
  where
    field = case T.uncons n of
        Nothing -> error "takeId: empty field"
        Just (f, ield) -> toLower f `T.cons` ield
    addDefaultIdType = takeColsEx ps (field : keyCon : rest ) -- `mappend` setIdName)
    setFieldDef fd = fd
        { fieldReference =
            ForeignRef (EntityNameHS tableName) $
                if fieldType fd == FTTypeCon Nothing keyCon
                then defaultReferenceTypeCon
                else fieldType fd
        }
    keyCon = keyConName tableName
    -- this will be ignored if there is already an existing sql=
    -- TODO: I think there is a ! ignore syntax that would screw this up
    -- setIdName = ["sql=" `mappend` psIdName ps]
takeId _ tableName _ = error $ "empty Id field for " `mappend` show tableName


takeComposite
    :: [FieldDef]
    -> [Text]
    -> CompositeDef
takeComposite fields pkcols =
    CompositeDef (map (getDef fields) pkcols) attrs
  where
    (_, attrs) = break ("!" `T.isPrefixOf`) pkcols
    getDef [] t = error $ "Unknown column in primary key constraint: " ++ show t
    getDef (d:ds) t
        | fieldHaskell d == FieldNameHS t =
            if nullable (fieldAttrs d) /= NotNullable
                then error $ "primary key column cannot be nullable: " ++ show t ++ show fields
                else d
        | otherwise = getDef ds t

-- Unique UppercaseConstraintName list of lowercasefields terminated
-- by ! or sql= such that a unique constraint can look like:
-- `UniqueTestNull fieldA fieldB sql=ConstraintNameInDatabase !force`
-- Here using sql= sets the name of the constraint.
takeUniq :: PersistSettings
         -> Text
         -> [FieldDef]
         -> [Text]
         -> UniqueDef
takeUniq ps tableName defs (n:rest)
    | isCapitalizedText n
        = UniqueDef
            (ConstraintNameHS n)
            dbName
            (map (FieldNameHS &&& getDBName defs) fields)
            attrs
  where
    isAttr a =
      "!" `T.isPrefixOf` a
    isSqlName a =
      "sql=" `T.isPrefixOf` a
    isNonField a =
       isAttr a
      || isSqlName a
    (fields, nonFields) =
      break isNonField rest
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
    getDBName [] t =
      error $ "Unknown column in unique constraint: " ++ show t
              ++ " " ++ show defs ++ show n ++ " " ++ show attrs
    getDBName (d:ds) t
        | fieldHaskell d == FieldNameHS t = fieldDB d
        | otherwise = getDBName ds t
takeUniq _ tableName _ xs =
  error $ "invalid unique constraint on table["
          ++ show tableName
          ++ "] expecting an uppercase constraint name xs="
          ++ show xs

data UnboundForeignDef = UnboundForeignDef
                         { _unboundForeignFields :: [Text] -- ^ fields in the parent entity
                         , _unboundParentFields :: [Text] -- ^ fields in parent entity
                         , _unboundForeignDef :: ForeignDef
                         }

takeForeign
    :: PersistSettings
    -> Text
    -> [FieldDef]
    -> [Text]
    -> UnboundForeignDef
takeForeign ps tableName _defs = takeRefTable
  where
    errorPrefix :: String
    errorPrefix = "invalid foreign key constraint on table[" ++ show tableName ++ "] "

    takeRefTable :: [Text] -> UnboundForeignDef
    takeRefTable [] = error $ errorPrefix ++ " expecting foreign table name"
    takeRefTable (refTableName:restLine) = go restLine Nothing Nothing
      where
        go :: [Text] -> Maybe CascadeAction -> Maybe CascadeAction -> UnboundForeignDef
        go (n:rest) onDelete onUpdate | not (T.null n) && isLower (T.head n)
            = UnboundForeignDef fFields pFields $ ForeignDef
                { foreignRefTableHaskell =
                    EntityNameHS refTableName
                , foreignRefTableDBName =
                    EntityNameDB $ psToDBName ps refTableName
                , foreignConstraintNameHaskell =
                    ConstraintNameHS n
                , foreignConstraintNameDBName =
                    ConstraintNameDB $ psToDBName ps (tableName `T.append` n)
                , foreignFieldCascade = FieldCascade
                    { fcOnDelete = onDelete
                    , fcOnUpdate = onUpdate
                    }
                , foreignFields =
                    []
                , foreignAttrs =
                    attrs
                , foreignNullable =
                    False
                , foreignToPrimary =
                    null pFields
                }
          where
            (fields,attrs) = break ("!" `T.isPrefixOf`) rest
            (fFields, pFields) = case break (== "References") fields of
                (ffs, []) -> (ffs, [])
                (ffs, _ : pfs) -> case (length ffs, length pfs) of
                    (flen, plen) | flen == plen -> (ffs, pfs)
                    (flen, plen) -> error $ errorPrefix ++ concat
                        [ "Found ", show flen, " foreign fields but "
                        , show plen, " parent fields" ]

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

        go xs _ _ = error $ errorPrefix ++ "expecting a lower case constraint name or a cascading action xs=" ++ show xs

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
                                        nope "found more than one OnDelete action: "
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
    case readEither (T.unpack cascadeStr) of
        Right a ->
            Just a
        Left _ ->
            Nothing
  where
    toPrefix cp =
        case cp of
            CascadeUpdate -> "Update"
            CascadeDelete -> "Delete"

takeDerives :: [Text] -> Maybe [Text]
takeDerives ("deriving":rest) = Just rest
takeDerives _ = Nothing

nullable :: [FieldAttr] -> IsNullable
nullable s
    | FieldAttrMaybe    `elem` s = Nullable ByMaybeAttr
    | FieldAttrNullable `elem` s = Nullable ByNullableAttr
    | otherwise = NotNullable
