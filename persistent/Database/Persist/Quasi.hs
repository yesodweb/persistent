{-# LANGUAGE BangPatterns, CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-|
This module defines the Persistent entity syntax used in the quasiquoter to generate persistent entities.

The basic structure of the syntax looks like this:

> TableName
>     fieldName      FieldType
>     otherField     String
>     nullableField  Int       Maybe

You start an entity definition with the table name, in this case, @TableName@. It's followed by a list of fields on the entity, which have the basic form @fieldName FieldType@. You can indicate that a field is nullable with 'Maybe' at the end of the type.

@persistent@ automatically generates an ID column for you, if you don't specify one, so the above table definition corresponds to the following SQL:

> CREATE TABLE table_name (
>     id                SERIAL PRIMARY KEY,
>     field_name        field_type NOT NULL,
>     other_field       varchar    NOT NULL,
>     nullable_field    int NULL
> );

Note that the exact SQL that is generated can be customized using the 'PersistSettings' that are passed to the 'parse' function.

It generates a Haskell datatype with the following form:

@
data TableName = TableName
    { tableNameFieldName :: FieldType
    , tableNameOtherField :: String
    , tableNameNullableField :: Maybe Int
    }
@

As with the SQL generated, the specifics of this are customizable.
See the @persistent-template@ package for details.

= Deriving

You can add a deriving clause to a table, and the generated Haskell type will have a deriving clause with that.
Unlike normal Haskell syntax, you don't need parentheses or commas to separate the classes, and you can even have multiple deriving clauses.

> User
>     name String
>     age  Int
>     deriving Eq Show
>     deriving Ord

= Unique Keys

You can define a uniqueness key on a table with the following format:

> User
>    name String
>    age  Int
>
>    UniqueUserName name

This will put a unique index on the @user@ table and the @name@ field.

= Setting defaults

You can use a @default=${sql expression}@ clause to set a default for a field.
The thing following the `=` is interpreted as SQL that will be put directly into the table definition.

@
User
    name    Text
    admin   Bool default=false
@

This creates a SQL definition like this:

> CREATE TABLE user (
>   id      SERIAL PRIMARY KEY,
>   name    VARCHAR NOT NULL,
>   admin   BOOL DEFAULT=false
> );

A restriction here is that you still need to provide a value when performing an `insert`, because the generated Haskell type has the form:

@
data User = User
    { userName :: Text
    , userAdmin :: Bool
    }
@

You can work around this by using a 'Maybe Bool' and supplying 'Nothing' by default.

= Custom ID column

If you don't want to use the default ID column type of 'Int64', you can set a custom type with an @Id@ field.
This @User@ has a @Text@ ID.

> User
>     Id   Text
>     name Text
>     age  Int

If you do this, it's a good idea to set a default for the ID.
Otherwise, you will need to use 'insertKey' instead of 'insert' when performing inserts.

@
'insertKey' (UserKey "Hello world!") (User "Bob" 32)
@

If you attempt to do @'insert' (User "Bob" 32)@, then you will receive a runtime error because the SQL database doesn't know how to make an ID for you anymore.
So instead just use a default expression, like this:

@
User
    Id      Text default=generate_user_id()
    name    Text
    age     Int
@

= Custom Primary Keys

Sometimes you don't want to have an ID column, and you want a different sort of primary key.
This is a table that stores unique email addresses, and the email is the primary key.
We store the first and second part (eg @first\@second@) separately.

@
Email
    firstPart   Text
    secondPart  Text

    Primary firstPart secondPart
@

This creates a table with the following form:

@
CREATE TABLE email (
    first_part  varchar,
    second_part varchar,

    PRIMARY KEY (first_part, second_part)
@

You can specify 1 or more columns in the primary key.

= Overriding SQL

You can use a @sql=custom@ annotation to provide some customization on the entity and field.
For example, you might prefer to name a table differently than what @persistent@ will do by default.
You may also prefer to name a field differently.

@
User sql=big_user_table
    fullName    String sql=name
    age         Int
@

This will alter the generated SQL to be:

@
CREATE TABEL big_user_table (
    id      SERIAL PRIMARY KEY,
    name    VARCHAR,
    age     INT
);
@

= Attributes

The QuasiQuoter allows you to provide arbitrary attributes to an entity or field.
This can be used to extend the code in ways that the library hasn't anticipated.
If you use this feature, we'd definitely appreciate hearing about it and potentially supporting your use case directly!

@
User !funny
    field   String  !sad
    good    Dog     !sogood
@

We can see the attributes using the 'entityAttrs' field and the 'fieldAttrs' field.

@
userAttrs = do
    let userDefinition = 'entityDef' ('Proxy' :: 'Proxy' User)
    let userAttributes = 'entityAttrs' userDefinition
    let fieldAttributes = 'map' 'fieldAttrs' ('entityFields' userDefinition)
    print userAttributes
-- ["funny"]
    print fieldAttributes
-- [["sad"],["sogood"]]
@

= Documentation Comments

The quasiquoter supports ordinary comments with @--@ and @#@.
Since @persistent-2.10.5.1@, it also supports documentation comments.
The grammar for documentation comments is similar to Haskell's Haddock syntax, with a few restrictions:

1. Only the @-- | @ form is allowed.
2. You must put a space before and after the @|@ pipe character.
3. The comment must be indented at the same level as the entity or field it documents.

An example of the field documentation is:

@
-- | I am a doc comment for a User. Users are important
-- | to the application, and should be treasured.
User
    -- | Users have names. Call them by names.
    name String
    -- | A user can be old, or young, and we care about
    -- | this for some reason.
    age Int
@

The documentation is present on the `entityComments` field on the `EntityDef` for the entity:

@
>>> let userDefinition = entityDef (Proxy :: Proxy User)
>>> entityComments userDefinition
"I am a doc comment for a User. Users are important\nto the application, and should be treasured.\n"
@

Likewise, the field documentation is present in the `fieldComments` field on the `FieldDef` present in the `EntityDef`:

@
>>> let userFields = entityFields userDefinition
>>> let comments = map fieldComments userFields
>>> mapM_ putStrLn comments
"Users have names. Call them by names."
"A user can be old, or young, and we care about\nthis for some reason."
@

Unfortunately, we can't use this to create Haddocks for you, because <https://gitlab.haskell.org/ghc/ghc/issues/5467 Template Haskell does not support Haddock yet>.
`persistent` backends *can* use this to generate SQL @COMMENT@s, which are useful for a database perspective, and you can use the <https://hackage.haskell.org/package/persistent-documentation @persistent-documentation@> library to render a Markdown document of the entity definitions.

-}
module Database.Persist.Quasi
    ( parse
    , PersistSettings (..)
    , upperCaseSettings
    , lowerCaseSettings
    , nullable
#if TEST
    , Token (..)
    , Line' (..)
    , preparse
    , tokenize
    , parseFieldType
    , empty
    , removeSpaces
    , associateLines
    , skipEmpty
    , LinesWithComments(..)
#endif
    ) where

import Prelude hiding (lines)

import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty (NonEmpty(..))
import Control.Arrow ((&&&))
import Control.Monad (msum, mplus)
import Data.Char
import Data.List (find, foldl')
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe, maybeToList, listToMaybe)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Types
import Text.Read (readEither)

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
parse ps = parseLines ps . preparse

preparse :: Text -> [Line]
preparse =
    removeSpaces
        . filter (not . empty)
        . map tokenize
        . T.lines

-- | A token used by the parser.
data Token = Spaces !Int   -- ^ @Spaces n@ are @n@ consecutive spaces.
           | Token Text    -- ^ @Token tok@ is token @tok@ already unquoted.
           | DocComment Text -- ^ @DocComment@ is a documentation comment, unmodified.
  deriving (Show, Eq)

-- | Tokenize a string.
tokenize :: Text -> [Token]
tokenize t
    | T.null t = []
    | "-- | " `T.isPrefixOf` t = [DocComment t]
    | "--" `T.isPrefixOf` t = [] -- Comment until the end of the line.
    | "#" `T.isPrefixOf` t = [] -- Also comment to the end of the line, needed for a CPP bug (#110)
    | T.head t == '"' = quotes (T.tail t) id
    | T.head t == '(' = parens 1 (T.tail t) id
    | isSpace (T.head t) =
        let (spaces, rest) = T.span isSpace t
         in Spaces (T.length spaces) : tokenize rest

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

-- | A string of tokens is empty when it has only spaces.  There
-- can't be two consecutive 'Spaces', so this takes /O(1)/ time.
empty :: [Token] -> Bool
empty []         = True
empty [Spaces _] = True
empty _          = False

-- | A line.  We don't care about spaces in the middle of the
-- line.  Also, we don't care about the ammount of indentation.
data Line' f
    = Line
    { lineIndent   :: Int
    , tokens       :: f Text
    }

deriving instance Show (f Text) => Show (Line' f)
deriving instance Eq (f Text) => Eq (Line' f)

mapLine :: (forall x. f x -> g x) -> Line' f -> Line' g
mapLine k (Line i t) = Line i (k t)

traverseLine :: Functor t => (forall x. f x -> t (g x)) -> Line' f -> t (Line' g)
traverseLine k (Line i xs) = Line i <$> k xs

type Line = Line' []

-- | Remove leading spaces and remove spaces in the middle of the
-- tokens.
removeSpaces :: [[Token]] -> [Line]
removeSpaces =
    map toLine
  where
    toLine (Spaces i:rest) = toLine' i rest
    toLine xs              = toLine' 0 xs

    toLine' i = Line i . mapMaybe fromToken

    fromToken (Token t) = Just t
    fromToken (DocComment t) = Just t
    fromToken Spaces{}  = Nothing

-- | Divide lines into blocks and make entity definitions.
parseLines :: PersistSettings -> [Line] -> [EntityDef]
parseLines ps lines =
    fixForeignKeysAll $ toEnts lines
  where
    toEnts :: [Line] -> [UnboundEntityDef]
    toEnts =
        map mk
        . associateLines
        . skipEmpty
    mk :: LinesWithComments -> UnboundEntityDef
    mk lwc =
        let Line _ (name :| entAttribs) :| rest = lwcLines lwc
         in setComments (lwcComments lwc) $ mkEntityDef ps name entAttribs (map (mapLine NEL.toList) rest)

isComment :: Text -> Maybe Text
isComment xs =
    T.stripPrefix "-- | " xs

data LinesWithComments = LinesWithComments
    { lwcLines :: NonEmpty (Line' NonEmpty)
    , lwcComments :: [Text]
    } deriving (Eq, Show)

-- TODO: drop this and use <> when 8.2 isn't supported anymore so the
-- monoid/semigroup nonsense isn't annoying
appendLwc :: LinesWithComments -> LinesWithComments -> LinesWithComments
appendLwc a b =
    LinesWithComments (foldr NEL.cons (lwcLines b) (lwcLines a)) (lwcComments a `mappend` lwcComments b)

newLine :: Line' NonEmpty -> LinesWithComments
newLine l = LinesWithComments (pure l) []

firstLine :: LinesWithComments -> Line' NonEmpty
firstLine = NEL.head . lwcLines

consLine :: Line' NonEmpty -> LinesWithComments -> LinesWithComments
consLine l lwc = lwc { lwcLines = NEL.cons l (lwcLines lwc) }

consComment :: Text -> LinesWithComments -> LinesWithComments
consComment l lwc = lwc { lwcComments = l : lwcComments lwc }

associateLines :: [Line' NonEmpty] -> [LinesWithComments]
associateLines lines =
    foldr combine [] $
    foldr toLinesWithComments [] lines
  where
    toLinesWithComments line linesWithComments =
        case linesWithComments of
            [] ->
                [newLine line]
            (lwc : lwcs) ->
                case isComment (NEL.head (tokens line)) of
                    Just comment
                        | lineIndent line == lowestIndent ->
                        consComment comment lwc : lwcs
                    _ ->
                        if lineIndent line <= lineIndent (firstLine lwc)
                        then
                            consLine line lwc : lwcs
                        else
                            newLine line : lwc : lwcs

    lowestIndent = minimum . fmap lineIndent $ lines
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


    minimumIndentOf = minimum . fmap lineIndent . lwcLines

skipEmpty :: [Line' []] -> [Line' NonEmpty]
skipEmpty = mapMaybe (traverseLine NEL.nonEmpty)

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

    -- check the count and the sqltypes match and update the foreignFields with the names of the primary columns
    fixForeignKey :: EntityDef -> UnboundForeignDef -> ForeignDef
    fixForeignKey ent (UnboundForeignDef foreignFieldTexts fdef) =
        let pentError =
                error $ "could not find table " ++ show (foreignRefTableHaskell fdef)
                ++ " fdef=" ++ show fdef ++ " allnames="
                ++ show (map (unHaskellName . entityHaskell . unboundEntityDef) unEnts)
                ++ "\n\nents=" ++ show ents
            pent =
                fromMaybe pentError $ M.lookup (foreignRefTableHaskell fdef) entLookup
         in
            case entityPrimary pent of
                Just pdef ->
                    if length foreignFieldTexts /= length (compositeFields pdef)
                    then
                        lengthError pdef
                    else
                        let
                            fds_ffs =
                                zipWith (toForeignFields pent)
                                    foreignFieldTexts
                                    (compositeFields pdef)
                            dbname =
                                unDBName (entityDB pent)
                            oldDbName =
                                unDBName (foreignRefTableDBName fdef)
                         in fdef
                            { foreignFields = map snd fds_ffs
                            , foreignNullable = setNull $ map fst fds_ffs
                            , foreignRefTableDBName =
                                DBName dbname
                            , foreignConstraintNameDBName =
                                DBName
                                . T.replace oldDbName dbname . unDBName
                                $ foreignConstraintNameDBName fdef
                            }
                Nothing ->
                    error $ "no explicit primary key fdef="++show fdef++ " ent="++show ent
      where
        setNull :: [FieldDef] -> Bool
        setNull [] = error "setNull: impossible!"
        setNull (fd:fds) = let nullSetting = isNull fd in
          if all ((nullSetting ==) . isNull) fds then nullSetting
            else error $ "foreign key columns must all be nullable or non-nullable"
                   ++ show (map (unHaskellName . fieldHaskell) (fd:fds))
        isNull = (NotNullable /=) . nullable . fieldAttrs

        toForeignFields pent fieldText pfd =
           case chktypes fd haskellField (entityFields pent) pfh of
               Just err -> error err
               Nothing -> (fd, ((haskellField, fieldDB fd), (pfh, pfdb)))
          where
            fd = getFd (entityFields ent) haskellField

            haskellField = HaskellName fieldText
            (pfh, pfdb) = (fieldHaskell pfd, fieldDB pfd)

            chktypes :: FieldDef -> HaskellName -> [FieldDef] -> HaskellName -> Maybe String
            chktypes ffld _fkey pflds pkey =
                if fieldType ffld == fieldType pfld then Nothing
                  else Just $ "fieldType mismatch: " ++ show (fieldType ffld) ++ ", " ++ show (fieldType pfld)
              where
                pfld = getFd pflds pkey

            entName = entityHaskell ent
            getFd [] t = error $ "foreign key constraint for: " ++ show (unHaskellName entName)
                           ++ " unknown column: " ++ show t
            getFd (f:fs) t
                | fieldHaskell f == t = f
                | otherwise = getFd fs t

        lengthError pdef = error $ "found " ++ show (length foreignFieldTexts) ++ " fkeys and " ++ show (length (compositeFields pdef)) ++ " pkeys: fdef=" ++ show fdef ++ " pdef=" ++ show pdef


data UnboundEntityDef = UnboundEntityDef
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
mkEntityDef :: PersistSettings
            -> Text -- ^ name
            -> [Attr] -- ^ entity attributes
            -> [Line] -- ^ indented lines
            -> UnboundEntityDef
mkEntityDef ps name entattribs lines =
  UnboundEntityDef foreigns $
    EntityDef
        { entityHaskell = entName
        , entityDB = DBName $ getDbName ps name' entattribs
        -- idField is the user-specified Id
        -- otherwise useAutoIdField
        -- but, adjust it if the user specified a Primary
        , entityId = (setComposite primaryComposite $ fromMaybe autoIdField idField)
        , entityAttrs = entattribs
        , entityFields = cols
        , entityUniques = uniqs
        , entityForeigns = []
        , entityDerives = derives
        , entityExtra = extras
        , entitySum = isSum
        , entityComments = comments
        }
  where
    comments = Nothing
    entName = HaskellName name'
    (isSum, name') =
        case T.uncons name of
            Just ('+', x) -> (True, x)
            _ -> (False, name)
    (attribs, extras) = splitExtras lines

    attribPrefix = flip lookupKeyVal entattribs
    idName | Just _ <- attribPrefix "id" = error "id= is deprecated, ad a field named 'Id' and use sql="
           | otherwise = Nothing

    (idField, primaryComposite, uniqs, foreigns) = foldl' (\(mid, mp, us, fs) attr ->
        let (i, p, u, f) = takeConstraint ps name' cols attr
            squish xs m = xs `mappend` maybeToList m
        in (just1 mid i, just1 mp p, squish us u, squish fs f)) (Nothing, Nothing, [],[]) attribs

    derives = concat $ mapMaybe takeDerives attribs

    cols :: [FieldDef]
    cols = reverse . fst . foldr k ([], []) $ reverse attribs
    k x (!acc, !comments) =
        case isComment =<< listToMaybe x of
            Just comment ->
                (acc, comment : comments)
            Nothing ->
                ( maybe id (:) (setFieldComments comments <$> takeColsEx ps x) acc
                , []
                )
    setFieldComments [] x = x
    setFieldComments xs fld =
        fld { fieldComments = Just (T.unlines xs) }

    autoIdField = mkAutoIdField ps entName (DBName `fmap` idName) idSqlType
    idSqlType = maybe SqlInt64 (const $ SqlOther "Primary Key") primaryComposite

    setComposite Nothing fd = fd
    setComposite (Just c) fd = fd { fieldReference = CompositeRef c }


just1 :: (Show x) => Maybe x -> Maybe x -> Maybe x
just1 (Just x) (Just y) = error $ "expected only one of: "
  `mappend` show x `mappend` " " `mappend` show y
just1 x y = x `mplus` y


mkAutoIdField :: PersistSettings -> HaskellName -> Maybe DBName -> SqlType -> FieldDef
mkAutoIdField ps entName idName idSqlType = FieldDef
      { fieldHaskell = HaskellName "Id"
      -- this should be modeled as a Maybe
      -- but that sucks for non-ID field
      -- TODO: use a sumtype FieldDef | IdFieldDef
      , fieldDB = fromMaybe (DBName $ psIdName ps) idName
      , fieldType = FTTypeCon Nothing $ keyConName $ unHaskellName entName
      , fieldSqlType = idSqlType
      -- the primary field is actually a reference to the entity
      , fieldReference = ForeignRef entName  defaultReferenceTypeCon
      , fieldAttrs = []
      , fieldStrict = True
      , fieldComments = Nothing
      }

defaultReferenceTypeCon :: FieldType
defaultReferenceTypeCon = FTTypeCon (Just "Data.Int") "Int64"

keyConName :: Text -> Text
keyConName entName = entName `mappend` "Id"


splitExtras :: [Line] -> ([[Text]], M.Map Text [[Text]])
splitExtras [] = ([], M.empty)
splitExtras (Line indent [name]:rest)
    | not (T.null name) && isUpper (T.head name) =
        let (children, rest') = span ((> indent) . lineIndent) rest
            (x, y) = splitExtras rest'
         in (x, M.insert name (map tokens children) y)
splitExtras (Line _ ts:rest) =
    let (x, y) = splitExtras rest
     in (ts:x, y)

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
takeCols onErr ps (n':typ:rest)
    | not (T.null n) && isLower (T.head n) =
        case parseFieldType typ of
            Left err -> onErr typ err
            Right ft -> Just FieldDef
                { fieldHaskell = HaskellName n
                , fieldDB = DBName $ getDbName ps n rest
                , fieldType = ft
                , fieldSqlType = SqlOther $ "SqlType unset for " `mappend` n
                , fieldAttrs = rest
                , fieldStrict = fromMaybe (psStrictFields ps) mstrict
                , fieldReference = NoReference
                , fieldComments = Nothing
                }
  where
    (mstrict, n)
        | Just x <- T.stripPrefix "!" n' = (Just True, x)
        | Just x <- T.stripPrefix "~" n' = (Just False, x)
        | otherwise = (Nothing, n')
takeCols _ _ _ = Nothing

getDbName :: PersistSettings -> Text -> [Text] -> Text
getDbName ps n [] = psToDBName ps n
getDbName ps n (a:as) = fromMaybe (getDbName ps n as) $ T.stripPrefix "sql=" a

takeConstraint :: PersistSettings
          -> Text
          -> [FieldDef]
          -> [Text]
          -> (Maybe FieldDef, Maybe CompositeDef, Maybe UniqueDef, Maybe UnboundForeignDef)
takeConstraint ps tableName defs (n:rest) | not (T.null n) && isUpper (T.head n) = takeConstraint'
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
takeId ps tableName (n:rest) = fromMaybe (error "takeId: impossible!") $ setFieldDef $
    takeCols (\_ _ -> addDefaultIdType) ps (field:rest `mappend` setIdName)
  where
    field = case T.uncons n of
      Nothing -> error "takeId: empty field"
      Just (f, ield) -> toLower f `T.cons` ield
    addDefaultIdType = takeColsEx ps (field : keyCon : rest `mappend` setIdName)
    setFieldDef = fmap (\fd ->
      let refFieldType = if fieldType fd == FTTypeCon Nothing keyCon
              then defaultReferenceTypeCon
              else fieldType fd
      in fd { fieldReference = ForeignRef (HaskellName tableName) $ refFieldType
            })
    keyCon = keyConName tableName
    -- this will be ignored if there is already an existing sql=
    -- TODO: I think there is a ! ignore syntax that would screw this up
    setIdName = ["sql=" `mappend` psIdName ps]
takeId _ tableName _ = error $ "empty Id field for " `mappend` show tableName


takeComposite :: [FieldDef]
              -> [Text]
              -> CompositeDef
takeComposite fields pkcols
        = CompositeDef
            (map (getDef fields) pkcols)
            attrs
  where
    (_, attrs) = break ("!" `T.isPrefixOf`) pkcols
    getDef [] t = error $ "Unknown column in primary key constraint: " ++ show t
    getDef (d:ds) t
        | fieldHaskell d == HaskellName t =
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
    | not (T.null n) && isUpper (T.head n)
        = UniqueDef
            (HaskellName n)
            dbName
            (map (HaskellName &&& getDBName defs) fields)
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
      DBName $ psToDBName ps (tableName `T.append` n)
    sqlName :: Maybe DBName
    sqlName =
      case find isSqlName nonFields of
        Nothing ->
          Nothing
        (Just t) ->
          case drop 1 $ T.splitOn "=" t of
            (x : _) -> Just (DBName x)
            _ -> Nothing
    dbName = fromMaybe usualDbName sqlName
    getDBName [] t =
      error $ "Unknown column in unique constraint: " ++ show t
              ++ " " ++ show defs ++ show n ++ " " ++ show attrs
    getDBName (d:ds) t
        | fieldHaskell d == HaskellName t = fieldDB d
        | otherwise = getDBName ds t
takeUniq _ tableName _ xs =
  error $ "invalid unique constraint on table["
          ++ show tableName
          ++ "] expecting an uppercase constraint name xs="
          ++ show xs

data UnboundForeignDef = UnboundForeignDef
                         { _unboundFields :: [Text] -- ^ fields in other entity
                         , _unboundForeignDef :: ForeignDef
                         }

takeForeign :: PersistSettings
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
            = UnboundForeignDef fields $ ForeignDef
                { foreignRefTableHaskell =
                    HaskellName refTableName
                , foreignRefTableDBName =
                    DBName $ psToDBName ps refTableName
                , foreignConstraintNameHaskell =
                    HaskellName n
                , foreignConstraintNameDBName =
                    DBName $ psToDBName ps (tableName `T.append` n)
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
                }
          where
            (fields,attrs) = break ("!" `T.isPrefixOf`) rest
        go ((T.stripPrefix "OnDelete" -> Just onDelete) : rest) onDelete' onUpdate
          = case (onDelete', readEither $ T.unpack onDelete) of
            (Nothing, Right cascadingAction) -> go rest (Just cascadingAction) onUpdate
            (Nothing, Left _) -> error $ errorPrefix ++ "could not parse OnDelete action"
            (Just _, _)  -> error $ errorPrefix ++ "found more than one OnDelete actions"
        go ((T.stripPrefix "OnUpdate" -> Just onUpdate) : rest) onDelete onUpdate'
          = case (onUpdate', readEither $ T.unpack onUpdate) of
            (Nothing, Right cascadingAction) -> go rest onDelete (Just cascadingAction)
            (Nothing, Left _) -> error $ errorPrefix ++ "could not parse OnUpdate action"
            (Just _, _)  -> error $ errorPrefix ++ "found more than one OnUpdate actions"
        go xs _ _ = error $ errorPrefix ++ "expecting a lower case constraint name or a cascading action xs=" ++ show xs

takeDerives :: [Text] -> Maybe [Text]
takeDerives ("deriving":rest) = Just rest
takeDerives _ = Nothing

nullable :: [Text] -> IsNullable
nullable s
    | "Maybe"    `elem` s = Nullable ByMaybeAttr
    | "nullable" `elem` s = Nullable ByNullableAttr
    | otherwise = NotNullable
