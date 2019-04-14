{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Persist.Quasi
    ( parse
    , PersistSettings (..)
    , upperCaseSettings
    , lowerCaseSettings
    , nullable
#if TEST
    , Token (..)
    , tokenize
    , parseFieldType
#endif
    ) where

import Prelude hiding (lines)

import Control.Arrow ((&&&))
import Control.Monad (msum, mplus)
import Data.Char
import Data.List (find, foldl')
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe, maybeToList)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Types

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
parse ps = parseLines ps
      . removeSpaces
      . filter (not . empty)
      . map tokenize
      . T.lines

-- | A token used by the parser.
data Token = Spaces !Int   -- ^ @Spaces n@ are @n@ consecutive spaces.
           | Token Text    -- ^ @Token tok@ is token @tok@ already unquoted.
  deriving (Show, Eq)

-- | Tokenize a string.
tokenize :: Text -> [Token]
tokenize t
    | T.null t = []
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
data Line = Line { lineIndent :: Int
                 , tokens     :: [Text]
                 }

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
    fromToken Spaces{}  = Nothing

-- | Divide lines into blocks and make entity definitions.
parseLines :: PersistSettings -> [Line] -> [EntityDef]
parseLines ps lines =
    fixForeignKeysAll $ toEnts lines
  where
    toEnts (Line indent (name:entattribs) : rest) =
        let (x, y) = span ((> indent) . lineIndent) rest
        in  mkEntityDef ps name entattribs x : toEnts y
    toEnts (Line _ []:rest) = toEnts rest
    toEnts [] = []

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
        case M.lookup (foreignRefTableHaskell fdef) entLookup of
          Just pent -> case entityPrimary pent of
             Just pdef ->
                 if length foreignFieldTexts /= length (compositeFields pdef)
                   then lengthError pdef
                   else let fds_ffs = zipWith (toForeignFields pent)
                                foreignFieldTexts
                                (compositeFields pdef)
                        in  fdef { foreignFields = map snd fds_ffs
                                 , foreignNullable = setNull $ map fst fds_ffs
                                 }
             Nothing ->
                 error $ "no explicit primary key fdef="++show fdef++ " ent="++show ent
          Nothing ->
             error $ "could not find table " ++ show (foreignRefTableHaskell fdef)
               ++ " fdef=" ++ show fdef ++ " allnames="
               ++ show (map (unHaskellName . entityHaskell . unboundEntityDef) unEnts)
               ++ "\n\nents=" ++ show ents
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
        entName
        (DBName $ getDbName ps name' entattribs)
        -- idField is the user-specified Id
        -- otherwise useAutoIdField
        -- but, adjust it if the user specified a Primary
        (setComposite primaryComposite $ fromMaybe autoIdField idField)
        entattribs
        cols
        uniqs
        []
        derives
        extras
        isSum
        comments
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
    cols = mapMaybe (takeColsEx ps) attribs

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
takeColsEx = takeCols (\ft perr -> error $ "Invalid field type " ++ show ft ++ " " ++ perr)

takeCols :: (Text -> String -> Maybe FieldDef) -> PersistSettings -> [Text] -> Maybe FieldDef
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
                then error $ "primary key column cannot be nullable: " ++ show t
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
takeForeign ps tableName _defs (refTableName:n:rest)
    | not (T.null n) && isLower (T.head n)
        = UnboundForeignDef fields $ ForeignDef
            (HaskellName refTableName)
            (DBName $ psToDBName ps refTableName)
            (HaskellName n)
            (DBName $ psToDBName ps (tableName `T.append` n))
            []
            attrs
            False
  where
    (fields,attrs) = break ("!" `T.isPrefixOf`) rest

takeForeign _ tableName _ xs = error $ "invalid foreign key constraint on table[" ++ show tableName ++ "] expecting a lower case constraint name xs=" ++ show xs

takeDerives :: [Text] -> Maybe [Text]
takeDerives ("deriving":rest) = Just rest
takeDerives _ = Nothing

nullable :: [Text] -> IsNullable
nullable s
    | "Maybe"    `elem` s = Nullable ByMaybeAttr
    | "nullable" `elem` s = Nullable ByNullableAttr
    | otherwise = NotNullable
