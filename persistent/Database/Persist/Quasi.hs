{-# LANGUAGE OverloadedStrings #-}
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
import Database.Persist.Types
import Data.Char
import Data.Maybe (mapMaybe, fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.List (foldl',find)
import Data.Monoid (mappend)

data ParseState a = PSDone | PSFail | PSSuccess a Text

parseFieldType :: Text -> Maybe FieldType
parseFieldType t0 =
    case go t0 of
        PSSuccess ft t'
            | T.all isSpace t' -> Just ft
        _ -> Nothing
  where
    go t =
        case goMany id t of
            PSSuccess (ft:fts) t' -> PSSuccess (foldl' FTApp ft fts) t'
            PSSuccess [] _ -> PSFail
            PSFail -> PSFail
            PSDone -> PSDone
    go1 t =
        case T.uncons t of
            Nothing -> PSDone
            Just (c, t')
                | isSpace c -> go1 $ T.dropWhile isSpace t'
                | c == '(' ->
                    case go t' of
                        PSSuccess ft t'' ->
                            case T.uncons $ T.dropWhile isSpace t'' of
                                Just (')', t''') -> PSSuccess ft t'''
                                _ -> PSFail
                        _ -> PSFail
                | c == '[' ->
                    case go t' of
                        PSSuccess ft t'' ->
                            case T.uncons $ T.dropWhile isSpace t'' of
                                Just (']', t''') -> PSSuccess (FTList ft) t'''
                                _ -> PSFail
                        _ -> PSFail
                | isUpper c ->
                    let (a, b) = T.break (\x -> isSpace x || x `elem` "()[]") t
                     in PSSuccess (getCon a) b
                | otherwise -> PSFail
    getCon t =
        case T.breakOnEnd "." t of
            (_, "") -> FTTypeCon Nothing t
            ("", _) -> FTTypeCon Nothing t
            (a, b) -> FTTypeCon (Just $ T.init a) b
    goMany front t =
        case go1 t of
            PSSuccess x t' -> goMany (front . (x:)) t'
            _ -> PSSuccess (front []) t

data PersistSettings = PersistSettings
    { psToDBName :: !(Text -> Text)
    , psStrictFields :: !Bool
    -- ^ Whether fields are by default strict. Default value: @True@.
    --
    -- Since 1.2
    }

upperCaseSettings :: PersistSettings
upperCaseSettings = PersistSettings
    { psToDBName = id
    , psStrictFields = True
    }

lowerCaseSettings :: PersistSettings
lowerCaseSettings = PersistSettings
    { psToDBName =
        let go c
                | isUpper c = T.pack ['_', toLower c]
                | otherwise = T.singleton c
         in T.dropWhile (== '_') . T.concatMap go
    , psStrictFields = True
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
            let (x, y) = T.break (`elem` "\\\"") t'
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
            let (x, y) = T.break (`elem` "\\()") t'
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
                 if length foreignFieldTexts /= length (primaryFields pdef)
                   then lengthError pdef
                   else let fds_ffs = zipWith (toForeignFields ent pent)
                                foreignFieldTexts
                                (primaryFields pdef)
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

        toForeignFields ent pent fieldText pfd =
           case chktypes fd haskellField (entityFields pent) pfh of
               Just err -> error err
               Nothing -> (fd, ((haskellField, fieldDB fd), (pfh, pfdb)))
          where
            fd = getFd (entityFields ent) haskellField

            haskellField = HaskellName fieldText
            (pfh, pfdb) = (fieldHaskell pfd, fieldDB pfd)

            chktypes :: FieldDef -> HaskellName -> [FieldDef] -> HaskellName -> Maybe String
            chktypes ffld fkey pflds pkey =
                if fieldType ffld == fieldType pfld then Nothing
                  else Just $ "fieldType mismatch: " ++ show (fieldType ffld) ++ ", " ++ show (fieldType pfld)
              where
                pfld = getFd pflds pkey

            entName = entityHaskell ent
            getFd [] t = error $ "foreign key constraint for: " ++ show (unHaskellName entName)
                           ++ " unknown column: " ++ show t
            getFd (fd:fds) t
                | fieldHaskell fd == t = fd
                | otherwise = getFd fds t

        lengthError pdef = error $ "found " ++ show (length foreignFieldTexts) ++ " fkeys and " ++ show (length (primaryFields pdef)) ++ " pkeys: fdef=" ++ show fdef ++ " pdef=" ++ show pdef


data UnboundEntityDef = UnboundEntityDef
                        { unboundForeignDefs :: [UnboundForeignDef]
                        , unboundEntityDef :: EntityDef
                        }

-- | Construct an entity definition.
mkEntityDef :: PersistSettings
            -> Text -- ^ name
            -> [Attr] -- ^ entity attributes
            -> [Line] -- ^ indented lines
            -> UnboundEntityDef
mkEntityDef ps name entattribs lines = UnboundEntityDef foreigns $
    EntityDef
        (HaskellName name')
        (DBName $ getDbName ps name' entattribs)
        (DBName $ idName entattribs)
        entattribs cols primary uniqs [] derives
        extras
        isSum
  where
    (isSum, name') =
        case T.uncons name of
            Just ('+', x) -> (True, x)
            _ -> (False, name)
    (attribs, extras) = splitExtras lines
    idName [] = "id"
    idName (t:ts) = fromMaybe (idName ts) $ T.stripPrefix "id=" t
            
    (primarys, uniqs, foreigns) = foldl' (\(a,b,c) attr -> 
                                    let (a',b',c') = takeConstraint ps name' cols attr 
                                        squish xs m = xs `mappend` maybeToList m
                                    in (squish a a', squish b b', squish c c')) ([],[],[]) attribs
                                    
    primary = case primarys of 
                []  -> Nothing 
                [p] -> Just p
                _ -> error $ "found more than one primary key in table[" ++ show name' ++ "]"
                
    derives = concat $ mapMaybe takeDerives attribs

    cols :: [FieldDef]
    cols = mapMaybe (takeCols ps) attribs

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

takeCols :: PersistSettings -> [Text] -> Maybe FieldDef
takeCols _ ("deriving":_) = Nothing
takeCols ps (n':typ:rest)
    | not (T.null n) && isLower (T.head n) =
        case parseFieldType typ of
            Nothing -> error $ "Invalid field type: " ++ show typ
            Just ft -> Just FieldDef
                { fieldHaskell = HaskellName n
                , fieldDB = DBName $ getDbName ps n rest
                , fieldType = ft
                , fieldSqlType = SqlOther $ "SqlType unset for " `mappend` n
                , fieldAttrs = rest
                , fieldStrict = fromMaybe (psStrictFields ps) mstrict
                , fieldReference = NoReference
                }
  where
    (mstrict, n)
        | Just x <- T.stripPrefix "!" n' = (Just True, x)
        | Just x <- T.stripPrefix "~" n' = (Just False, x)
        | otherwise = (Nothing, n')
takeCols _ _ = Nothing

getDbName :: PersistSettings -> Text -> [Text] -> Text
getDbName ps n [] = psToDBName ps n
getDbName ps n (a:as) = fromMaybe (getDbName ps n as) $ T.stripPrefix "sql=" a

takeConstraint :: PersistSettings
          -> Text
          -> [FieldDef]
          -> [Text]
          -> (Maybe PrimaryDef, Maybe UniqueDef, Maybe UnboundForeignDef)
takeConstraint ps tableName defs (n:rest) | not (T.null n) && isUpper (T.head n) = takeConstraint' 
    where takeConstraint' 
            | n == "Primary" = (Just $ takePrimary defs rest, Nothing, Nothing)
            | n == "Unique"  = (Nothing, Just $ takeUniq ps tableName defs rest, Nothing)
            | n == "Foreign" = (Nothing, Nothing, Just $ takeForeign ps tableName defs rest)
            | otherwise      = (Nothing, Just $ takeUniq ps "" defs (n:rest), Nothing) -- retain compatibility with original unique constraint
takeConstraint _ _ _ _ = (Nothing, Nothing, Nothing)
    
takePrimary :: [FieldDef]
            -> [Text]
            -> PrimaryDef
takePrimary defs pkcols
        = PrimaryDef
            (map (getDef defs) pkcols)
            attrs
  where
    (_, attrs) = break ("!" `T.isPrefixOf`) pkcols
    getDef [] t = error $ "Unknown column in primary key constraint: " ++ show t
    getDef (d:ds) t
        | nullable (fieldAttrs d) /= NotNullable = error $ "primary key column cannot be nullable: " ++ show t
        | fieldHaskell d == HaskellName t = d
        | otherwise = getDef ds t

-- Unique UppercaseConstraintName list of lowercasefields    
takeUniq :: PersistSettings
          -> Text
          -> [FieldDef]
          -> [Text]
          -> UniqueDef
takeUniq ps tableName defs (n:rest)
    | not (T.null n) && isUpper (T.head n)
        = UniqueDef
            (HaskellName n)
            (DBName $ psToDBName ps (tableName `T.append` n))
            (map (HaskellName &&& getDBName defs) fields)
            attrs
  where
    (fields,attrs) = break ("!" `T.isPrefixOf`) rest
    getDBName [] t = error $ "Unknown column in unique constraint: " ++ show t
    getDBName (d:ds) t
        | fieldHaskell d == HaskellName t = fieldDB d
        | otherwise = getDBName ds t
takeUniq _ tableName _ xs = error $ "invalid unique constraint on table[" ++ show tableName ++ "] expecting an uppercase constraint name xs=" ++ show xs

data UnboundForeignDef = UnboundForeignDef [Text] -- ^ fields in other entity
                                           ForeignDef
takeForeign :: PersistSettings
          -> Text
          -> [FieldDef]
          -> [Text]
          -> UnboundForeignDef
takeForeign ps tableName defs (refTableName:n:rest)
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
