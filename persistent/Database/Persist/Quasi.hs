{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
module Database.Persist.Quasi
    ( parse
    , PersistSettings (..)
    , upperCaseSettings
    , lowerCaseSettings
    , stripId
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
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.List (foldl')

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
parse :: PersistSettings -> Text -> [EntityDef ()]
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
    | otherwise =
        let (token, rest) = T.break isSpace t
         in Token token : tokenize rest
  where
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
parseLines :: PersistSettings -> [Line] -> [EntityDef ()]
parseLines ps lines =
    toEnts lines
  where
    toEnts (Line indent (name:entattribs) : rest) =
        let (x, y) = span ((> indent) . lineIndent) rest
         in mkEntityDef ps name entattribs x : toEnts y
    toEnts (Line _ []:rest) = toEnts rest
    toEnts [] = []

-- | Construct an entity definition.
mkEntityDef :: PersistSettings
            -> Text -- ^ name
            -> [Attr] -- ^ entity attributes
            -> [Line] -- ^ indented lines
            -> EntityDef ()
mkEntityDef ps name entattribs lines =
    EntityDef
        (HaskellName name')
        (DBName $ getDbName ps name' entattribs)
        (DBName $ idName entattribs)
        entattribs cols uniqs derives
        extras
        isSum
  where
    (isSum, name') =
        case T.uncons name of
            Just ('+', x) -> (True, x)
            _ -> (False, name)
    (attribs, extras) = splitExtras lines
    idName [] = "id"
    idName (t:ts) =
        case T.stripPrefix "id=" t of
            Nothing -> idName ts
            Just s -> s
    uniqs = mapMaybe (takeUniqs ps name' cols) attribs
    derives = concat $ mapMaybe takeDerives attribs

    cols :: [FieldDef ()]
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

takeCols :: PersistSettings -> [Text] -> Maybe (FieldDef ())
takeCols _ ("deriving":_) = Nothing
takeCols ps (n':typ:rest)
    | not (T.null n) && isLower (T.head n) =
        case parseFieldType typ of
            Nothing -> error $ "Invalid field type: " ++ show typ
            Just ft -> Just FieldDef
                { fieldHaskell = HaskellName n
                , fieldDB = DBName $ getDbName ps n rest
                , fieldType = ft
                , fieldSqlType = ()
                , fieldAttrs = rest
                , fieldStrict = fromMaybe (psStrictFields ps) mstrict
                , fieldEmbedded = Nothing
                , fieldManyDB = []
                }
  where
    (mstrict, n)
        | Just x <- T.stripPrefix "!" n' = (Just True, x)
        | Just x <- T.stripPrefix "~" n' = (Just False, x)
        | otherwise = (Nothing, n')
takeCols _ _ = Nothing

getDbName :: PersistSettings -> Text -> [Text] -> Text
getDbName ps n [] = psToDBName ps n
getDbName ps n (a:as) =
    case T.stripPrefix "sql=" a of
      Nothing -> getDbName ps n as
      Just s  -> s

takeUniqs :: PersistSettings
          -> Text
          -> [FieldDef a]
          -> [Text]
          -> Maybe UniqueDef
takeUniqs ps tableName defs (n:rest)
    | not (T.null n) && isUpper (T.head n)
        = Just $ UniqueDef
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
takeUniqs _ _ _ _ = Nothing

takeDerives :: [Text] -> Maybe [Text]
takeDerives ("deriving":rest) = Just rest
takeDerives _ = Nothing

stripId :: FieldType -> Maybe Text
stripId (FTTypeCon Nothing t) = T.stripSuffix "Id" t
stripId _ = Nothing

nullable :: [Text] -> IsNullable
nullable s
    | "Maybe"    `elem` s = Nullable ByMaybeAttr
    | "nullable" `elem` s = Nullable ByNullableAttr
    | otherwise = NotNullable
