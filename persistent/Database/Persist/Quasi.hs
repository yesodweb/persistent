{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Quasi
    ( parse
    , PersistSettings (..)
    , upperCaseSettings
    , lowerCaseSettings
    ) where

import Prelude hiding (lines)
import Database.Persist.EntityDef
import Data.Char
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow ((&&&))
import qualified Data.Map as M
import qualified Data.Set as S

data PersistSettings = PersistSettings
    { psToDBName :: Text -> Text
    }

upperCaseSettings :: PersistSettings
upperCaseSettings = PersistSettings
    { psToDBName = id
    }

lowerCaseSettings :: PersistSettings
lowerCaseSettings = PersistSettings
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

-- | Tokenize a string.
tokenize :: Text -> [Token]
tokenize t
    | T.null t = []
    | "--" `T.isPrefixOf` t = [] -- Comment until the end of the line.
    | T.head t == '"' = quotes (T.tail t) id
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
            quotes (T.drop 2 t') (front . (T.take 2 t':))
        | otherwise =
            let (x, y) = T.break (`elem` "\\\"") t'
             in quotes y (front . (x:))

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
    let entNames = S.fromList $ mapMaybe entName lines
    in toEntities entNames
  where
    entName (Line _ (name:_)) = Just name
    entName _ = Nothing

    toEntities entNames = toEnts lines
      where
        toEnts (Line indent (name:entattribs) : rest) =
            let (x, y) = span ((> indent) . lineIndent) rest
             in mkEntityDef ps entNames name entattribs x : toEnts y
        toEnts (Line _ []:rest) = toEnts rest
        toEnts [] = []

-- | Construct an entity definition.
mkEntityDef :: PersistSettings
            -> S.Set Text -- ^ Entity names
            -> Text -- ^ name
            -> [Attr] -- ^ entity attributes
            -> [Line] -- ^ indented lines
            -> EntityDef
mkEntityDef ps entityNames name entattribs lines =
    EntityDef
        (HaskellName name)
        (DBName $ psToDBName ps name)
        (DBName $ idName entattribs)
        entattribs cols uniqs derives
        extras
  where
    (attribs, extras) = splitExtras lines
    idName [] = "id"
    idName (t:ts) =
        case T.stripPrefix "id=" t of
            Nothing -> idName ts
            Just s -> s
    uniqs = mapMaybe (takeUniqs ps cols) attribs
    derives = case mapMaybe takeDerives attribs of
                [] -> ["Show", "Read", "Eq"]
                x -> concat x
    cols :: [FieldDef]
    cols = map toSimple noSimpleCols
      where
        toSimple fd@FieldDef { fieldType = (EmbedNone t) }
            | S.member t entityNames = fd { fieldType = (EmbedSimple t) }
        toSimple n = n

    noSimpleCols :: [FieldDef]
    noSimpleCols   = mapMaybe (takeCols ps) attribs
    {-embedNoneStr x =-}
        {-case x of-}
          {-EmbedNone str -> Just str-}
          {-_ -> Nothing-}

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
takeCols ps (n:typ:rest)
    | not (T.null n) && isLower (T.head n) =
      let (rst, ft) = checkEmbed
      in Just $ FieldDef
          (HaskellName n)
          (DBName $ db rest)
          ft
          rst
  where
    checkEmbed | T.head typ == '[' && T.last typ == ']' =
                   (rest, EmbedList (T.init $ T.tail typ))
               | typ == "Set" = (tail rest, EmbedSet (head rest))
               -- don't know yet if it is an EmbedSimple
               | otherwise = (rest, EmbedNone typ)

    db [] = psToDBName ps n
    db (a:as) =
        case T.stripPrefix "sql=" a of
            Nothing -> db as
            Just s -> s
takeCols _ _ = Nothing

takeUniqs :: PersistSettings
          -> [FieldDef]
          -> [Text]
          -> Maybe UniqueDef
takeUniqs ps defs (n:rest)
    | not (T.null n) && isUpper (T.head n)
        = Just $ UniqueDef
            (HaskellName n)
            (DBName $ psToDBName ps n)
            (map (HaskellName &&& getDBName defs) rest)
  where
    getDBName [] t = error $ "Unknown column in unique constraint: " ++ show t
    getDBName (d:ds) t
        | fieldHaskell d == HaskellName t = fieldDB d
        | otherwise = getDBName ds t
takeUniqs _ _ _ = Nothing

takeDerives :: [Text] -> Maybe [Text]
takeDerives ("deriving":rest) = Just rest
takeDerives _ = Nothing
