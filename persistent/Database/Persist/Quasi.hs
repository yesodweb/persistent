{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Quasi
    ( parse
    , PersistSettings (..) -- FIXME
    , sqlSettings
    ) where

import Database.Persist.Base
import Database.Persist.EntityDef
import Data.Char
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow ((&&&))

data PersistSettings = PersistSettings -- FIXME

sqlSettings :: PersistSettings
sqlSettings = PersistSettings

-- | Parses a quasi-quoted syntax into a list of entity definitions.
parse :: PersistSettings -> Text -> [EntityDef]
parse ps = parse'
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
    quotes t front
        | T.null t = error $ T.unpack $ T.concat $
            "Unterminated quoted string starting with " : front []
        | T.head t == '"' = Token (T.concat $ front []) : tokenize (T.tail t)
        | T.head t == '\\' && T.length t > 1 =
            quotes (T.drop 2 t) (front . (T.take 2 t:))
        | otherwise =
            let (x, y) = T.break (`elem` "\\\"") t
             in quotes y (front . (x:))

-- | A string of tokens is empty when it has only spaces.  There
-- can't be two consecutive 'Spaces', so this takes /O(1)/ time.
empty :: [Token] -> Bool
empty []         = True
empty [Spaces _] = True
empty _          = False

-- | A line.  We don't care about spaces in the middle of the
-- line.  Also, we don't care about the ammount of indentation.
data Line = Line { lineType :: LineType
                 , tokens   :: [Text]
                 }

-- | A line may be part of a header or body.
data LineType = Header | Body
                deriving (Eq)

-- | Remove leading spaces and remove spaces in the middle of the
-- tokens.
removeSpaces :: [[Token]] -> [Line]
removeSpaces xs = map (makeLine . subtractSpace) xs
    where
      -- | Ammount of leading spaces.
      s = minimum $ map headSpace xs

      -- | Ammount of leading space in a single token string.
      headSpace (Spaces n : _) = n
      headSpace _              = 0

      -- | Subtract the leading space.
      subtractSpace ys | s == 0 = ys
      subtractSpace (Spaces n : rest)
          | n == s    = rest
          | otherwise = Spaces (n - s) : rest
      subtractSpace _ = error "Database.Persist.Quasi: never here"

      -- | Get all tokens while ignoring spaces.
      getTokens (Token tok : rest) = tok : getTokens rest
      getTokens (Spaces _  : rest) =       getTokens rest
      getTokens []                 = []

      -- | Make a 'Line' from a @[Token]@.
      makeLine (Spaces _ : rest) = Line Body   (getTokens rest)
      makeLine rest              = Line Header (getTokens rest)

-- | Divide lines into blocks and make entity definitions.
parse' :: [Line] -> [EntityDef]
parse' (Line Header (name:entattribs) : rest) =
    let (x, y) = span ((== Body) . lineType) rest
    in mkEntityDef name entattribs (map tokens x) : parse' y
parse' ((Line Header []) : _) =
    error "Indented line must contain at least name."
parse' ((Line Body _)    : _) =
    error "Blocks must begin with non-indented lines."
parse' [] = []

type RawLine = [Text]

-- | Construct an entity definition.
mkEntityDef :: Text -- ^ name
            -> [Attr] -- ^ entity attributes
            -> [RawLine] -- ^ indented lines
            -> EntityDef
mkEntityDef name entattribs attribs =
    EntityDef
        (HaskellName name)
        (DBName name) -- FIXME
        (DBName "id") -- FIXME
        entattribs cols uniqs derives
  where
    cols = mapMaybe takeCols attribs
    uniqs = mapMaybe takeUniqs attribs
    derives = case mapMaybe takeDerives attribs of
                [] -> ["Show", "Read", "Eq"]
                x -> concat x

takeCols :: [Text] -> Maybe FieldDef
takeCols ("deriving":_) = Nothing
takeCols (n:ty:rest)
    | not (T.null n) && isLower (T.head n) = Just $ FieldDef
        (HaskellName n)
        (DBName n) -- FIXME
        (FieldType ty)
        rest
takeCols _ = Nothing

takeUniqs :: [Text] -> Maybe UniqueDef
takeUniqs (n:rest)
    | not (T.null n) && isUpper (T.head n)
        = Just $ UniqueDef
            (HaskellName n)
            (DBName n {- FIXME -})
            (map (HaskellName &&& DBName) rest) -- FIXME
takeUniqs _ = Nothing

takeDerives :: [Text] -> Maybe [Text]
takeDerives ("deriving":rest) = Just rest
takeDerives _ = Nothing
