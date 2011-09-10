module Database.Persist.Quasi
    ( parse
    ) where

import Database.Persist.Base
import Data.Char
import Data.Maybe (mapMaybe)

-- | Parses a quasi-quoted syntax into a list of entity definitions.
parse :: String -> [EntityDef]
parse = parse'
      . removeSpaces
      . filter (not . empty)
      . map tokenize
      . lines

-- | A token used by the parser.
data Token = Spaces !Int   -- ^ @Spaces n@ are @n@ consecutive spaces.
           | Token String  -- ^ @Token tok@ is token @tok@ already unquoted.

-- | Tokenize a string.
tokenize :: String -> [Token]
tokenize [] = []
tokenize ('-':'-':_) = [] -- Comment until the end of the line.
tokenize ('"':xs) = go xs ""
    where
      go ('\"':rest) acc = Token (reverse acc) : tokenize rest
      go ('\\':y:ys) acc = go ys (y:acc)
      go (y:ys)      acc = go ys (y:acc)
      go []          acc = error $ "Unterminated quoted (\") string starting with " ++
                                   show (reverse acc) ++ "."
tokenize (x:xs)
    | isSpace x = let (spaces, rest) = span isSpace xs
                  in Spaces (1 + length spaces) : tokenize rest
tokenize xs = let (token, rest) = break isSpace xs
              in Token token : tokenize rest

-- | A string to tokens is empty when it has only spaces.  There
-- can't be two consecutive 'Spaces', so this takes /O(1)/ time.
empty :: [Token] -> Bool
empty []         = True
empty [Spaces _] = True
empty _          = False

-- | A line.  We don't care about spaces in the middle of the
-- line.  Also, we don't care about the ammount of indentation.
data Line = Line { lineType :: LineType
                 , tokens   :: [String] }

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

      -- | Subtract leading space.
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
parse' ((Line Header []) : _) = error "Indented line must contain at least name."
parse' ((Line Body _)    : _) = error "Blocks must begin with non-indented lines."
parse' [] = []

-- | Construct an entity definition.
mkEntityDef :: String -> [String] -> [[String]] -> EntityDef
mkEntityDef name entattribs attribs =
    EntityDef name entattribs cols uniqs derives
  where
    cols = mapMaybe takeCols attribs
    uniqs = mapMaybe takeUniqs attribs
    derives = case mapMaybe takeDerives attribs of
                [] -> ["Show", "Read", "Eq"]
                x -> concat x

takeCols :: [String] -> Maybe ColumnDef
takeCols ("deriving":_) = Nothing
takeCols (n@(f:_):ty:rest)
    | isLower f = Just $ ColumnDef n ty rest
takeCols _ = Nothing

takeUniqs :: [String] -> Maybe UniqueDef
takeUniqs (n@(f:_):rest)
    | isUpper f = Just $ UniqueDef n rest
takeUniqs _ = Nothing

takeDerives :: [String] -> Maybe [String]
takeDerives ("deriving":rest) = Just rest
takeDerives _ = Nothing
