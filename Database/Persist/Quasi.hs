module Database.Persist.Quasi (persist) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Database.Persist.Base
import Data.Char
import Data.Maybe (mapMaybe)

-- | Converts a quasi-quoted syntax into a list of entity definitions, to be
-- used as input to the template haskell generation code (mkPersist).
persist :: QuasiQuoter
persist = QuasiQuoter
    { quoteExp = lift . parse
    , quotePat = error "Cannot quasi-quote a Persist pattern."
    }

parse :: String -> [EntityDef]
parse = map parse' . nest . map words' . filter (not . null)
      . map killCarriage . lines

killCarriage :: String -> String
killCarriage "" = ""
killCarriage s
    | last s == '\r' = init s
    | otherwise = s

words' :: String -> (Bool, [String])
words' (' ':x) = (True, words x)
words' x = (False, words x)

nest :: [(Bool, [String])] -> [(String, [[String]])]
nest ((False, [name]):rest) =
    let (x, y) = break (not . fst) rest
     in (name, map snd x) : nest y
nest ((False, _):_) = error "First line in block must have exactly one word"
nest ((True, _):_) = error "Blocks must begin with non-indented lines"
nest [] = []

parse' :: (String, [[String]]) -> EntityDef
parse' (name, attribs) = EntityDef name cols uniqs derives
  where
    cols = concatMap takeCols attribs
    uniqs = concatMap takeUniqs attribs
    derives = case mapMaybe takeDerives attribs of
                [] -> ["Show", "Read", "Eq"]
                x -> concat x

takeCols :: [String] -> [(String, String, [String])]
takeCols ("deriving":_) = []
takeCols (n@(f:_):ty:rest)
    | isLower f = [(n, ty, rest)]
takeCols _ = []

takeUniqs :: [String] -> [(String, [String])]
takeUniqs (n@(f:_):rest)
    | isUpper f = [(n, rest)]
takeUniqs _ = []

takeDerives :: [String] -> Maybe [String]
takeDerives ("deriving":rest) = Just rest
takeDerives _ = Nothing
