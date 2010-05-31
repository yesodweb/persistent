module Database.Persist.Quasi
    ( persist
    ) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Database.Persist
import Data.Char

persist :: QuasiQuoter
persist = QuasiQuoter
    { quoteExp = lift . parse
    , quotePat = error "Cannot quasi-quote a Persist pattern."
    }

parse :: String -> [Table]
parse = map parse' . nest . map words' . filter (not . null) . lines

words' :: String -> (Bool, [String])
words' (' ':x) = (True, words x)
words' x = (False, words x)

nest :: [(Bool, [String])] -> [(String, [[String]])]
nest ((False, [name]):rest) =
    let (x, y) = break (not . fst) rest
     in (name, map snd x) : nest y
nest [] = []

parse' :: (String, [[String]]) -> Table
parse' (name, attribs) = Table name cols upds filts ords uniqs
  where
    cols = concatMap takeCols attribs
    upds = concatMap takeUpds attribs
    filts = filter notAllFalse $ concatMap takeFilts attribs
    ords = filter notAllFalse' $ concatMap takeOrds attribs
    uniqs = concatMap takeUniqs attribs

takeCols :: [String] -> [Column]
takeCols (n@(f:_):ty:rest)
    | isLower f = [(n, (ty, "null" `elem` rest))]
takeCols _ = []

takeUpds :: [String] -> [String]
takeUpds (n@(f:_):ty:rest)
    | isLower f && "update" `elem` rest = [n]
takeUpds _ = []

takeFilts :: [String] -> [(String, Bool, Bool, Bool, Bool, Bool, Bool)]
takeFilts (n@(f:_):ty:rest)
    | isLower f = [(n, "Eq" `elem` rest
                     , "Ne" `elem` rest
                     , "Gt" `elem` rest
                     , "Lt" `elem` rest
                     , "Ge" `elem` rest
                     , "Le" `elem` rest
                     )]
takeFilts _ = []

notAllFalse (_, False, False, False, False, False, False) = False
notAllFalse _ = True

takeOrds :: [String] -> [(String, Bool, Bool)]
takeOrds (n@(f:_):ty:rest)
    | isLower f = [(n, "Asc" `elem` rest
                     , "Desc" `elem` rest
                     )]
takeOrds _ = []

notAllFalse' (_, False, False) = False
notAllFalse' _ = True

takeUniqs :: [String] -> [(String, [String])]
takeUniqs (n@(f:_):rest)
    | isUpper f = [(n, rest)]
takeUniqs _ = []
