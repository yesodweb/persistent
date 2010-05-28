module Database.Persist.QuasiYaml
    ( persist
    ) where

import Data.Object
import Data.Object.Yaml
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Database.Persist
import Data.ByteString.Char8 (pack)
import System.IO.Unsafe

persist :: QuasiQuoter
persist = QuasiQuoter
    { quoteExp = lift . parse
    , quotePat = error "Cannot quasi-quote a Persist pattern."
    }

parse :: String -> [Table]
parse s = unsafePerformIO $ do -- just for errors, not as bad as you think
    so <- decode $ pack s
    m <- fromMapping so
    mapM parseTable m

parseTable (name, Mapping m) = do
    columns' <- lookupSequence "columns" m
    x <- mapM parseColumn columns'
    let columns = map (\(a, _, _, _) -> a) x
        updates = concatMap (\(_, b, _, _) -> b) x
        filters = concatMap (\(_, _, c, _) -> c) x
        orders =  concatMap (\(_, _, _, d) -> d) x
    uniques <- case lookup "uniques" m of
                Just (Mapping uniques') -> mapM parseUnique uniques'
                _ -> return []
    return $ Table name columns updates filters orders uniques

parseColumn (Mapping m) = do
    n <- lookupScalar "name" m
    ty <- lookupScalar "type" m
    let nullable = lookup "nullable" m == Just (Scalar "True")
    let upd = if lookup "update" m == Just (Scalar "True")
                then [n]
                else []
    let filt = case lookup "filter" m of
                Just (Sequence filt') -> [parseFilter n filt']
                _ -> []
    let ord = case lookup "order" m of
                Just (Sequence ord') -> [parseOrder n ord']
                _ -> []
    return ((n, (ty, nullable)), upd, filt, ord)

parseFilter n l =
    (n, Scalar "Eq" `elem` l, Scalar "Ne" `elem` l,
        Scalar "Lt" `elem` l, Scalar "Gt" `elem` l,
        Scalar "Le" `elem` l, Scalar "Ge" `elem` l)

parseOrder n l = (n, Scalar "Asc" `elem` l, Scalar "Desc" `elem` l)

parseUnique (n, Sequence fields) = do
    fs <- mapM fromScalar fields
    return (n, fs)
