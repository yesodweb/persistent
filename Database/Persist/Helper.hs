{-# LANGUAGE TemplateHaskell #-}
module Database.Persist.Helper
    ( recName
    , upperFirst
    , dataTypeDec
    , keyTypeDec
    , filterTypeDec
    , filtsToList
    , updateTypeDec
    , orderTypeDec
    , uniqueTypeDec
    ) where

import Database.Persist
import Language.Haskell.TH.Syntax
import Data.Char (toLower, toUpper)
import Data.Maybe (fromJust)

recName :: String -> String -> String
recName dt f = map toLower dt ++ upperFirst f

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs
upperFirst [] = []

dataTypeDec :: Table -> Dec
dataTypeDec t =
    let name = mkName $ tableName t
        cols = map (mkCol $ tableName t) $ tableColumns t
     in DataD [] name [] [RecC name cols] []
  where
    mkCol x (n, ty) =
        (mkName $ recName x n, NotStrict, ConT $ mkName ty)

keyTypeDec :: String -> String -> Table -> Type -> Dec
keyTypeDec constr typ t monad =
    NewtypeInstD [] ''Key [ConT $ mkName $ tableName t, monad]
                (NormalC (mkName constr) [(NotStrict, ConT $ mkName typ)])
                [''Show, ''Read, ''Num, ''Integral, ''Enum, ''Eq, ''Ord,
                 ''Real]

filterTypeDec :: Table -> Type -> Dec
filterTypeDec t monad =
    DataInstD [] ''Filter [ConT $ mkName $ tableName t, monad]
                (concatMap (mkFilter (tableName t) (tableColumns t))
                  $ tableFilters t)
                [''Show, ''Read, ''Eq]

mkFilter :: String
         -> [(String, String)]
         -> (String, Bool, Bool, Bool, Bool, Bool, Bool)
         -> [Con]
mkFilter x cols filts = map go $ filtsToList filts
  where
    go (s, t) = NormalC (mkName $ x ++ upperFirst s ++ t)
                               [(NotStrict, ConT $ mkName $ ty s)]
    ty s = case lookup s cols of
                Nothing -> error $ "Invalid column: " ++ s
                Just ty' -> ty'

updateTypeDec :: Table -> Type -> Dec
updateTypeDec t monad =
    DataInstD [] ''Update [ConT $ mkName $ tableName t, monad]
                (map (mkUpdate (tableName t) (tableColumns t)) (tableUpdates t))
                [''Show, ''Read, ''Eq]

mkUpdate :: String -> [(String, String)] -> String -> Con
mkUpdate x cols s =
    NormalC (mkName $ x ++ upperFirst s)
                [(NotStrict, ConT $ mkName ty)]
  where
    ty = case lookup s cols of
                Nothing -> error $ "Invalid column: " ++ s
                Just ty' -> ty'

orderTypeDec :: Table -> Type -> Dec
orderTypeDec t monad =
    DataInstD [] ''Order [ConT $ mkName $ tableName t, monad]
                (concatMap (mkOrder $ tableName t) (tableOrders t))
                [''Show, ''Read, ''Eq]

mkOrder :: String -> (String, Bool, Bool) -> [Con]
mkOrder x (s, a, d) =
     (if a then (:) (go "Asc") else id)
   $ (if d then (:) (go "Desc") else id) []
  where
    go ad = NormalC (mkName $ x ++ upperFirst s ++ ad) []

uniqueTypeDec :: Table -> Type -> Dec
uniqueTypeDec t monad =
    DataInstD [] ''Unique [ConT $ mkName $ tableName t, monad]
                (map (mkUnique t) $ tableUniques t)
                [''Show, ''Read, ''Eq]

mkUnique :: Table -> (String, [String]) -> Con
mkUnique t (constr, fields) =
    NormalC (mkName constr) types
  where
    types = map (go . fromJust . flip lookup (tableColumns t)) fields
    go x = (NotStrict, ConT $ mkName x)

filtsToList :: (String, Bool, Bool, Bool, Bool, Bool, Bool)
            -> [(String, String)]
filtsToList (s, a, b, c, d, e, f)
    = go $ zip ["Eq", "Ne", "Gt", "Lt", "Ge", "Le"] [a, b, c, d, e, f]
  where
    go [] = []
    go ((_, False):rest) = go rest
    go ((x, True):rest) = (s, x) : go rest
