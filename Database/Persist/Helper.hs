{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module Database.Persist.Helper
    ( recName
    , upperFirst
    , filtsToList
    , ordsToList
      -- * TH datatype helpers
    , dataTypeDec
    , keyTypeDec
    , filterTypeDec
    , updateTypeDec
    , orderTypeDec
    , uniqueTypeDec
      -- * TH typeclass helpers
    , mkToPersistables
    , mkToFieldNames
    , mkToFieldName
    , mkPersistable
    , mkToFilter
    , mkToOrder
    , mkHalfDefined
      -- * Type classes
    , SomePersistable (..)
    , ToPersistables (..)
    , FromPersistValues (..)
    , toPersistValues
    , ToFieldNames (..)
    , ToOrder (..)
    , PersistOrder (..)
    , ToFieldName (..)
    , PersistFilter (..)
    , ToFilter (..)
    , HalfDefined (..)
      -- * Utils
    , apE
    , addIsNullable
    ) where

import Database.Persist
import Language.Haskell.TH.Syntax
import Data.Char (toLower, toUpper)
import Data.Maybe (fromJust)
import Web.Routes.Quasi (SinglePiece)

recName :: String -> String -> String
recName dt f = lowerFirst dt ++ upperFirst f

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst [] = []

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs
upperFirst [] = []

dataTypeDec :: Table -> Dec
dataTypeDec t =
    let name = mkName $ tableName t
        cols = map (mkCol $ tableName t) $ tableColumns t
     in DataD [] name [] [RecC name cols] $ map mkName $ tableDerives t
  where
    mkCol x (n, ty) = (mkName $ recName x n, NotStrict, pairToType ty)

keyTypeDec :: String -> String -> Table -> Dec
keyTypeDec constr typ t =
    NewtypeInstD [] ''Key [ConT $ mkName $ tableName t]
                (NormalC (mkName constr) [(NotStrict, ConT $ mkName typ)])
                [''Show, ''Read, ''Num, ''Integral, ''Enum, ''Eq, ''Ord,
                 ''Real, ''Persistable, ''SinglePiece]

filterTypeDec :: Table -> Dec
filterTypeDec t =
    DataInstD [] ''Filter [ConT $ mkName $ tableName t]
            (concatMap (mkFilter (tableName t) (tableColumns t))
              $ tableFilters t)
            (if null (tableFilters t) then [] else [''Show, ''Read, ''Eq])

mkFilter :: String
         -> [(String, (String, Bool))]
         -> (String, Bool, Bool, Bool, Bool, Bool, Bool)
         -> [Con]
mkFilter x cols filts = map go $ filtsToList filts
  where
    go (s, t) = NormalC (mkName $ x ++ upperFirst s ++ t)
                               [(NotStrict, pairToType $ ty s)]
    ty s = case lookup s cols of
                Nothing -> error $ "Invalid column: " ++ s
                Just ty' -> ty'

updateTypeDec :: Table -> Dec
updateTypeDec t =
    DataInstD [] ''Update [ConT $ mkName $ tableName t]
        (map (mkUpdate (tableName t) (tableColumns t)) (tableUpdates t))
        (if null (tableUpdates t) then [] else [''Show, ''Read, ''Eq])

mkUpdate :: String -> [(String, (String, Bool))] -> String -> Con
mkUpdate x cols s =
    NormalC (mkName $ x ++ upperFirst s)
                [(NotStrict, pairToType ty)]
  where
    ty = case lookup s cols of
                Nothing -> error $ "Invalid column: " ++ s
                Just ty' -> ty'

orderTypeDec :: Table -> Dec
orderTypeDec t =
    DataInstD [] ''Order [ConT $ mkName $ tableName t]
            (concatMap (mkOrder $ tableName t) (tableOrders t))
            (if null (tableOrders t) then [] else [''Show, ''Read, ''Eq])

mkOrder :: String -> (String, Bool, Bool) -> [Con]
mkOrder x (s, a, d) =
     (if a then (:) (go "Asc") else id)
   $ (if d then (:) (go "Desc") else id) []
  where
    go ad = NormalC (mkName $ x ++ upperFirst s ++ ad) []

uniqueTypeDec :: Table -> Dec
uniqueTypeDec t =
    DataInstD [] ''Unique [ConT $ mkName $ tableName t]
            (map (mkUnique t) $ tableUniques t)
            (if null (tableUniques t) then [] else [''Show, ''Read, ''Eq])

mkUnique :: Table -> (String, [String]) -> Con
mkUnique t (constr, fields) =
    NormalC (mkName constr) types
  where
    types = map (go . fromJust . flip lookup (tableColumns t)) fields
    go (_, True) = error "Error: cannot have nullables in unique"
    go x = (NotStrict, pairToType x)

filtsToList :: (String, Bool, Bool, Bool, Bool, Bool, Bool)
            -> [(String, String)]
filtsToList (s, a, b, c, d, e, f)
    = go $ zip ["Eq", "Ne", "Gt", "Lt", "Ge", "Le"] [a, b, c, d, e, f]
  where
    go [] = []
    go ((_, False):rest) = go rest
    go ((x, True):rest) = (s, x) : go rest

ordsToList :: (String, Bool, Bool) -> [(String, String)]
ordsToList (s, a, b) = go $ zip ["Asc", "Desc"] [a, b]
  where
    go [] = []
    go ((_, False):rest) = go rest
    go ((x, True):rest) = (s, x) : go rest

pairToType :: (String, Bool) -> Type
pairToType (s, False) = ConT $ mkName s
pairToType (s, True) = ConT (mkName "Maybe") `AppT` ConT (mkName s)

data SomePersistable = forall a. Persistable a => SomePersistable a
instance Persistable SomePersistable where
    toPersistValue (SomePersistable a) = toPersistValue a
    fromPersistValue x = fmap SomePersistable (fromPersistValue x :: Either String String)
    sqlType (SomePersistable a) = sqlType a

class ToPersistables a where
    toPersistables :: a -> [SomePersistable]

degen :: [Clause] -> [Clause]
degen [] =
    let err = VarE (mkName "error") `AppE` LitE (StringL
                "Degenerate case, should never happen")
     in [Clause [WildP] (NormalB err) []]
degen x = x

mkToPersistables :: Type
                 -> [(String, Int)]
                 -> Q Dec
mkToPersistables typ pairs = do
    clauses <- mapM go pairs
    return $ InstanceD [] (ConT ''ToPersistables `AppT` typ)
                [FunD (mkName "toPersistables") $ degen clauses]
  where
    go (constr, fields) = do
        xs <- sequence $ replicate fields $ newName "x"
        let pat = ConP (mkName constr) $ map VarP xs
        sp <- [|SomePersistable|]
        let bod = ListE $ map (AppE sp . VarE) xs
        return $ Clause [pat] (NormalB bod) []

class FromPersistValues a where
    fromPersistValues :: [PersistValue] -> Either String a

toPersistValues :: ToPersistables a => a -> [PersistValue]
toPersistValues = map toPersistValue . toPersistables

class ToFieldNames a where
    toFieldNames :: a -> [String]

mkToFieldNames :: Type -> [(String, [String])] -> Dec
mkToFieldNames typ pairs =
    InstanceD [] (ConT ''ToFieldNames `AppT` typ)
        [FunD (mkName "toFieldNames") $ degen $ map go pairs]
  where
    go (constr, names) =
        Clause [RecP (mkName constr) []]
               (NormalB $ ListE $ map (LitE . StringL) names)
               []

class ToFieldName a where
    toFieldName :: a -> String

mkToFieldName :: Type -> [(String, String)] -> Dec
mkToFieldName typ pairs =
    InstanceD [] (ConT ''ToFieldName `AppT` typ)
        [FunD (mkName "toFieldName") $ degen $ map go pairs]
  where
    go (constr, name) =
        Clause [RecP (mkName constr) []] (NormalB $ LitE $ StringL name) []

data PersistOrder = Asc | Desc
class ToOrder a where
    toOrder :: a -> PersistOrder

mkToOrder :: Type -> [(String, String)] -> Dec
mkToOrder typ pairs =
    InstanceD [] (ConT ''ToOrder `AppT` typ)
        [FunD (mkName "toOrder") $ degen $ map go pairs]
  where
    go (constr, val) =
        Clause [RecP (mkName constr) []] (NormalB $ ConE $ mkName val) []

data PersistFilter = Eq | Ne | Gt | Lt | Ge | Le
class ToFilter a where
    toFilter :: a -> PersistFilter
    isNull :: a -> Bool

mkToFilter :: Type -> [(String, (String, Bool))] -> Dec
mkToFilter typ pairs =
    InstanceD [] (ConT ''ToFilter `AppT` typ)
        [ FunD (mkName "toFilter") $ degen $ map go pairs
        , FunD (mkName "isNull") $ degen $ concatMap go' pairs
        ]
  where
    go (constr, (val, _)) =
        Clause [RecP (mkName constr) []] (NormalB $ ConE $ mkName val) []
    go' (constr, (_, False)) =
        [Clause [RecP (mkName constr) []]
            (NormalB $ ConE $ mkName "False") []]
    go' (constr, (_, True)) =
        [ Clause [ConP (mkName constr) [ConP (mkName "Nothing") []]]
            (NormalB $ ConE $ mkName "True") []
        , Clause [ConP (mkName constr) [WildP]]
            (NormalB $ ConE $ mkName "False") []
        ]

mkPersistable :: Type -> [String] -> Dec
mkPersistable typ constrs =
    InstanceD [] (ConT ''Persistable `AppT` typ)
        $ fpv : map go
            [ "toPersistValue"
            , "sqlType"
            , "isNullable"
            ]
  where
    go func = FunD (mkName func) $ degen $ map (go' func) constrs
    go' func constr =
        let x = mkName "x"
         in Clause [ConP (mkName constr) [VarP x]]
                   (NormalB $ VarE (mkName func) `AppE` VarE x)
                   []
    fpv = FunD (mkName "fromPersistValue")
            [Clause [WildP] (NormalB $ VarE (mkName "error")
                `AppE` LitE (StringL "fromPersistValue")) []]

class HalfDefined a where
    halfDefined :: a

mkHalfDefined :: Type -> String -> Int -> Dec
mkHalfDefined typ constr count =
    InstanceD [] (ConT ''HalfDefined `AppT` typ)
        [FunD (mkName "halfDefined")
            [Clause [] (NormalB
            $ foldl AppE (ConE $ mkName constr)
                    (replicate count $ VarE $ mkName "undefined")) []]]

apE :: Either x (y -> z) -> Either x y -> Either x z
apE (Left x) _ = Left x
apE _ (Left x) = Left x
apE (Right f) (Right y) = Right $ f y

addIsNullable :: [Column] -> (String, (String, String))
              -> (String, (String, Bool))
addIsNullable cols (col, (name, typ)) =
    case lookup col cols of
        Nothing -> error $ "Missing columns: " ++ name
        Just (_, nullable) -> (name, (typ, nullable))
