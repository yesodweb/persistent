{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | This module provides utilities for creating backends. Regular users do not
-- need to use this module.
module Database.Persist.Helper
    ( recName
    , upperFirst
      -- * High level design
    , EntityDef (..)
    , entityOrders
    , entityFilters
    , entityUpdates
      -- * TH datatype helpers
    , dataTypeDec
    , persistMonadTypeDec
    , keyTypeDec
    , filterTypeDec
    , updateTypeDec
    , orderTypeDec
    , uniqueTypeDec
      -- * TH typeclass helpers
    , mkToPersistFields
    , mkToFieldNames
    , mkToFieldName
    , mkPersistField
    , mkToFilter
    , mkToOrder
    , mkHalfDefined
      -- * Type classes
    , SomePersistField (..)
    , ToPersistFields (..)
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
import Data.Maybe (fromJust, mapMaybe)
import Web.Routes.Quasi (SinglePiece)

data EntityDef = EntityDef
    { entityName    :: String
    , entityColumns :: [(String, String, [String])] -- ^ name, type, attribs
    , entityUniques :: [(String, [String])] -- ^ name, columns
    , entityDerives :: [String]
    }
    deriving Show

instance Lift EntityDef where
    lift (EntityDef a b c d) = do
        e <- [|EntityDef|]
        a' <- lift a
        b' <- lift b
        c' <- lift c
        d' <- lift d
        return $ e `AppE` a' `AppE` b' `AppE` c' `AppE` d'

recName :: String -> String -> String
recName dt f = lowerFirst dt ++ upperFirst f

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst [] = []

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs
upperFirst [] = []

dataTypeDec :: EntityDef -> Dec
dataTypeDec t =
    let name = mkName $ entityName t
        cols = map (mkCol $ entityName t) $ entityColumns t
     in DataD [] name [] [RecC name cols] $ map mkName $ entityDerives t
  where
    mkCol x (n, ty, as) =
        (mkName $ recName x n, NotStrict, pairToType (ty, "null" `elem` as))

persistMonadTypeDec :: Type -> EntityDef -> Dec
persistMonadTypeDec monad t =
    TySynInstD ''PersistMonad [ConT $ mkName $ entityName t] monad

keyTypeDec :: String -> String -> EntityDef -> Dec
keyTypeDec constr typ t =
    NewtypeInstD [] ''Key [ConT $ mkName $ entityName t]
                (NormalC (mkName constr) [(NotStrict, ConT $ mkName typ)])
                [''Show, ''Read, ''Num, ''Integral, ''Enum, ''Eq, ''Ord,
                 ''Real, ''PersistField, ''SinglePiece]

filterTypeDec :: EntityDef -> Dec
filterTypeDec t =
    DataInstD [] ''Filter [ConT $ mkName $ entityName t]
            (map (mkFilter $ entityName t) filts)
            (if null filts then [] else [''Show, ''Read, ''Eq])
  where
    filts = entityFilters t

entityFilters :: EntityDef -> [(String, String, Bool, PersistFilter)]
entityFilters = mapMaybe go' . concatMap go . entityColumns
  where
    go (x, y, as) = map (\a -> (x, y, "null" `elem` as, a)) as
    go' (x, y, z, a) =
        case readMay a of
            Nothing -> Nothing
            Just a' -> Just (x, y, z, a')
    readMay s =
        case reads s of
            (x, _):_ -> Just x
            [] -> Nothing

mkFilter :: String -> (String, String, Bool, PersistFilter) -> Con
mkFilter x (s, ty, isNull', filt) =
    NormalC (mkName $ x ++ upperFirst s ++ show filt)
                       [(NotStrict, pairToType (ty, isNull'))]

updateTypeDec :: EntityDef -> Dec
updateTypeDec t =
    DataInstD [] ''Update [ConT $ mkName $ entityName t]
        (map (mkUpdate $ entityName t) tu)
        (if null tu then [] else [''Show, ''Read, ''Eq])
  where
    tu = entityUpdates t

entityUpdates :: EntityDef -> [(String, String, Bool)]
entityUpdates = mapMaybe go . entityColumns
  where
    go (name, typ, attribs)
        | "update" `elem` attribs =
            Just (name, typ, "null" `elem` attribs)
        | otherwise = Nothing

mkUpdate :: String -> (String, String, Bool) -> Con
mkUpdate x (s, ty, isBool) =
    NormalC (mkName $ x ++ upperFirst s)
                [(NotStrict, pairToType (ty, isBool))]

orderTypeDec :: EntityDef -> Dec
orderTypeDec t =
    DataInstD [] ''Order [ConT $ mkName $ entityName t]
            (map (mkOrder $ entityName t) ords)
            (if null ords then [] else [''Show, ''Read, ''Eq])
  where
    ords = entityOrders t

entityOrders :: EntityDef -> [(String, String)]
entityOrders = concatMap go . entityColumns
  where
    go (x, _, ys) = mapMaybe (go' x) ys
    go' x "Asc" = Just (x, "Asc")
    go' x "Desc" = Just (x, "Desc")
    go' _ _ = Nothing

mkOrder :: String -> (String, String) -> Con
mkOrder x (s, ad) = NormalC (mkName $ x ++ upperFirst s ++ ad) []

uniqueTypeDec :: EntityDef -> Dec
uniqueTypeDec t =
    DataInstD [] ''Unique [ConT $ mkName $ entityName t]
            (map (mkUnique t) $ entityUniques t)
            (if null (entityUniques t) then [] else [''Show, ''Read, ''Eq])

mkUnique :: EntityDef -> (String, [String]) -> Con
mkUnique t (constr, fields) =
    NormalC (mkName constr) types
  where
    types = map (go . fromJust . flip lookup3 (entityColumns t)) fields
    go (_, True) = error "Error: cannot have nullables in unique"
    go x = (NotStrict, pairToType x)
    lookup3 _ [] = Nothing
    lookup3 x ((x', y, z):rest)
        | x == x' = Just (y, "null" `elem` z)
        | otherwise = lookup3 x rest

pairToType :: (String, Bool) -> Type
pairToType (s, False) = ConT $ mkName s
pairToType (s, True) = ConT (mkName "Maybe") `AppT` ConT (mkName s)

data SomePersistField = forall a. PersistField a => SomePersistField a
instance PersistField SomePersistField where
    toPersistValue (SomePersistField a) = toPersistValue a
    fromPersistValue x = fmap SomePersistField (fromPersistValue x :: Either String String)
    sqlType (SomePersistField a) = sqlType a

class ToPersistFields a where
    toPersistFields :: a -> [SomePersistField]

degen :: [Clause] -> [Clause]
degen [] =
    let err = VarE (mkName "error") `AppE` LitE (StringL
                "Degenerate case, should never happen")
     in [Clause [WildP] (NormalB err) []]
degen x = x

mkToPersistFields :: Type
                 -> [(String, Int)]
                 -> Q Dec
mkToPersistFields typ pairs = do
    clauses <- mapM go pairs
    return $ InstanceD [] (ConT ''ToPersistFields `AppT` typ)
                [FunD (mkName "toPersistFields") $ degen clauses]
  where
    go (constr, fields) = do
        xs <- sequence $ replicate fields $ newName "x"
        let pat = ConP (mkName constr) $ map VarP xs
        sp <- [|SomePersistField|]
        let bod = ListE $ map (AppE sp . VarE) xs
        return $ Clause [pat] (NormalB bod) []

class FromPersistValues a where
    fromPersistValues :: [PersistValue] -> Either String a

toPersistValues :: ToPersistFields a => a -> [PersistValue]
toPersistValues = map toPersistValue . toPersistFields

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
    deriving (Read, Show)
class ToFilter a where
    toFilter :: a -> PersistFilter
    isNull :: a -> Bool

mkToFilter :: Type -> [(String, PersistFilter, Bool)] -> Dec
mkToFilter typ pairs =
    InstanceD [] (ConT ''ToFilter `AppT` typ)
        [ FunD (mkName "toFilter") $ degen $ map go pairs
        , FunD (mkName "isNull") $ degen $ concatMap go' pairs
        ]
  where
    go (constr, pf, _) =
        Clause [RecP (mkName constr) []] (NormalB $ ConE $ mkName $ show pf) []
    go' (constr, _, False) =
        [Clause [RecP (mkName constr) []]
            (NormalB $ ConE $ mkName "False") []]
    go' (constr, _, True) =
        [ Clause [ConP (mkName constr) [ConP (mkName "Nothing") []]]
            (NormalB $ ConE $ mkName "True") []
        , Clause [ConP (mkName constr) [WildP]]
            (NormalB $ ConE $ mkName "False") []
        ]

mkPersistField :: Type -> [String] -> Dec
mkPersistField typ constrs =
    InstanceD [] (ConT ''PersistField `AppT` typ)
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

addIsNullable :: EntityDef -> (String, (String, String))
              -> (String, (String, Bool))
addIsNullable ed (col, (name, typ)) =
    case filter (\(x, _, _) -> x == col) $ entityColumns ed of
        [] -> error $ "Missing columns: " ++ col ++ ", " ++ show ed
        (_, _, attribs):_ -> (name, (typ, "null" `elem` attribs))
