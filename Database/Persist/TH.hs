{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | This module provides utilities for creating backends. Regular users do not
-- need to use this module.
module Database.Persist.TH
    ( mkPersist
    , share
    , share2
    , mkSave
    , mkDeleteCascade
    , derivePersistField
    , nullable
    ) where

import Database.Persist.Base
import Language.Haskell.TH.Syntax
import Data.Char (toLower, toUpper)
import Data.Maybe (mapMaybe, catMaybes)
import Web.Routes.Quasi (SinglePiece)
import Data.Int (Int64)
import Control.Monad (forM)
import System.IO.Unsafe (unsafePerformIO)

-- | Create data types and appropriate 'PersistEntity' instances for the given
-- 'EntityDef's. Works well with the persist quasi-quoter.
mkPersist :: [EntityDef] -> Q [Dec]
mkPersist = fmap concat . mapM mkEntity

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
        (mkName $ recName x n, NotStrict, pairToType (ty, nullable as))

keyTypeDec :: String -> Name -> EntityDef -> Dec
keyTypeDec constr typ t =
    NewtypeInstD [] ''Key [ConT $ mkName $ entityName t]
                (NormalC (mkName constr) [(NotStrict, ConT typ)])
                [''Show, ''Read, ''Num, ''Integral, ''Enum, ''Eq, ''Ord,
                 ''Real, ''PersistField, ''SinglePiece]

filterTypeDec :: EntityDef -> Dec
filterTypeDec t =
    DataInstD [] ''Filter [ConT $ mkName $ entityName t]
            (NormalC (mkName $ entityName t ++ "IdIn") [(NotStrict, listOfIds)]
            : map (mkFilter $ entityName t) filts)
            (if null filts then [] else [''Show, ''Read, ''Eq])
  where
    listOfIds = ListT `AppT` (ConT ''Key `AppT` ConT (mkName $ entityName t))
    filts = entityFilters t

entityFilters :: EntityDef -> [(String, String, Bool, PersistFilter)]
entityFilters = mapMaybe go' . concatMap go . entityColumns
  where
    go (x, y, as) = map (\a -> (x, y, nullable as, a)) as
    go' (x, y, z, a) =
        case readMay a of
            Nothing -> Nothing
            Just a' -> Just (x, y, z, a')

readMay :: Read a => String -> Maybe a
readMay s =
    case reads s of
        (x, _):_ -> Just x
        [] -> Nothing

isFilterList :: PersistFilter -> Bool
isFilterList In = True
isFilterList NotIn = True
isFilterList _ = False

mkFilter :: String -> (String, String, Bool, PersistFilter) -> Con
mkFilter x (s, ty, isNull', filt) =
    NormalC (mkName $ x ++ upperFirst s ++ show filt) [(NotStrict, ty'')]
  where
    ty' = pairToType (ty, isNull' && isNullableFilter filt)
    ty'' = if isFilterList filt then ListT `AppT` ty' else ty'
    isNullableFilter Eq = True
    isNullableFilter Ne = True
    isNullableFilter In = True
    isNullableFilter NotIn = True
    isNullableFilter Lt = False
    isNullableFilter Le = False
    isNullableFilter Gt = False
    isNullableFilter Ge = False

updateTypeDec :: EntityDef -> Dec
updateTypeDec t =
    DataInstD [] ''Update [ConT $ mkName $ entityName t]
        (map (mkUpdate $ entityName t) tu)
        (if null tu then [] else [''Show, ''Read, ''Eq])
  where
    tu = entityUpdates t

entityUpdates :: EntityDef -> [(String, String, Bool, PersistUpdate)]
entityUpdates = mapMaybe go' . concatMap go . entityColumns
  where
    go (x, y, as) = map (\a -> (x, y, nullable as, a)) as
    go' (x, y, z, "update") =
        deprecate "'update' is deprecated; please use 'Update'"
            $ Just (x, y, z, Update)
    go' (x, y, z, a) =
        case readMay a of
            Nothing -> Nothing
            Just a' -> Just (x, y, z, a')

mkUpdate :: String -> (String, String, Bool, PersistUpdate) -> Con
mkUpdate x (s, ty, isBool, pu) =
    NormalC (mkName $ updateConName x s pu) [(NotStrict, pairToType (ty, isBool))]

orderTypeDec :: EntityDef -> Q Dec
orderTypeDec t = do
    ords <- entityOrders t
    return $ DataInstD [] ''Order [ConT $ mkName $ entityName t]
            (map (mkOrder $ entityName t) ords)
            (if null ords then [] else [''Show, ''Read, ''Eq])

entityOrders :: EntityDef -> Q [(String, String, Exp)]
entityOrders = fmap concat . mapM go . entityColumns
  where
    go (x, _, ys) = fmap catMaybes $ mapM (go' x) ys
    go' x s =
        case reads s of
            (y, _):_ -> do
                z <- lift (y :: PersistOrder)
                return $ Just (x, s, z)
            _ -> return Nothing

mkOrder :: String -> (String, String, Exp) -> Con
mkOrder x (s, ad, _) = NormalC (mkName $ x ++ upperFirst s ++ ad) []

uniqueTypeDec :: EntityDef -> Dec
uniqueTypeDec t =
    DataInstD [] ''Unique [ConT $ mkName $ entityName t]
            (map (mkUnique t) $ entityUniques t)
            (if null (entityUniques t) then [] else [''Show, ''Read, ''Eq])

mkUnique :: EntityDef -> (String, [String]) -> Con
mkUnique t (constr, fields) =
    NormalC (mkName constr) types
  where
    types = map (go . flip lookup3 (entityColumns t)) fields
    go (_, True) = error "Error: cannot have nullables in unique"
    go x = (NotStrict, pairToType x)
    lookup3 s [] =
        error $ "Column not found: " ++ s ++ " in unique " ++ constr
    lookup3 x ((x', y, z):rest)
        | x == x' = (y, nullable z)
        | otherwise = lookup3 x rest

pairToType :: (String, Bool) -> Type
pairToType (s, False) = ConT $ mkName s
pairToType (s, True) = ConT (mkName "Maybe") `AppT` ConT (mkName s)

degen :: [Clause] -> [Clause]
degen [] =
    let err = VarE (mkName "error") `AppE` LitE (StringL
                "Degenerate case, should never happen")
     in [Clause [WildP] (NormalB err) []]
degen x = x

mkToPersistFields :: [(String, Int)] -> Q Dec
mkToPersistFields pairs = do
    clauses <- mapM go pairs
    return $ FunD (mkName "toPersistFields") $ degen clauses
  where
    go :: (String, Int) -> Q Clause
    go (constr, fields) = do
        xs <- sequence $ replicate fields $ newName "x"
        let pat = ConP (mkName constr) $ map VarP xs
        sp <- [|SomePersistField|]
        let bod = ListE $ map (AppE sp . VarE) xs
        return $ Clause [pat] (NormalB bod) []

mkToFieldNames :: [(String, [String])] -> Dec
mkToFieldNames pairs =
        FunD (mkName "persistUniqueToFieldNames") $ degen $ map go pairs
  where
    go (constr, names) =
        Clause [RecP (mkName constr) []]
               (NormalB $ ListE $ map (LitE . StringL) names)
               []

mkToUpdate :: String -> [(String, PersistUpdate)] -> Q Dec
mkToUpdate name pairs = do
    pairs' <- mapM go pairs
    return $ FunD (mkName name) $ degen pairs'
  where
    go (constr, pu) = do
        pu' <- lift pu
        return $ Clause [RecP (mkName constr) []] (NormalB pu') []

mkUniqueToValues :: [(String, [String])] -> Q Dec
mkUniqueToValues pairs = do
    pairs' <- mapM go pairs
    return $ FunD (mkName "persistUniqueToValues") $ degen pairs'
  where
    go :: (String, [String]) -> Q Clause
    go (constr, names) = do
        xs <- mapM (const $ newName "x") names
        let pat = ConP (mkName constr) $ map VarP xs
        tpv <- [|toPersistValue|]
        let bod = ListE $ map (AppE tpv . VarE) xs
        return $ Clause [pat] (NormalB bod) []

mkToFieldName :: String -> [(String, String)] -> Dec
mkToFieldName func pairs =
        FunD (mkName func) $ degen $ map go pairs
  where
    go (constr, name) =
        Clause [RecP (mkName constr) []] (NormalB $ LitE $ StringL name) []

mkToOrder :: [(String, Exp)] -> Dec
mkToOrder pairs =
        FunD (mkName "persistOrderToOrder") $ degen $ map go pairs
  where
    go (constr, val) =
        Clause [RecP (mkName constr) []] (NormalB val) []

mkToFilter :: EntityDef -> [(String, PersistFilter, Bool)] -> Q [Dec]
mkToFilter e pairs = do
    c1 <- mapM go pairs
    idIn' <- idIn
    let _FIXMEc2 = concatMap go' pairs
    return
        [ FunD (mkName "persistFilterToFilter") $ idIn' : c1
        ]
  where
    idIn = do
        in_ <- [|In|]
        return $ Clause
            [RecP (mkName $ entityName e ++ "IdIn") []]
            (NormalB in_)
            []
    go (constr, pf, _) = do
        pf' <- lift pf
        return $ Clause [RecP (mkName constr) []] (NormalB pf') []
    go' (constr, _, False) =
        [Clause [RecP (mkName constr) []]
            (NormalB $ ConE $ mkName "False") []]
    go' (constr, _, True) =
        [ Clause [ConP (mkName constr) [ConP (mkName "Nothing") []]]
            (NormalB $ ConE $ mkName "True") []
        , Clause [ConP (mkName constr) [WildP]]
            (NormalB $ ConE $ mkName "False") []
        ]

mkToValue :: String -> [String] -> Dec
mkToValue func = FunD (mkName func) . degen . map go
  where
    go constr =
        let x = mkName "x"
         in Clause [ConP (mkName constr) [VarP x]]
                   (NormalB $ VarE (mkName "toPersistValue") `AppE` VarE x)
                   []

mkToFiltValue :: EntityDef -> String -> [(String, Bool)] -> Q Dec
mkToFiltValue e func pairs = do
    left <- [|Left . toPersistValue|]
    right <- [|Right . map toPersistValue|]
    clauses <- mapM (go left right) pairs
    inId' <- inId right
    return $ FunD (mkName func) $ (inId' : clauses)
  where
    inId right = do
        x <- newName "x"
        return $ Clause
            [ConP (mkName $ entityName e ++ "IdIn") [VarP x]]
            (NormalB $ right `AppE` VarE x)
            []
    go left right (constr, isList) = do
        x <- newName "x"
        return
            $ Clause [ConP (mkName constr) [VarP x]]
                (NormalB $ (if isList then right else left) `AppE` VarE x)
                []

mkHalfDefined :: String -> Int -> Dec
mkHalfDefined constr count' =
        FunD (mkName "halfDefined")
            [Clause [] (NormalB
            $ foldl AppE (ConE $ mkName constr)
                    (replicate count' $ VarE $ mkName "undefined")) []]

apE :: Either x (y -> z) -> Either x y -> Either x z
apE (Left x) _ = Left x
apE _ (Left x) = Left x
apE (Right f) (Right y) = Right $ f y

mkFromPersistValues :: EntityDef -> Q [Clause]
mkFromPersistValues t = do
    nothing <- [|Left "Invalid fromPersistValues input"|]
    let cons = ConE $ mkName $ entityName t
    xs <- mapM (const $ newName "x") $ entityColumns t
    fs <- [|fromPersistValue|]
    let xs' = map (AppE fs . VarE) xs
    let pat = ListP $ map VarP xs
    ap' <- [|apE|]
    just <- [|Right|]
    let cons' = just `AppE` cons
    return
        [ Clause [pat] (NormalB $ foldl (go ap') cons' xs') []
        , Clause [WildP] (NormalB nothing) []
        ]
  where
    go ap' x y = InfixE (Just x) ap' (Just y)

mkEntity :: EntityDef -> Q [Dec]
mkEntity t = do
    t' <- lift t
    let name = entityName t
    let clazz = ConT ''PersistEntity `AppT` ConT (mkName $ entityName t)
    tpf <- mkToPersistFields [(name, length $ entityColumns t)]
    fpv <- mkFromPersistValues t
    fromIntegral' <- [|fromIntegral|]
    utv <- mkUniqueToValues $ entityUniques t
    show' <- [|show|]
    entityOrders' <- entityOrders t
    otd <- orderTypeDec t
    puk <- mkUniqueKeys t
    tf <- mkToFilter t
                (map (\(x, _, z, y) ->
                    (name ++ upperFirst x ++ show y, y, z))
                $ entityFilters t)
    ftv <- mkToFiltValue t "persistFilterToValue"
                $ map (\(x, _, _, y) ->
                    (name ++ upperFirst x ++ show y, isFilterList y))
                $ entityFilters t
    putu <- mkToUpdate "persistUpdateToUpdate"
                $ map (\(s, _, _, pu) -> (updateConName name s pu, pu))
                $ entityUpdates t
    return
      [ dataTypeDec t
      , TySynD (mkName $ entityName t ++ "Id") [] $
            ConT ''Key `AppT` ConT (mkName $ entityName t)
      , InstanceD [] clazz $
        [ keyTypeDec (entityName t ++ "Id") ''Int64 t
        , filterTypeDec t
        , updateTypeDec t
        , otd
        , uniqueTypeDec t
        , FunD (mkName "entityDef") [Clause [WildP] (NormalB t') []]
        , tpf
        , FunD (mkName "fromPersistValues") fpv
        , mkHalfDefined name $ length $ entityColumns t
        , FunD (mkName "toPersistKey") [Clause [] (NormalB fromIntegral') []]
        , FunD (mkName "fromPersistKey") [Clause [] (NormalB fromIntegral') []]
        , FunD (mkName "showPersistKey") [Clause [] (NormalB show') []]
        , mkToFieldName "persistOrderToFieldName"
                $ map (\(x, y, _) -> (name ++ upperFirst x ++ y, x))
                entityOrders'
        , mkToOrder
                $ map (\(x, y, z) -> (name ++ upperFirst x ++ y, z))
                entityOrders'
        , mkToFieldName "persistUpdateToFieldName"
                $ map (\(s, _, _, pu) -> (updateConName name s pu, s))
                $ entityUpdates t
        , mkToValue "persistUpdateToValue"
                $ map (\(s, _, _, pu) -> updateConName name s pu)
                $ entityUpdates t
        , putu
        , mkToFieldName "persistFilterToFieldName"
                $ (:) (entityName t ++ "IdIn", "id")
                $ map (\(x, _, _, y) -> (name ++ upperFirst x ++ show y, x))
                $ entityFilters t
        , ftv
        , mkToFieldNames $ entityUniques t
        , utv
        , puk
        ] ++ tf
        ]

updateConName :: String -> String -> PersistUpdate -> String
updateConName name s pu = concat
    [ name
    , upperFirst s
    , case pu of
        Update -> ""
        _ -> show pu
    ]

share :: [[EntityDef] -> Q [Dec]] -> [EntityDef] -> Q [Dec]
share fs x = fmap concat $ mapM ($ x) fs

share2 :: ([EntityDef] -> Q [Dec])
       -> ([EntityDef] -> Q [Dec])
       -> [EntityDef]
       -> Q [Dec]
share2 f g x = do
    y <- f x
    z <- g x
    return $ y ++ z

mkSave :: String -> [EntityDef] -> Q [Dec]
mkSave name' defs' = do
    let name = mkName name'
    defs <- lift defs'
    return [ SigD name $ ListT `AppT` ConT ''EntityDef
           , FunD name [Clause [] (NormalB defs) []]
           ]

data Dep = Dep
    { depTarget :: String
    , depSourceTable :: String
    , depSourceField :: String
    , depSourceNull :: Bool
    }

mkDeleteCascade :: [EntityDef] -> Q [Dec]
mkDeleteCascade defs = do
    let deps = concatMap getDeps defs
    mapM (go deps) defs
  where
    getDeps :: EntityDef -> [Dep]
    getDeps def =
        concatMap getDeps' $ entityColumns def
      where
        getDeps' (name, typ, attribs) =
            let isNull = nullable attribs
                l = length typ
                (f, b) = splitAt (l - 2) typ
             in if b == "Id"
                    then return Dep
                            { depTarget = f
                            , depSourceTable = entityName def
                            , depSourceField = name
                            , depSourceNull = isNull
                            }
                    else []
    go :: [Dep] -> EntityDef -> Q Dec
    go allDeps EntityDef{entityName = name} = do
        let deps = filter (\x -> depTarget x == name) allDeps
        key <- newName "key"
        del <- [|delete|]
        dcw <- [|deleteCascadeWhere|]
        just <- [|Just|]
        let mkStmt dep = NoBindS
                $ dcw `AppE`
                  ListE
                    [ ConE (mkName filtName) `AppE` val (depSourceNull dep)
                    ]
              where
                filtName =
                    depSourceTable dep ++ upperFirst (depSourceField dep)
                      ++ "Eq"
                val False = VarE key
                val True = just `AppE` VarE key



        let stmts = map mkStmt deps ++ [NoBindS $ del `AppE` VarE key]
        return $
            InstanceD
            []
            (ConT ''DeleteCascade `AppT` ConT (mkName name))
            [ FunD (mkName "deleteCascade")
                [Clause [VarP key] (NormalB $ DoE stmts) []]
            ]

mkUniqueKeys :: EntityDef -> Q Dec
mkUniqueKeys def = do
    c <- clause
    return $ FunD (mkName "persistUniqueKeys") [c]
  where
    clause = do
        xs <- forM (entityColumns def) $ \(x, _, _) -> do
            x' <- newName $ '_' : x
            return (x, x')
        let pcs = map (go xs) $ entityUniques def
        let pat = ConP (mkName $ entityName def) $ map (VarP . snd) xs
        return $ Clause [pat] (NormalB $ ListE pcs) []
    go xs (name, cols) =
        foldl (go' xs) (ConE (mkName name)) cols
    go' xs front col =
        let Just col' = lookup col xs
         in front `AppE` VarE col'

-- | Automatically creates a valid 'PersistField' instance for any datatype
-- that has valid 'Show' and 'Read' instances. Can be very convenient for
-- 'Enum' types.
derivePersistField :: String -> Q [Dec]
derivePersistField s = do
    ss <- [|SqlString|]
    tpv <- [|PersistString . show|]
    fpv <- [|\dt v ->
                case fromPersistValue v of
                    Left e -> Left e
                    Right s' ->
                        case reads s' of
                            (x, _):_ -> Right x
                            [] -> Left $ "Invalid " ++ dt ++ ": " ++ s'|]
    return
        [ InstanceD [] (ConT ''PersistField `AppT` ConT (mkName s))
            [ FunD (mkName "sqlType")
                [ Clause [WildP] (NormalB ss) []
                ]
            , FunD (mkName "toPersistValue")
                [ Clause [] (NormalB tpv) []
                ]
            , FunD (mkName "fromPersistValue")
                [ Clause [] (NormalB $ fpv `AppE` LitE (StringL s)) []
                ]
            ]
        ]

nullable :: [String] -> Bool
nullable s
    | "Maybe" `elem` s = True
    | "null" `elem` s = deprecate "Please replace null with Maybe" True
    | otherwise = False

deprecate :: String -> a -> a
deprecate s x = unsafePerformIO $ do
    putStrLn $ "DEPRECATED: " ++ s
    return x
