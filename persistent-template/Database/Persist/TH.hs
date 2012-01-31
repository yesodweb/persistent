{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-fields #-}
-- | This module provides utilities for creating backends. Regular users do not
-- need to use this module.
module Database.Persist.TH
    ( -- * Parse entity defs
      persistWith
    , persistUpperCase
    , persistLowerCase
    , persistFileWith
      -- ** Deprecated synonyms
    , persist
    , persistFile
      -- * Turn @EntityDef@s into types
    , mkPersist
    , MkPersistSettings (..)
    , sqlSettings
      -- * Various other TH functions
    , mkMigrate
    , mkSave
    , mkDeleteCascade
    , share
    , derivePersistField
    , persistFieldFromEntity
      -- ** Deprecated
    , share2
    ) where

import Prelude hiding ((++), take, concat, splitAt)
import Database.Persist.EntityDef
import Database.Persist.Quasi
import Database.Persist.Store
import Database.Persist.Query.Internal
import Database.Persist.GenericSql (Migration, SqlPersist, migrate)
import Database.Persist.Util (nullable)
import Database.Persist.TH.Library (apE)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Char (toLower, toUpper)
import Control.Monad (forM)
#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
#else
import Control.Monad.IO.Control (MonadControlIO)
#endif
import qualified System.IO as SIO
import Data.Text (pack, Text, append, isSuffixOf, unpack, take, concat, uncons, cons, splitAt)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (foldl')
import Data.Monoid (mappend, mconcat)
import qualified Data.Map as M

-- | Converts a quasi-quoted syntax into a list of entity definitions, to be
-- used as input to the template haskell generation code (mkPersist).
persistWith :: PersistSettings -> QuasiQuoter
persistWith ps = QuasiQuoter
    { quoteExp = lift . parse ps . pack
    }

-- | Deprecate synonym for 'persistUpperCase'.
persist :: QuasiQuoter
persist = persistUpperCase
{-# DEPRECATED persist "Please use persistUpperCase instead." #-}

-- | Apply 'persistWith' to 'upperCaseSettings'.
persistUpperCase :: QuasiQuoter
persistUpperCase = persistWith upperCaseSettings

-- | Apply 'persistWith' to 'lowerCaseSettings'.
persistLowerCase :: QuasiQuoter
persistLowerCase = persistWith lowerCaseSettings

-- | Same as 'persistWith', but uses an external file instead of a
-- quasiquotation.
persistFileWith :: PersistSettings -> FilePath -> Q Exp
persistFileWith ps fp = do
    h <- qRunIO $ SIO.openFile fp SIO.ReadMode
    qRunIO $ SIO.hSetEncoding h SIO.utf8_bom
    s <- qRunIO $ TIO.hGetContents h
    lift $ parse ps s

-- | Deprecated function. Equivalent to @persistFileWith upperCaseSettings@.
persistFile :: FilePath -> Q Exp
persistFile = persistFileWith upperCaseSettings

-- | Create data types and appropriate 'PersistEntity' instances for the given
-- 'EntityDef's. Works well with the persist quasi-quoter.
mkPersist :: MkPersistSettings -> [EntityDef] -> Q [Dec]
mkPersist mps ents = fmap mconcat $ mapM (mkEntity mps entLookup) ents
  where
    entMap = M.fromList $ zip (map (unHaskellName . entityHaskell) ents) ents
    entLookup fd = 
        let typName = unFieldType $ fieldType fd
        in  case M.lookup typName entMap of
              Nothing -> lookupError typName
              Just ent -> ent
    lookupError field = error $ unpack $
        "expected the schema to define " `mappend` field

-- | Settings to be passed to the 'mkPersist' function.
data MkPersistSettings = MkPersistSettings
    { mpsBackend :: Type
    -- ^ Which database backend we\'re using.
    --
    -- When generating data types, each type is given a generic version- which
    -- works with any backend- and a type synonym for the commonly used
    -- backend. This is where you specify that commonly used backend.
    }

-- | Use the 'SqlPersist' backend.
sqlSettings :: MkPersistSettings
sqlSettings = MkPersistSettings
    { mpsBackend = ConT ''SqlPersist
    }

recName :: Text -> Text -> Text
recName dt f = lowerFirst dt ++ upperFirst f

lowerFirst :: Text -> Text
lowerFirst t =
    case uncons t of
        Just (a, b) -> cons (toLower a) b
        Nothing -> t

upperFirst :: Text -> Text
upperFirst t =
    case uncons t of
        Just (a, b) -> cons (toUpper a) b
        Nothing -> t

dataTypeDec :: EntityDef -> Dec
dataTypeDec t =
    DataD [] nameG [KindedTV backend monadTransKind] [RecC name cols]
    $ map (mkName . unpack) $ entityDerives t
  where
    monadKind = StarK `ArrowK` StarK
    monadTransKind = monadKind `ArrowK` monadKind
    mkCol x (FieldDef n _ ty as _) =
        (mkName $ unpack $ recName x $ unHaskellName n,
         NotStrict,
         pairToType backend (unFieldType ty, nullable as)
        )
    nameG = mkName $ unpack $ unHaskellName (entityHaskell t) ++ suffix
    name = mkName $ unpack $ unHaskellName $ entityHaskell t
    cols = map (mkCol $ unHaskellName $ entityHaskell t) $ entityFields t
    backend = mkName "backend"

readMay :: Read a => String -> Maybe a
readMay s =
    case reads s of
        (x, _):_ -> Just x
        [] -> Nothing

entityUpdates :: EntityDef -> [(HaskellName, FieldType, Bool, PersistUpdate)]
entityUpdates =
    concatMap go . entityFields
  where
    go (FieldDef x _ y as _) = map (\a -> (x, y, nullable as, a)) [minBound..maxBound]

uniqueTypeDec :: EntityDef -> Dec
uniqueTypeDec t =
    DataInstD [] ''Unique
        [ ConT (mkName $ unpack (unHaskellName (entityHaskell t) ++ suffix))
          `AppT` VarT backend, VarT backend2
        ]
            (map (mkUnique backend t) $ entityUniques t)
            (if null (entityUniques t) then [] else [''Show, ''Read, ''Eq])
  where
    backend = mkName "backend"
    backend2 = mkName "backend2"

mkUnique :: Name -> EntityDef -> UniqueDef -> Con
mkUnique backend t (UniqueDef (HaskellName constr) _ fields) =
    NormalC (mkName $ unpack constr) types
  where
    types = map (go . flip lookup3 (entityFields t))
          $ map (unHaskellName . fst) fields

    go :: (FieldType, Bool) -> (Strict, Type)
    go (_, True) = error "Error: cannot have nullables in unique"
    go (FieldType x, y) = (NotStrict, pairToType backend (x, y))

    lookup3 :: Text -> [FieldDef] -> (FieldType, Bool)
    lookup3 s [] =
        error $ unpack $ "Column not found: " ++ s ++ " in unique " ++ constr
    lookup3 x ((FieldDef (HaskellName x') _ y z _):rest)
        | x == x' = (y, nullable z)
        | otherwise = lookup3 x rest

pairToType :: Name -- ^ backend
           -> (Text, Bool) -> Type
pairToType backend (s, False) = idType backend s
pairToType backend (s, True) = ConT (mkName "Maybe") `AppT` idType backend s

idType :: Name -> Text -> Type
idType backend typ
    | "Id" `isSuffixOf` typ =
        ConT ''Key
        `AppT` VarT backend
        `AppT` (ConT (mkName $ unpack $ take (T.length typ - 2) typ ++ "Generic")
                `AppT` VarT backend)
    | otherwise = ConT $ mkName $ unpack typ

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

mkToFieldNames :: [UniqueDef] -> Q Dec
mkToFieldNames pairs = do
    pairs' <- mapM go pairs
    return $ FunD (mkName "persistUniqueToFieldNames") $ degen pairs'
  where
    go (UniqueDef constr _ names) = do
        names' <- lift names
        return $
            Clause
                [RecP (mkName $ unpack $ unHaskellName constr) []]
                (NormalB names')
                []

mkToUpdate :: String -> [(String, PersistUpdate)] -> Q Dec
mkToUpdate name pairs = do
    pairs' <- mapM go pairs
    return $ FunD (mkName name) $ degen pairs'
  where
    go (constr, pu) = do
        pu' <- lift pu
        return $ Clause [RecP (mkName constr) []] (NormalB pu') []

mkUniqueToValues :: [UniqueDef] -> Q Dec
mkUniqueToValues pairs = do
    pairs' <- mapM go pairs
    return $ FunD (mkName "persistUniqueToValues") $ degen pairs'
  where
    go :: UniqueDef -> Q Clause
    go (UniqueDef constr _ names) = do
        xs <- mapM (const $ newName "x") names
        let pat = ConP (mkName $ unpack $ unHaskellName constr) $ map VarP xs
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

mkToValue :: String -> [String] -> Dec
mkToValue func = FunD (mkName func) . degen . map go
  where
    go constr =
        let x = mkName "x"
         in Clause [ConP (mkName constr) [VarP x]]
                   (NormalB $ VarE (mkName "toPersistValue") `AppE` VarE x)
                   []

mkHalfDefined :: String -> Int -> Dec
mkHalfDefined constr count' =
        FunD (mkName "halfDefined")
            [Clause [] (NormalB
            $ foldl AppE (ConE $ mkName constr)
                    (replicate count' $ VarE $ mkName "undefined")) []]

mkFromPersistValues :: EntityDef -> Q [Clause]
mkFromPersistValues t = do
    nothing <- [|Left $(liftT "Invalid fromPersistValues input")|]
    let cons' = ConE $ mkName $ unpack $ unHaskellName $ entityHaskell t
    xs <- mapM (const $ newName "x") $ entityFields t
    fs <- [|fromPersistValue|]
    let xs' = map (AppE fs . VarE) xs
    let pat = ListP $ map VarP xs
    ap' <- [|apE|]
    just <- [|Right|]
    let cons'' = just `AppE` cons'
    return
        [ Clause [pat] (NormalB $ foldl (go ap') cons'' xs') []
        , Clause [WildP] (NormalB nothing) []
        ]
  where
    go ap' x y = InfixE (Just x) ap' (Just y)


mkEntity :: MkPersistSettings -> (FieldDef -> EntityDef) -> EntityDef -> Q [Dec]
mkEntity mps entLookup t = do
    t' <- lift t
    let name = unpack $ unHaskellName $ entityHaskell t
    let clazz = ConT ''PersistEntity `AppT` (ConT (mkName $ unpack $ unHaskellName (entityHaskell t) ++ suffix) `AppT` VarT (mkName "backend"))
    tpf <- mkToPersistFields [(name, length $ entityFields t)]
    fpv <- mkFromPersistValues t
    utv <- mkUniqueToValues $ entityUniques t
    puk <- mkUniqueKeys t

    -- TODO: enforce ordering issue: embedded entities must be defined before they are later embedded
    let embeddedFields = filter fieldEmbedded (entityFields t)
    embeds <- fmap mconcat $ mapM persistFieldFromEntity $ map entLookup embeddedFields

    fields <- mapM (mkField t) $ FieldDef
        (HaskellName "Id")
        (entityID t)
        (FieldType $ unHaskellName (entityHaskell t) ++ "Id")
        []
        False
        : entityFields t
    toFieldNames <- mkToFieldNames $ entityUniques t
    return $ embeds `mappend`
      [ dataTypeDec t
      , TySynD (mkName $ unpack $ unHaskellName $ entityHaskell t) [] $
            ConT (mkName $ unpack $ unHaskellName (entityHaskell t) ++ suffix)
                `AppT` mpsBackend mps
      , TySynD (mkName $ unpack $ unHaskellName (entityHaskell t) ++ "Id") [] $
            ConT ''Key `AppT` mpsBackend mps `AppT` ConT (mkName $ unpack $ unHaskellName $ entityHaskell t)
      , InstanceD [] clazz $
        [ uniqueTypeDec t
        , FunD (mkName "entityDef") [Clause [WildP] (NormalB t') []]
        , tpf
        , FunD (mkName "fromPersistValues") fpv
        , mkHalfDefined name $ length $ entityFields t
        , toFieldNames
        , utv
        , puk
        , DataInstD
            []
            ''EntityField
            [ ConT (mkName $ unpack $ unHaskellName (entityHaskell t) ++ suffix) `AppT` VarT (mkName "backend")
            , VarT $ mkName "typ"
            ]
            (map fst fields)
            []
        , FunD (mkName "persistFieldDef") (map snd fields)
        , TySynInstD
            (mkName "PersistEntityBackend")
            [ConT (mkName $ unpack $ unHaskellName (entityHaskell t) ++ suffix) `AppT` VarT (mkName "backend")]
            (VarT (mkName "backend"))
        , FunD (mkName "persistIdField") [Clause [] (NormalB $ ConE $ mkName $ unpack $ unHaskellName (entityHaskell t) ++ "Id") []]
        ]
      ]

-- | produce code similar to the following:
--
-- instance PersistEntity e => PersistField e where
--    toPersistValue = PersistMap $ zip columNames (map toPersistValue . toPersistFields)
--    fromPersistValue (PersistMap o) = fromPersistValues $ map (\(_,v) ->
--        casefromPersistValue v of
--            Left e -> error e
--            Right r -> r) o
--    fromPersistValue x = Left $ "Expected PersistMap, received: " ++ show x 
--    sqlType _ = SqlString
persistFieldFromEntity :: EntityDef -> Q [Dec]
persistFieldFromEntity e = do
    ss <- [|SqlString|]
    unexpected <- [|\x -> Left $ "Expected PersistMap, received: " ++ T.pack (show x)|]
    let columnNames = map (unpack . unHaskellName . fieldHaskell) (entityFields e)
    obj <- [|PersistMap $ zip (map pack columnNames) (map toPersistValue $ toPersistFields e)|]
    pmName <- newName "pm"
    fpv <- [|\x -> fromPersistValues $ map (\(_,v) -> case fromPersistValue v of
                                                      Left e' -> error $ unpack e'
                                                      Right r -> r) x|]

    return
        [ InstanceD [] (ConT ''PersistField `AppT` ConT (mkName $ unpack $ unHaskellName $ entityHaskell e))
            [ sqlTypeFunD ss
            , FunD (mkName "toPersistValue") [ Clause [] (NormalB obj) [] ]
            , FunD (mkName "fromPersistValue")
                [ Clause [ConP (mkName "PersistMap") [VarP pmName]]
                    (NormalB $ fpv `AppE` VarE pmName) []
                , Clause [WildP] (NormalB unexpected) []
                ]
            ]
        ]

updateConName :: Text -> Text -> PersistUpdate -> Text
updateConName name s pu = concat
    [ name
    , upperFirst s
    , case pu of
        Assign -> ""
        _ -> pack $ show pu
    ]

-- | Apply the given list of functions to the same @EntityDef@s.
--
-- This function is useful for cases such as:
--
-- >>> share [mkSave "myDefs", mkPersist sqlSettings] [persistLowerCase|...|]
share :: [[EntityDef] -> Q [Dec]] -> [EntityDef] -> Q [Dec]
share fs x = fmap mconcat $ mapM ($ x) fs

-- | Deprecated, restricted version of 'share'.
share2 :: ([EntityDef] -> Q [Dec])
       -> ([EntityDef] -> Q [Dec])
       -> [EntityDef]
       -> Q [Dec]
share2 f g x = do
    y <- f x
    z <- g x
    return $ y `mappend` z
{-# DEPRECATED share2 "Use share instead" #-}

-- | Save the @EntityDef@s passed in under the given name.
mkSave :: String -> [EntityDef] -> Q [Dec]
mkSave name' defs' = do
    let name = mkName name'
    defs <- lift defs'
    return [ SigD name $ ListT `AppT` ConT ''EntityDef
           , FunD name [Clause [] (NormalB defs) []]
           ]

data Dep = Dep
    { depTarget :: Text
    , depSourceTable :: HaskellName
    , depSourceField :: HaskellName
    , depSourceNull :: Bool
    }

-- | Generate a 'DeleteCascade' instance for the given @EntityDef@s.
mkDeleteCascade :: [EntityDef] -> Q [Dec]
mkDeleteCascade defs = do
    let deps = concatMap getDeps defs
    mapM (go deps) defs
  where
    getDeps :: EntityDef -> [Dep]
    getDeps def =
        concatMap getDeps' $ entityFields def
      where
        getDeps' :: FieldDef -> [Dep]
        getDeps' (FieldDef name _ ftyp attribs _) =
            let isNull = nullable attribs
                typ = unFieldType ftyp
                l = T.length typ
                (f, b) = splitAt (l - 2) typ
             in if b == "Id"
                    then return Dep
                            { depTarget = f
                            , depSourceTable = entityHaskell def
                            , depSourceField = name
                            , depSourceNull = isNull
                            }
                    else []
    go :: [Dep] -> EntityDef -> Q Dec
    go allDeps EntityDef{entityHaskell = name} = do
        let deps = filter (\x -> depTarget x == unHaskellName name) allDeps
        key <- newName "key"
        del <- [|delete|]
        dcw <- [|deleteCascadeWhere|]
        just <- [|Just|]
        filt <- [|Filter|]
        eq <- [|Eq|]
        left <- [|Left|]
        let mkStmt :: Dep -> Stmt
            mkStmt dep = NoBindS
                $ dcw `AppE`
                  ListE
                    [ filt `AppE` ConE (mkName $ unpack filtName)
                           `AppE` (left `AppE` val (depSourceNull dep))
                           `AppE` eq
                    ]
              where
                filtName = unHaskellName (depSourceTable dep) ++
                           upperFirst (unHaskellName $ depSourceField dep)
                val False = VarE key
                val True = just `AppE` VarE key



        let stmts :: [Stmt]
            stmts = map mkStmt deps `mappend`
                    [NoBindS $ del `AppE` VarE key]
        return $
            InstanceD
            [ ClassP ''PersistQuery [VarT $ mkName "backend", VarT $ mkName "m"]
            , ClassP ''Monad [VarT $ mkName "m"]
            ]
            (ConT ''DeleteCascade `AppT`
                (ConT (mkName $ unpack $ unHaskellName name ++ suffix) `AppT` VarT (mkName "backend"))
                `AppT` VarT (mkName "backend")
                `AppT` VarT (mkName "m")
                )
            [ FunD (mkName "deleteCascade")
                [Clause [VarP key] (NormalB $ DoE stmts) []]
            ]

mkUniqueKeys :: EntityDef -> Q Dec
mkUniqueKeys def = do
    c <- clause
    return $ FunD (mkName "persistUniqueKeys") [c]
  where
    clause = do
        xs <- forM (entityFields def) $ \(FieldDef x _ _ _ _) -> do
            x' <- newName $ '_' : unpack (unHaskellName x)
            return (x, x')
        let pcs = map (go xs) $ entityUniques def
        let pat = ConP
                (mkName $ unpack $ unHaskellName $ entityHaskell def)
                (map (VarP . snd) xs)
        return $ Clause [pat] (NormalB $ ListE pcs) []

    go :: [(HaskellName, Name)] -> UniqueDef -> Exp
    go xs (UniqueDef name _ cols) =
        foldl' (go' xs) (ConE (mkName $ unpack $ unHaskellName name)) (map fst cols)

    go' :: [(HaskellName, Name)] -> Exp -> HaskellName -> Exp
    go' xs front col =
        let Just col' = lookup col xs
         in front `AppE` VarE col'

sqlTypeFunD :: Exp -> Dec
sqlTypeFunD st = FunD (mkName "sqlType")
                [ Clause [WildP] (NormalB st) []
                ]

-- | Automatically creates a valid 'PersistField' instance for any datatype
-- that has valid 'Show' and 'Read' instances. Can be very convenient for
-- 'Enum' types.
derivePersistField :: String -> Q [Dec]
derivePersistField s = do
    ss <- [|SqlString|]
    tpv <- [|PersistText . pack . show|]
    fpv <- [|\dt v ->
                case fromPersistValue v of
                    Left e -> Left e
                    Right s' ->
                        case reads $ unpack s' of
                            (x, _):_ -> Right x
                            [] -> Left $ "Invalid " ++ dt ++ ": " ++ s'|]
    return
        [ InstanceD [] (ConT ''PersistField `AppT` ConT (mkName s))
            [ sqlTypeFunD ss 
            , FunD (mkName "toPersistValue")
                [ Clause [] (NormalB tpv) []
                ]
            , FunD (mkName "fromPersistValue")
                [ Clause [] (NormalB $ fpv `AppE` LitE (StringL s)) []
                ]
            ]
        ]

-- | Creates a single function to perform all migrations for the entities
-- defined here. One thing to be aware of is dependencies: if you have entities
-- with foreign references, make sure to place those definitions after the
-- entities they reference.
mkMigrate :: String -> [EntityDef] -> Q [Dec]
mkMigrate fun allDefs = do
    body' <- body
    return
        [ SigD (mkName fun) typ
        , FunD (mkName fun) [Clause [] (NormalB body') []]
        ]
  where
    defs = filter isMigrated allDefs
    isMigrated def = not $ "no-migrate" `elem` entityAttrs def
    typ = ForallT [PlainTV $ mkName "m"]
            [ ClassP ''MonadBaseControl [ConT ''IO, VarT $ mkName "m"]
            , ClassP ''MonadIO [VarT $ mkName "m"]
            ]
            $ ConT ''Migration `AppT` (ConT ''SqlPersist `AppT` VarT (mkName "m"))
    body :: Q Exp
    body =
        case defs of
            [] -> [|return ()|]
            _ -> DoE `fmap` mapM toStmt defs
    toStmt :: EntityDef -> Q Stmt
    toStmt ed = do
        let n = entityHaskell ed
        u <- [|undefined|]
        m <- [|migrate|]
        defs' <- lift defs
        let u' = SigE u $ ConT $ mkName $ unpack $ unHaskellName n
        return $ NoBindS $ m `AppE` defs' `AppE` u'

instance Lift EntityDef where
    lift (EntityDef a b c d e f g h) =
        [|EntityDef
            $(lift a)
            $(lift b)
            $(lift c)
            $(liftTs d)
            $(lift e)
            $(lift f)
            $(liftTs g)
            $(liftMap h)
            |]
instance Lift FieldDef where
    lift (FieldDef a b c d e) = [|FieldDef $(lift a) $(lift b) $(lift c) $(liftTs d) $(lift e)|]
instance Lift UniqueDef where
    lift (UniqueDef a b c) = [|UniqueDef $(lift a) $(lift b) $(lift c)|]

liftT :: Text -> Q Exp
liftT t = [|pack $(lift (unpack t))|]

liftTs :: [Text] -> Q Exp
liftTs = fmap ListE . mapM liftT

liftTss :: [[Text]] -> Q Exp
liftTss = fmap ListE . mapM liftTs

liftMap :: M.Map Text [[Text]] -> Q Exp
liftMap m = [|M.fromList $(fmap ListE $ mapM liftPair $ M.toList m)|]

liftPair :: (Text, [[Text]]) -> Q Exp
liftPair (t, ts) = [|($(liftT t), $(liftTss ts))|]

instance Lift HaskellName where
    lift (HaskellName t) = [|HaskellName $(liftT t)|]
instance Lift DBName where
    lift (DBName t) = [|DBName $(liftT t)|]
instance Lift FieldType where
    lift (FieldType t) = [|FieldType $(liftT t)|]

instance Lift PersistFilter where
    lift Eq = [|Eq|]
    lift Ne = [|Ne|]
    lift Gt = [|Gt|]
    lift Lt = [|Lt|]
    lift Ge = [|Ge|]
    lift Le = [|Le|]
    lift In = [|In|]
    lift NotIn = [|NotIn|]
    lift (BackendSpecificFilter x) = [|BackendSpecificFilter $(liftT x)|]

instance Lift PersistUpdate where
    lift Assign = [|Assign|]
    lift Add = [|Add|]
    lift Subtract = [|Subtract|]
    lift Multiply = [|Multiply|]
    lift Divide = [|Divide|]

mkField :: EntityDef -> FieldDef -> Q (Con, Clause)
mkField et cd = do
    let con = ForallC
                []
                [EqualP (VarT $ mkName "typ") typ]
                $ NormalC name []
    bod <- lift cd
    let cla = Clause
                [ConP name []]
                (NormalB bod)
                []
    return (con, cla)
    {-
    bod <- [|Field $(lift cd)|]
    return
        [ SigD name $ ConT ''Field `AppT` ConT (mkName $ entityHaskell et) `AppT` typ
        , FunD name [Clause [] (NormalB bod) []]
        ]
    -}
  where
    name = mkName $ unpack $ concat
        [ unHaskellName $ entityHaskell et
        , upperFirst $ unHaskellName $ fieldHaskell cd
        ]
    base =
        if "Id" `isSuffixOf` unFieldType (fieldType cd)
            then ConT ''Key
                    `AppT` (VarT $ mkName "backend")
                    `AppT`
                        let len = T.length (unFieldType $ fieldType cd) - 2
                            ft = take len $ unFieldType $ fieldType cd
                            con = ConT $ mkName $ unpack $ ft ++ suffix
                         in con `AppT` VarT (mkName "backend")
            else ConT $ mkName $ unpack $ unFieldType $ fieldType cd
    typ = if nullable $ fieldAttrs cd
            then ConT ''Maybe `AppT` base
            else base

suffix :: Text
suffix = "Generic"

infixr 5 ++
(++) :: Text -> Text -> Text
(++) = append
