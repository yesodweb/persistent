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
    , MkPersistSettings
    , mpsBackend
    , mpsGeneric
    , mkPersistSettings
    , sqlSettings
    , sqlOnlySettings
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
import Database.Persist.GenericSql.Raw (SqlBackend)
import Database.Persist.Util (nullable)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Char (toLower, toUpper)
import Control.Monad (forM, (<=<), mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import qualified System.IO as SIO
import Data.Text (pack, Text, append, unpack, concat, uncons, cons)
import qualified Data.Text.IO as TIO
import Data.List (foldl')
import Data.Monoid (mappend, mconcat)
import qualified Data.Map as M
import Data.Aeson
    ( ToJSON (toJSON), FromJSON (parseJSON), (.=), object
    , Value (Object), (.:), (.:?)
    )
import Control.Applicative (pure, (<*>))

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
#ifdef GHC_7_4
    qAddDependentFile fp
#endif
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
mkPersist mps ents = do
    x <- fmap mconcat $ mapM (persistFieldFromEntity mps) ents
    y <- fmap mconcat $ mapM (mkEntity mps) ents
    z <- fmap mconcat $ mapM (mkJSON mps) ents
    return $ mconcat [x, y, z]

-- | Settings to be passed to the 'mkPersist' function.
data MkPersistSettings = MkPersistSettings
    { mpsBackend :: Type
    -- ^ Which database backend we\'re using.
    --
    -- When generating data types, each type is given a generic version- which
    -- works with any backend- and a type synonym for the commonly used
    -- backend. This is where you specify that commonly used backend.
    , mpsGeneric :: Bool
    -- ^ Create generic types that can be used with multiple backends. Good for
    -- reusable code, but makes error messages harder to understand. Default:
    -- True.
    }

-- | Create an @MkPersistSettings@ with default values.
mkPersistSettings :: Type -- ^ Value for 'mpsBackend'
                  -> MkPersistSettings
mkPersistSettings t = MkPersistSettings
    { mpsBackend = t
    , mpsGeneric = True -- FIXME switch default to False in the future
    }

-- | Use the 'SqlPersist' backend.
sqlSettings :: MkPersistSettings
sqlSettings = mkPersistSettings $ ConT ''SqlBackend

-- | Same as 'sqlSettings', but set 'mpsGeneric' to @False@.
--
-- Since 1.1.1
sqlOnlySettings :: MkPersistSettings
sqlOnlySettings = sqlSettings { mpsGeneric = False }

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

dataTypeDec :: MkPersistSettings -> EntityDef -> Dec
dataTypeDec mps t =
    DataD [] nameFinal paramsFinal constrs
    $ map (mkName . unpack) $ entityDerives t
  where
    mkCol x (FieldDef n _ ty as) =
        (mkName $ unpack $ recName x $ unHaskellName n,
         NotStrict,
         pairToType mps backend (ty, nullable as)
        )
    (nameFinal, paramsFinal)
        | mpsGeneric mps = (nameG, [PlainTV backend])
        | otherwise = (name, [])
    nameG = mkName $ unpack $ unHaskellName (entityHaskell t) ++ "Generic"
    name = mkName $ unpack $ unHaskellName $ entityHaskell t
    cols = map (mkCol $ unHaskellName $ entityHaskell t) $ entityFields t
    backend = mkName "backend"

    constrs
        | entitySum t = map sumCon $ entityFields t
        | otherwise = [RecC name cols]

    sumCon fd@(FieldDef _ _ ty _) = NormalC
        (sumConstrName t fd)
        [(NotStrict, pairToType mps backend (ty, False))]

sumConstrName :: EntityDef -> FieldDef -> Name
sumConstrName t (FieldDef n _ _ _) = mkName $ unpack $ concat
    [ unHaskellName $ entityHaskell t
    , upperFirst $ unHaskellName n
    , "Sum"
    ]

readMay :: Read a => String -> Maybe a
readMay s =
    case reads s of
        (x, _):_ -> Just x
        [] -> Nothing

entityUpdates :: EntityDef -> [(HaskellName, FieldType, Bool, PersistUpdate)]
entityUpdates =
    concatMap go . entityFields
  where
    go (FieldDef x _ y as) = map (\a -> (x, y, nullable as, a)) [minBound..maxBound]

uniqueTypeDec :: MkPersistSettings -> EntityDef -> Dec
uniqueTypeDec mps t =
    DataInstD [] ''Unique
        [genericDataType mps (unHaskellName $ entityHaskell t) $ VarT backend]
            (map (mkUnique mps backend t) $ entityUniques t)
            []
  where
    backend = mkName "backend"

mkUnique :: MkPersistSettings -> Name -> EntityDef -> UniqueDef -> Con
mkUnique mps backend t (UniqueDef (HaskellName constr) _ fields attrs) =
    NormalC (mkName $ unpack constr) types
  where
    types = map (go . flip lookup3 (entityFields t))
          $ map (unHaskellName . fst) fields

    force = "!force" `elem` attrs

    go :: (FieldType, Bool) -> (Strict, Type)
    go (ft, True) | not force = error nullErrMsg
    go (ft, y) = (NotStrict, pairToType mps backend (ft, y))

    lookup3 :: Text -> [FieldDef] -> (FieldType, Bool)
    lookup3 s [] =
        error $ unpack $ "Column not found: " ++ s ++ " in unique " ++ constr
    lookup3 x ((FieldDef (HaskellName x') _ y z):rest)
        | x == x' = (y, nullable z)
        | otherwise = lookup3 x rest

    nullErrMsg =
      mconcat [ "Error:  By default we disallow NULLables in an uniqueness "
              , "constraint.  The semantics of how NULL interacts with those "
              , "constraints is non-trivial:  two NULL values are not "
              , "considered equal for the purposes of an uniqueness "
              , "constraint.  If you understand this feature, it is possible "
              , "to use it your advantage.    *** Use a \"!force\" attribute "
              , "on the end of the line that defines your uniqueness "
              , "constraint in order to disable this check. ***" ]

pairToType :: MkPersistSettings
           -> Name -- ^ backend
           -> (FieldType, Bool) -- ^ True == has Maybe attr
           -> Type
pairToType mps backend (s, False) = idType mps backend s
pairToType mps backend (s, True) = ConT (mkName "Maybe") `AppT` idType mps backend s

backendDataType :: MkPersistSettings -> Type
backendDataType mps
    | mpsGeneric mps = VarT $ mkName "backend"
    | otherwise = mpsBackend mps

genericDataType :: MkPersistSettings
                -> Text -- ^ entity name
                -> Type -- ^ backend
                -> Type
genericDataType mps typ' backend
    | mpsGeneric mps = ConT (mkName $ unpack $ typ' ++ "Generic") `AppT` backend
    | otherwise = ConT $ mkName $ unpack typ'

idType :: MkPersistSettings -> Name -> FieldType -> Type
idType mps backend typ =
    case stripId typ of
        Just typ' ->
            ConT ''KeyBackend
            `AppT` backend'
            `AppT` genericDataType mps typ' (VarT backend)
        Nothing -> ftToType typ
  where
    backend'
        | mpsGeneric mps = VarT backend
        | otherwise = mpsBackend mps

degen :: [Clause] -> [Clause]
degen [] =
    let err = VarE (mkName "error") `AppE` LitE (StringL
                "Degenerate case, should never happen")
     in [Clause [WildP] (NormalB err) []]
degen x = x

mkToPersistFields :: String -> EntityDef -> Q Dec
mkToPersistFields constr ed@EntityDef { entitySum = isSum, entityFields = fields } = do
    clauses <-
        if isSum
            then sequence $ zipWith goSum fields [1..]
            else fmap return go
    return $ FunD (mkName "toPersistFields") clauses
  where
    go :: Q Clause
    go = do
        xs <- sequence $ replicate fieldCount $ newName "x"
        let pat = ConP (mkName constr) $ map VarP xs
        sp <- [|SomePersistField|]
        let bod = ListE $ map (AppE sp . VarE) xs
        return $ Clause [pat] (NormalB bod) []

    fieldCount = length fields

    goSum :: FieldDef -> Int -> Q Clause
    goSum fd idx = do
        let name = sumConstrName ed fd
        enull <- [|SomePersistField PersistNull|]
        let beforeCount = idx - 1
            afterCount = fieldCount - idx
            before = replicate beforeCount enull
            after = replicate afterCount enull
        x <- newName "x"
        sp <- [|SomePersistField|]
        let body = NormalB $ ListE $ mconcat
                [ before
                , [sp `AppE` VarE x]
                , after
                ]
        return $ Clause [ConP name [VarP x]] body []


mkToFieldNames :: [UniqueDef] -> Q Dec
mkToFieldNames pairs = do
    pairs' <- mapM go pairs
    return $ FunD (mkName "persistUniqueToFieldNames") $ degen pairs'
  where
    go (UniqueDef constr _ names _) = do
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
    go (UniqueDef constr _ names _) = do
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

mkHalfDefined :: Name -> Int -> Dec
mkHalfDefined constr count' =
        FunD (mkName "halfDefined")
            [Clause [] (NormalB
            $ foldl AppE (ConE constr)
                    (replicate count' $ VarE $ mkName "undefined")) []]

mkFromPersistValues :: EntityDef -> Q [Clause]
mkFromPersistValues t@(EntityDef { entitySum = False }) = do
    nothing <- [|Left $(liftT "Invalid fromPersistValues input")|]
    let cons' = ConE $ mkName $ unpack $ unHaskellName $ entityHaskell t
    xs <- mapM (const $ newName "x") $ entityFields t
    fs <- [|fromPersistValue|]
    let xs' = map (AppE fs . VarE) xs
    let pat = ListP $ map VarP xs
    ap' <- [|(<*>)|]
    just <- [|Right|]
    let cons'' = just `AppE` cons'
    return
        [ Clause [pat] (NormalB $ foldl (go ap') cons'' xs') []
        , Clause [WildP] (NormalB nothing) []
        ]
  where
    go ap' x y = InfixE (Just x) ap' (Just y)
mkFromPersistValues t@(EntityDef { entitySum = True }) = do
    nothing <- [|Left $(liftT "Invalid fromPersistValues input")|]
    clauses <- mkClauses [] $ entityFields t
    return $ clauses `mappend` [Clause [WildP] (NormalB nothing) []]
  where
    mkClauses _ [] = return []
    mkClauses before (field:after) = do
        x <- newName "x"
        let null' = ConP 'PersistNull []
            pat = ListP $ mconcat
                [ map (const null') before
                , [VarP x]
                , map (const null') after
                ]
            constr = ConE $ sumConstrName t field
        fmap' <- [|fmap|]
        fs <- [|fromPersistValue $(return $ VarE x)|]
        let clause = Clause [pat] (NormalB $ InfixE (Just constr) fmap' (Just fs)) []
        clauses <- mkClauses (field : before) after
        return $ clause : clauses

mkEntity :: MkPersistSettings -> EntityDef -> Q [Dec]
mkEntity mps t = do
    t' <- lift t
    let nameT = unHaskellName $ entityHaskell t
    let nameS = unpack nameT
    let clazz = ConT ''PersistEntity `AppT` genericDataType mps (unHaskellName $ entityHaskell t) (VarT $ mkName "backend")
    tpf <- mkToPersistFields nameS t
    fpv <- mkFromPersistValues t
    utv <- mkUniqueToValues $ entityUniques t
    puk <- mkUniqueKeys t

    fields <- mapM (mkField mps t) $ FieldDef
        (HaskellName "Id")
        (entityID t)
        (FTTypeCon Nothing $ unHaskellName (entityHaskell t) ++ "Id")
        []
        : entityFields t
    toFieldNames <- mkToFieldNames $ entityUniques t

    let addSyn -- FIXME maybe remove this
            | mpsGeneric mps = (:) $
                TySynD (mkName nameS) [] $
                    genericDataType mps nameT $ mpsBackend mps
            | otherwise = id

    return $ addSyn
      [ dataTypeDec mps t
      , TySynD (mkName $ unpack $ unHaskellName (entityHaskell t) ++ "Id") [] $
            ConT ''KeyBackend `AppT` mpsBackend mps `AppT` ConT (mkName nameS)
      , InstanceD [] clazz $
        [ uniqueTypeDec mps t
        , FunD (mkName "entityDef") [Clause [WildP] (NormalB t') []]
        , tpf
        , FunD (mkName "fromPersistValues") fpv
        , mkHalfDefined
            (if entitySum t
                then sumConstrName t (head $ entityFields t)
                else mkName nameS)
            (if entitySum t then 1 else length $ entityFields t)
        , toFieldNames
        , utv
        , puk
        , DataInstD
            []
            ''EntityField
            [ genericDataType mps nameT $ VarT $ mkName "backend"
            , VarT $ mkName "typ"
            ]
            (map fst fields)
            []
        , FunD (mkName "persistFieldDef") (map snd fields)
        , TySynInstD
            (mkName "PersistEntityBackend")
            [genericDataType mps (unHaskellName $ entityHaskell t) $ VarT $ mkName "backend"]
            (backendDataType mps)
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
persistFieldFromEntity :: MkPersistSettings -> EntityDef -> Q [Dec]
persistFieldFromEntity mps e = do
    ss <- [|SqlString|]
    let columnNames = map (unpack . unHaskellName . fieldHaskell) (entityFields e)
    obj <- [|\ent -> PersistMap $ zip (map pack columnNames) (map toPersistValue $ toPersistFields ent)|]
    fpv <- [|\x -> fromPersistValues $ map (\(_,v) -> case fromPersistValue v of
                                                      Left e' -> error $ unpack e'
                                                      Right r -> r) x|]
    let typ = genericDataType mps (pack entityName) $ VarT $ mkName "backend"

    compose <- [|(<=<)|]
    getPersistMap' <- [|getPersistMap|]
    return
        [ persistFieldInstanceD typ
            [ sqlTypeFunD ss
            , FunD (mkName "toPersistValue") [ Clause [] (NormalB obj) [] ]
            , FunD (mkName "fromPersistValue")
                [ Clause [] (NormalB $ InfixE (Just fpv) compose $ Just getPersistMap') []
                ]
            ]
        ]
    where
      entityName = (unpack $ unHaskellName $ entityHaskell e)

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
mkDeleteCascade :: MkPersistSettings -> [EntityDef] -> Q [Dec]
mkDeleteCascade mps defs = do
    let deps = concatMap getDeps defs
    mapM (go deps) defs
  where
    getDeps :: EntityDef -> [Dep]
    getDeps def =
        concatMap getDeps' $ entityFields def
      where
        getDeps' :: FieldDef -> [Dep]
        getDeps' (FieldDef name _ ftyp attribs) =
            let isNull = nullable attribs
             in case stripId ftyp of
                    Just f ->
                         return Dep
                            { depTarget = f
                            , depSourceTable = entityHaskell def
                            , depSourceField = name
                            , depSourceNull = isNull
                            }
                    Nothing -> []
    go :: [Dep] -> EntityDef -> Q Dec
    go allDeps EntityDef{entityHaskell = name} = do
        let deps = filter (\x -> depTarget x == unHaskellName name) allDeps
        key <- newName "key"
        let del = VarE 'delete
        let dcw = VarE 'deleteCascadeWhere
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

        let entityT = genericDataType mps (unHaskellName name) $ VarT $ mkName "backend"

        return $
            InstanceD
            [ ClassP ''PersistQuery [VarT $ mkName "m"]
            , EqualP (ConT ''PersistEntityBackend `AppT` entityT) (ConT ''PersistMonadBackend `AppT` VarT (mkName "m"))
            ]
            (ConT ''DeleteCascade `AppT` entityT `AppT` VarT (mkName "m"))
            [ FunD (mkName "deleteCascade")
                [Clause [VarP key] (NormalB $ DoE stmts) []]
            ]

mkUniqueKeys :: EntityDef -> Q Dec
mkUniqueKeys def | entitySum def =
    return $ FunD (mkName "persistUniqueKeys") [Clause [WildP] (NormalB $ ListE []) []]
mkUniqueKeys def = do
    c <- clause
    return $ FunD (mkName "persistUniqueKeys") [c]
  where
    clause = do
        xs <- forM (entityFields def) $ \(FieldDef x _ _ _) -> do
            x' <- newName $ '_' : unpack (unHaskellName x)
            return (x, x')
        let pcs = map (go xs) $ entityUniques def
        let pat = ConP
                (mkName $ unpack $ unHaskellName $ entityHaskell def)
                (map (VarP . snd) xs)
        return $ Clause [pat] (NormalB $ ListE pcs) []

    go :: [(HaskellName, Name)] -> UniqueDef -> Exp
    go xs (UniqueDef name _ cols _) =
        foldl' (go' xs) (ConE (mkName $ unpack $ unHaskellName name)) (map fst cols)

    go' :: [(HaskellName, Name)] -> Exp -> HaskellName -> Exp
    go' xs front col =
        let Just col' = lookup col xs
         in front `AppE` VarE col'

sqlTypeFunD :: Exp -> Dec
sqlTypeFunD st = FunD (mkName "sqlType")
                [ Clause [WildP] (NormalB st) [] ]

persistFieldInstanceD :: Type -> [Dec] -> Dec
persistFieldInstanceD typ =
   InstanceD [] (ConT ''PersistField `AppT` typ)

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
        [ persistFieldInstanceD (ConT $ mkName s)
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
            _  -> do
              defsName <- newName "defs"
              defsStmt <- do
                u <- [|undefined|]
                e <- [|entityDef|]
                let defsExp = ListE $ map (AppE e . undefinedEntityTH u) defs
                return $ LetS [ValD (VarP defsName) (NormalB defsExp) []]
              stmts <- mapM (toStmt $ VarE defsName) defs
              return (DoE $ defsStmt : stmts)
    toStmt :: Exp -> EntityDef -> Q Stmt
    toStmt defsExp ed = do
        u <- [|undefined|]
        m <- [|migrate|]
        return $ NoBindS $ m `AppE` defsExp `AppE` (undefinedEntityTH u ed)
    undefinedEntityTH :: Exp -> EntityDef -> Exp
    undefinedEntityTH u = SigE u . ConT . mkName . unpack . unHaskellName . entityHaskell

instance Lift EntityDef where
    lift (EntityDef a b c d e f g h i) =
        [|EntityDef
            $(lift a)
            $(lift b)
            $(lift c)
            $(liftTs d)
            $(lift e)
            $(lift f)
            $(liftTs g)
            $(liftMap h)
            $(lift i)
            |]
instance Lift FieldDef where
    lift (FieldDef a b c d) = [|FieldDef $(lift a) $(lift b) $(lift c) $(liftTs d)|]
instance Lift UniqueDef where
    lift (UniqueDef a b c d) = [|UniqueDef $(lift a) $(lift b) $(lift c) $(liftTs d)|]

pack' :: String -> Text
pack' = pack
#if !MIN_VERSION_text(0, 11, 2)
{-# NOINLINE pack' #-}
#endif

liftT :: Text -> Q Exp
liftT t = [|pack' $(lift (unpack t))|]

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
    lift (FTTypeCon Nothing t)   = [|FTTypeCon Nothing $(liftT t)|]
    lift (FTTypeCon (Just x) t)   = [|FTTypeCon (Just $(liftT x)) $(liftT t)|]
    lift (FTApp x y) = [|FTApp $(lift x) $(lift y)|]
    lift (FTList x) = [|FTList $(lift x)|]

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

-- Ent
--   fieldName FieldType
--
-- forall . typ ~ FieldType => EntFieldName
--
-- EntFieldName = FieldDef ....
mkField :: MkPersistSettings -> EntityDef -> FieldDef -> Q (Con, Clause)
mkField mps et cd = do
    let con = ForallC
                []
                [EqualP (VarT $ mkName "typ") maybeTyp]
                $ NormalC name []
    bod <- lift cd
    let cla = Clause
                [ConP name []]
                (NormalB bod)
                []
    return (con, cla)
  where
    name = mkName $ unpack $ concat
        [ unHaskellName $ entityHaskell et
        , upperFirst $ unHaskellName $ fieldHaskell cd
        ]
    maybeTyp =
        if nullable $ fieldAttrs cd
            then ConT ''Maybe `AppT` typ
            else typ
    typ =
        case stripId $ fieldType cd of
            Just ft ->
                 ConT ''KeyBackend
                    `AppT` (if mpsGeneric mps
                                then VarT $ mkName "backend"
                                else mpsBackend mps)
                    `AppT` genericDataType mps ft (VarT $ mkName "backend")
            Nothing -> ftToType $ fieldType cd

ftToType :: FieldType -> Type
ftToType (FTTypeCon Nothing t) = ConT $ mkName $ unpack t
ftToType (FTTypeCon (Just m) t) = ConT $ mkName $ unpack $ concat [m, ".", t]
ftToType (FTApp x y) = ftToType x `AppT` ftToType y
ftToType (FTList x) = ListT `AppT` ftToType x

infixr 5 ++
(++) :: Text -> Text -> Text
(++) = append

mkJSON :: MkPersistSettings -> EntityDef -> Q [Dec]
mkJSON _ def | not ("json" `elem` entityAttrs def) = return []
mkJSON mps def = do
    pureE <- [|pure|]
    apE' <- [|(<*>)|]
    packE <- [|pack|]
    dotEqualE <- [|(.=)|]
    dotColonE <- [|(.:)|]
    dotColonQE <- [|(.:?)|]
    objectE <- [|object|]
    obj <- newName "obj"
    mzeroE <- [|mzero|]

    xs <- mapM (newName . unpack . unHaskellName . fieldHaskell)
        $ entityFields def

    let conName = mkName $ unpack $ unHaskellName $ entityHaskell def
        typ = genericDataType mps (unHaskellName $ entityHaskell def) $ VarT $ mkName "backend"
        toJSONI = InstanceD
            []
            (ConT ''ToJSON `AppT` typ)
            [toJSON']
        toJSON' = FunD 'toJSON $ return $ Clause
            [ConP conName $ map VarP xs]
            (NormalB $ objectE `AppE` ListE pairs)
            []
        pairs = zipWith toPair (entityFields def) xs
        toPair f x = InfixE
            (Just (packE `AppE` LitE (StringL $ unpack $ unHaskellName $ fieldHaskell f)))
            dotEqualE
            (Just $ VarE x)
        fromJSONI = InstanceD
            []
            (ConT ''FromJSON `AppT` typ)
            [parseJSON']
        parseJSON' = FunD 'parseJSON
            [ Clause [ConP 'Object [VarP obj]]
                (NormalB $ foldl'
                    (\x y -> InfixE (Just x) apE' (Just y))
                    (pureE `AppE` ConE conName)
                    pulls
                )
                []
            , Clause [WildP] (NormalB mzeroE) []
            ]
        pulls = map toPull $ entityFields def
        toPull f = InfixE
            (Just $ VarE obj)
            (if nullable (fieldAttrs f) then dotColonQE else dotColonE)
            (Just $ AppE packE $ LitE $ StringL $ unpack $ unHaskellName $ fieldHaskell f)
    return [toJSONI, fromJSONI]
