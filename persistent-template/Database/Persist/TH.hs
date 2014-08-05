{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-fields #-}
-- | This module provides utilities for creating backends. Regular users do not
-- need to use this module.
module Database.Persist.TH
    ( -- * Parse entity defs
      persistWith
    , persistUpperCase
    , persistLowerCase
    , persistFileWith
      -- * Turn @EntityDef@s into types
    , mkPersist
    , MkPersistSettings
    , mpsBackend
    , mpsGeneric
    , mpsPrefixFields
    , mpsEntityJSON
    , mpsGenerateLenses
    , EntityJSON(..)
    , mkPersistSettings
    , sqlSettings
    , sqlOnlySettings
      -- * Various other TH functions
    , mkMigrate
    , mkSave
    , mkDeleteCascade
    , share
    , derivePersistField
    , derivePersistFieldJSON
    , persistFieldFromEntity
      -- * Internal
    , packPTH
    , lensPTH
    ) where

import Prelude hiding ((++), take, concat, splitAt)
import qualified Prelude as P 
import Database.Persist
import Database.Persist.Sql (Migration, migrate, SqlBackend, PersistFieldSql)
import Database.Persist.Quasi
import Language.Haskell.TH.Lib (varE)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Char (toLower, toUpper)
import Control.Monad (forM, (<=<), mzero)
import qualified System.IO as SIO
import Data.Text (pack, Text, append, unpack, concat, uncons, cons, stripPrefix, stripSuffix)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import Data.List (foldl')
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Monoid (mappend, mconcat)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Aeson
    ( ToJSON (toJSON), FromJSON (parseJSON), (.=), object
    , Value (Object), (.:), (.:?)
    , encode, eitherDecodeStrict'
    )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Applicative (pure, (<*>))
import Database.Persist.Sql (sqlType)
import Data.Proxy (Proxy (Proxy))

-- | Converts a quasi-quoted syntax into a list of entity definitions, to be
-- used as input to the template haskell generation code (mkPersist).
persistWith :: PersistSettings -> QuasiQuoter
persistWith ps = QuasiQuoter
    { quoteExp = parseReferences ps . pack
    }

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
    parseReferences ps s

-- calls parse to Quasi.parse individual entities in isolation
-- afterwards, sets references to other entities
parseReferences :: PersistSettings -> Text -> Q Exp
parseReferences ps s = lift $
     map (mkEntityDefSqlTypeExp entityMap) entsWithEmbeds
  where
    -- every EntityDef could reference each-other (as an EmbeddedRef)
    -- let Haskell tie the knot
    entityMap = M.fromList $ map (\ent -> (entityHaskell ent, toEmbeddedDef ent)) entsWithEmbeds
    entsWithEmbeds = map setEmbedEntity rawEnts
    setEmbedEntity ent = ent
      { entityFields = map (setEmbedField entityMap) $ entityFields ent
      }
    rawEnts = parse ps s


stripId :: FieldType -> Maybe Text
stripId (FTTypeCon Nothing t) = stripSuffix "Id" t
stripId _ = Nothing

foreignReference :: FieldDef -> Maybe HaskellName
foreignReference field = case fieldReference field of
    ForeignRef ref -> Just ref
    _              -> Nothing


-- fieldSqlType at parse time can be an Exp
-- This helps delay setting fieldSqlType until lift time
data EntityDefSqlTypeExp = EntityDefSqlTypeExp EntityDef [SqlTypeExp]
                           deriving Show

data SqlTypeExp = SqlTypeExp FieldType
                | SqlType' SqlType
                deriving Show

instance Lift SqlTypeExp where
    lift (SqlType' t)       = lift t
    lift (SqlTypeExp ftype) = return st
      where
        typ = ftToType ftype
        mtyp = (ConT ''Proxy `AppT` typ)
        typedNothing = SigE (ConE 'Proxy) mtyp
        st = VarE 'sqlType `AppE` typedNothing

data FieldsSqlTypeExp = FieldsSqlTypeExp [FieldDef] [SqlTypeExp]

instance Lift FieldsSqlTypeExp where
    lift (FieldsSqlTypeExp fields sqlTypeExps) =
        lift $ zipWith FieldSqlTypeExp fields sqlTypeExps

data FieldSqlTypeExp = FieldSqlTypeExp FieldDef SqlTypeExp
instance Lift FieldSqlTypeExp where
    lift (FieldSqlTypeExp (FieldDef{..}) sqlTypeExp) =
      [|FieldDef fieldHaskell fieldDB fieldType $(lift sqlTypeExp) $(liftTs fieldAttrs) fieldStrict fieldReference|]

instance Lift EntityDefSqlTypeExp where
    lift (EntityDefSqlTypeExp (EntityDef{..}) sqlTypeExps) =
        [|EntityDef
            $(lift entityHaskell)
            $(lift entityDB)
            $(lift entityID)
            $(liftTs entityAttrs)
            $(lift (FieldsSqlTypeExp entityFields sqlTypeExps))
            $(lift entityPrimary)
            $(lift entityUniques)
            $(lift entityForeigns)
            $(liftTs entityDerives)
            $(liftMap entityExtra)
            $(lift entitySum)
            |]

instance Lift ReferenceDef where
    lift NoReference = [|NoReference|]
    lift (ForeignRef name) = [|ForeignRef name|]
    lift (EmbeddedRef em) = [|EmbeddedRef em|]

instance Lift EmbeddedDef where
    lift (EmbeddedDef name fields) = [|EmbeddedDef name fields|]

instance Lift EmbeddedFieldDef where
    lift (EmbeddedFieldDef name em) = [|EmbeddedFieldDef name em|]

type EntityMap = M.Map HaskellName EmbeddedDef
mEmbedded :: EntityMap -> FieldType -> Maybe EmbeddedDef
mEmbedded _ (FTTypeCon Just{} _) = Nothing
mEmbedded ents (FTTypeCon Nothing n) = let name = HaskellName n in
    M.lookup name ents
mEmbedded ents (FTList x) = mEmbedded ents x
mEmbedded ents (FTApp x y) = maybe (mEmbedded ents y) Just (mEmbedded ents x)

setEmbedField :: EntityMap -> FieldDef -> FieldDef
setEmbedField allEntities field = field
  { fieldReference = case fieldReference field of
      NoReference -> case mEmbedded allEntities (fieldType field) of
          Nothing -> case stripId $ fieldType field of
              Nothing -> NoReference
              Just name -> if M.member (HaskellName name) allEntities
                  then ForeignRef $ HaskellName name
                  else NoReference
          Just em -> EmbeddedRef em
      existing@_   -> existing
  }

mkEntityDefSqlTypeExp :: EntityMap -> EntityDef -> EntityDefSqlTypeExp
mkEntityDefSqlTypeExp allEntities ent = EntityDefSqlTypeExp ent
    $ (map getSqlType $ entityFields ent)
  where
    getSqlType field = maybe
        (defaultSqlTypeExp field)
        (SqlType' . SqlOther)
        (listToMaybe $ mapMaybe (stripPrefix "sqltype=") $ fieldAttrs field)


    -- In the case of embedding, there won't be any datatype created yet.
    -- We just use SqlString, as the data will be serialized to JSON.
    defaultSqlTypeExp field
        | isJust (mEmbedded allEntities ftype) = SqlType' SqlString
        | isReference = SqlType' SqlInt64
        | otherwise =
            case ftype of
                -- In the case of lists, we always serialize to a string
                -- value (via JSON).
                --
                -- Normally, this would be determined automatically by
                -- SqlTypeExp. However, there's one corner case: if there's
                -- a list of entity IDs, the datatype for the ID has not
                -- yet been created, so the compiler will fail. This extra
                -- clause works around this limitation.
                FTList _ -> SqlType' SqlString
                _ -> SqlTypeExp ftype
      where
        isReference = isJust (foreignReference field)
        ftype = fieldType field

-- | Create data types and appropriate 'PersistEntity' instances for the given
-- 'EntityDef's. Works well with the persist quasi-quoter.
mkPersist :: MkPersistSettings -> [EntityDef] -> Q [Dec]
mkPersist mps ents' = do
    x <- fmap mconcat $ mapM (persistFieldFromEntity mps) ents
    y <- fmap mconcat $ mapM (mkEntity mps) ents
    z <- fmap mconcat $ mapM (mkJSON mps) ents
    return $ mconcat [x, y, z]
  where
    ents = map fixEntityDef ents'

-- | Implement special preprocessing on EntityDef as necessary for 'mkPersist'.
-- For example, strip out any fields marked as MigrationOnly.
fixEntityDef :: EntityDef -> EntityDef
fixEntityDef ed =
    ed { entityFields = filter keepField $ entityFields ed }
  where
    keepField fd = "MigrationOnly" `notElem` fieldAttrs fd &&
                   "SafeToRemove" `notElem` fieldAttrs fd

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
    , mpsPrefixFields :: Bool
    -- ^ Prefix field names with the model name. Default: True.
    , mpsEntityJSON :: Maybe EntityJSON
    -- ^ Generate @ToJSON@/@FromJSON@ instances for each model types. If it's
    -- @Nothing@, no instances will be generated. Default:
    --
    -- @
    --  Just EntityJSON
    --      { entityToJSON = 'keyValueEntityToJSON
    --      , entityFromJSON = 'keyValueEntityFromJSON
    --      }
    -- @
    , mpsGenerateLenses :: !Bool
    -- ^ Instead of generating normal field accessors, generator lens-style accessors.
    --
    -- Default: False
    --
    -- Since 1.3.1
    }

data EntityJSON = EntityJSON
    { entityToJSON :: Name
    -- ^ Name of the @toJSON@ implementation for @Entity a@.
    , entityFromJSON :: Name
    -- ^ Name of the @fromJSON@ implementation for @Entity a@.
    }

-- | Create an @MkPersistSettings@ with default values.
mkPersistSettings :: Type -- ^ Value for 'mpsBackend'
                  -> MkPersistSettings
mkPersistSettings t = MkPersistSettings
    { mpsBackend = t
    , mpsGeneric = False
    , mpsPrefixFields = True
    , mpsEntityJSON = Just EntityJSON
        { entityToJSON = 'entityIdToJSON
        , entityFromJSON = 'entityIdFromJSON
        }
    , mpsGenerateLenses = False
    }

-- | Use the 'SqlPersist' backend.
sqlSettings :: MkPersistSettings
sqlSettings = mkPersistSettings $ ConT ''SqlBackend

-- | Same as 'sqlSettings'.
--
-- Since 1.1.1
sqlOnlySettings :: MkPersistSettings
sqlOnlySettings = sqlSettings
{-# DEPRECATED sqlOnlySettings "use sqlSettings" #-}

recNameNoUnderscore :: MkPersistSettings -> Text -> Text -> Text
recNameNoUnderscore mps dt f
  | mpsPrefixFields mps = lowerFirst dt ++ upperFirst f
  | otherwise           = lowerFirst f

recName :: MkPersistSettings -> Text -> Text -> Text
recName mps dt f =
    addUnderscore $ recNameNoUnderscore mps dt f
  where
    addUnderscore
        | mpsGenerateLenses mps = ("_" ++)
        | otherwise = id

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
    mkCol x fd@FieldDef {..} =
        (mkName $ unpack $ recName mps x $ unHaskellName fieldHaskell,
         if fieldStrict then IsStrict else NotStrict,
         pairToType mps backend (fd, nullable fieldAttrs)
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

    sumCon fd = NormalC
        (sumConstrName mps t fd)
        [(NotStrict, pairToType mps backend (fd, NotNullable))]

sumConstrName :: MkPersistSettings -> EntityDef -> FieldDef -> Name
sumConstrName mps t FieldDef {..} = mkName $ unpack $ concat
    [ if mpsPrefixFields mps
        then unHaskellName $ entityHaskell t
        else ""
    , upperFirst $ unHaskellName fieldHaskell
    , "Sum"
    ]

readMay :: Read a => String -> Maybe a
readMay s =
    case reads s of
        (x, _):_ -> Just x
        [] -> Nothing

entityUpdates :: EntityDef -> [(HaskellName, FieldType, IsNullable, PersistUpdate)]
entityUpdates =
    concatMap go . entityFields
  where
    go FieldDef {..} = map (\a -> (fieldHaskell, fieldType, nullable fieldAttrs, a)) [minBound..maxBound]

uniqueTypeDec :: MkPersistSettings -> EntityDef -> Dec
uniqueTypeDec mps t =
    DataInstD [] ''Unique
        [genericDataType mps (entityHaskell t) $ VarT backend]
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

    go :: (FieldDef, IsNullable) -> (Strict, Type)
    go (_, Nullable _) | not force = error nullErrMsg
    go (fd, y) = (NotStrict, pairToType mps backend (fd, y))

    lookup3 :: Text -> [FieldDef] -> (FieldDef, IsNullable)
    lookup3 s [] =
        error $ unpack $ "Column not found: " ++ s ++ " in unique " ++ constr
    lookup3 x (fd@FieldDef {..}:rest)
        | x == unHaskellName fieldHaskell = (fd, nullable fieldAttrs)
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
           -> (FieldDef, IsNullable)
           -> Type
pairToType mps backend (fd, Nullable ByMaybeAttr) =
  ConT ''Maybe `AppT` idType mps backend fd
pairToType mps backend (fd, _) = idType mps backend fd

backendDataType :: MkPersistSettings -> Type
backendDataType mps
    | mpsGeneric mps = VarT $ mkName "backend"
    | otherwise = mpsBackend mps

genericDataType :: MkPersistSettings
                -> HaskellName -- ^ entity name
                -> Type -- ^ backend
                -> Type
genericDataType mps (HaskellName typ') backend
    | mpsGeneric mps = ConT (mkName $ unpack $ typ' ++ "Generic") `AppT` backend
    | otherwise = ConT $ mkName $ unpack typ'

idType :: MkPersistSettings -> Name -> FieldDef -> Type
idType mps backend fd =
    case foreignReference fd of
        Just typ' ->
            ConT ''KeyBackend
            `AppT` backend'
            `AppT` genericDataType mps typ' (VarT backend)
        Nothing -> ftToType typ
  where
    typ = fieldType fd
    backend'
        | mpsGeneric mps = VarT backend
        | otherwise = mpsBackend mps

degen :: [Clause] -> [Clause]
degen [] =
    let err = VarE 'error `AppE` LitE (StringL
                "Degenerate case, should never happen")
     in [Clause [WildP] (NormalB err) []]
degen x = x

mkToPersistFields :: MkPersistSettings -> String -> EntityDef -> Q Dec
mkToPersistFields mps constr ed@EntityDef { entitySum = isSum, entityFields = fields } = do
    clauses <-
        if isSum
            then sequence $ zipWith goSum fields [1..]
            else fmap return go
    return $ FunD 'toPersistFields clauses
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
        let name = sumConstrName mps ed fd
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
    return $ FunD 'persistUniqueToFieldNames $ degen pairs'
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
    return $ FunD 'persistUniqueToValues $ degen pairs'
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

mkToValue :: String -> [String] -> Dec
mkToValue func = FunD (mkName func) . degen . map go
  where
    go constr =
        let x = mkName "x"
         in Clause [ConP (mkName constr) [VarP x]]
                   (NormalB $ VarE 'toPersistValue `AppE` VarE x)
                   []

isNotNull :: PersistValue -> Bool
isNotNull PersistNull = False
isNotNull _ = True

mkFromPersistValues :: MkPersistSettings -> EntityDef -> Q [Clause]
mkFromPersistValues mps t@(EntityDef { entitySum = False }) = do
    nothing <- [|Left $(liftT $ "Invalid fromPersistValues input. Entity: " `mappend` entName)|]
    let cons' = ConE $ mkName $ unpack $ entName
    xs <- mapM (const $ newName "x") $ entityFields t
    mkPersistValues <- mapM (mkPersistValue . unHaskellName . fieldHaskell) $ entityFields t
    let xs' = map (\(pv, x) -> pv `AppE` VarE x) $ zip mkPersistValues xs
    let pat = ListP $ map VarP xs
    ap' <- [|(<*>)|]
    just <- [|Right|]
    let cons'' = just `AppE` cons'
    return
        [ Clause [pat] (NormalB $ foldl (go ap') cons'' xs') []
        , Clause [WildP] (NormalB nothing) []
        ]
  where
    mkPersistValue fieldName = [|\persistValue ->
            case fromPersistValue persistValue of
                   Right r  -> Right r
                   Left err -> Left $
                     "field " `mappend` $(liftT fieldName) `mappend` ": " `mappend` err
          |]
    entName = unHaskellName $ entityHaskell t
    go ap' x y = InfixE (Just x) ap' (Just y)

mkFromPersistValues mps t@(EntityDef { entitySum = True }) = do
    nothing <- [|Left $(liftT $ "Invalid fromPersistValues input: sum type with all nulls. Entity: " `mappend` entName)|]
    clauses <- mkClauses [] $ entityFields t
    return $ clauses `mappend` [Clause [WildP] (NormalB nothing) []]
  where
    entName = unHaskellName $ entityHaskell t
    mkClauses _ [] = return []
    mkClauses before (field:after) = do
        x <- newName "x"
        let null' = ConP 'PersistNull []
            pat = ListP $ mconcat
                [ map (const null') before
                , [VarP x]
                , map (const null') after
                ]
            constr = ConE $ sumConstrName mps t field
        fmap' <- [|fmap|]
        fs <- [|fromPersistValue $(return $ VarE x)|]
        let guard' = NormalG $ VarE 'isNotNull `AppE` VarE x
        let clause = Clause [pat] (GuardedB [(guard', InfixE (Just constr) fmap' (Just fs))]) []
        clauses <- mkClauses (field : before) after
        return $ clause : clauses

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lensPTH :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lensPTH sa sbt afb s = fmap (sbt s) (afb $ sa s)

mkLensClauses :: MkPersistSettings -> EntityDef -> Q [Clause]
mkLensClauses mps t = do
    lens' <- [|lensPTH|]
    getId <- [|entityKey|]
    setId <- [|\(Entity _ value) key -> Entity key value|]
    getVal <- [|entityVal|]
    dot <- [|(.)|]
    keyName <- newName "key"
    valName <- newName "value"
    xName <- newName "x"
    let idClause = Clause
            [ConP (mkName $ unpack $ unHaskellName (entityHaskell t) ++ "Id") []]
            (NormalB $ lens' `AppE` getId `AppE` setId)
            []
    if entitySum t
        then return $ idClause : map (toSumClause lens' keyName valName xName) (entityFields t)
        else return $ idClause : map (toClause lens' getVal dot keyName valName xName) (entityFields t)
  where
    toClause lens' getVal dot keyName valName xName f = Clause
        [ConP (filterConName mps t f) []]
        (NormalB $ lens' `AppE` getter `AppE` setter)
        []
      where
        fieldName = mkName $ unpack $ recName mps (unHaskellName $ entityHaskell t) (unHaskellName $ fieldHaskell f)
        getter = InfixE (Just $ VarE fieldName) dot (Just getVal)
        setter = LamE
            [ ConP 'Entity [VarP keyName, VarP valName]
            , VarP xName
            ]
            $ ConE 'Entity `AppE` VarE keyName `AppE` RecUpdE
                (VarE valName)
                [(fieldName, VarE xName)]

    toSumClause lens' keyName valName xName f = Clause
        [ConP (filterConName mps t f) []]
        (NormalB $ lens' `AppE` getter `AppE` setter)
        []
      where
        emptyMatch = Match WildP (NormalB $ VarE 'error `AppE` LitE (StringL "Tried to use fieldLens on a Sum type")) []
        getter = LamE
            [ ConP 'Entity [WildP, VarP valName]
            ] $ CaseE (VarE valName)
            $ Match (ConP (sumConstrName mps t f) [VarP xName]) (NormalB $ VarE xName) []

            -- FIXME It would be nice if the types expressed that the Field is
            -- a sum type and therefore could result in Maybe.
            : if length (entityFields t) > 1 then [emptyMatch] else []
        setter = LamE
            [ ConP 'Entity [VarP keyName, WildP]
            , VarP xName
            ]
            $ ConE 'Entity `AppE` VarE keyName `AppE` (ConE (sumConstrName mps t f) `AppE` VarE xName)

mkEntity :: MkPersistSettings -> EntityDef -> Q [Dec]
mkEntity mps t = do
    t' <- lift t
    let entName = entityHaskell t
    let nameT = unHaskellName entName
    let nameS = unpack nameT
    let clazz = ConT ''PersistEntity `AppT` genericDataType mps entName (VarT $ mkName "backend")
    tpf <- mkToPersistFields mps nameS t
    fpv <- mkFromPersistValues mps t
    utv <- mkUniqueToValues $ entityUniques t
    puk <- mkUniqueKeys t
    fkc <- mapM (mkForeignKeysComposite mps t) $ entityForeigns t

    let primaryField = FieldDef
          { fieldHaskell = HaskellName "Id"
          , fieldDB = entityID t
          , fieldType = FTTypeCon Nothing $ unHaskellName entName ++ "Id"
          , fieldSqlType = maybe SqlInt64 (const $ SqlOther "Primary Key") $ entityPrimary t
          -- the primary field is actually a reference to the entity
          , fieldReference = ForeignRef entName
          , fieldAttrs = []
          , fieldStrict = True
          }
    
    fields <- mapM (mkField mps t) $ primaryField : entityFields t
    toFieldNames <- mkToFieldNames $ entityUniques t

    let addSyn -- FIXME maybe remove this
            | mpsGeneric mps = (:) $
                TySynD (mkName nameS) [] $
                    genericDataType mps entName $ mpsBackend mps
            | otherwise = id

    lensClauses <- mkLensClauses mps t

    lenses <- mkLenses mps t

    return $ addSyn $
       dataTypeDec mps t : mconcat fkc `mappend`
      ([ TySynD (mkName $ unpack $ unHaskellName entName ++ "Id") [] $
            ConT ''KeyBackend `AppT` mpsBackend mps `AppT` ConT (mkName nameS)
      , InstanceD [] clazz $
        [ uniqueTypeDec mps t
        , FunD 'entityDef [Clause [WildP] (NormalB t') []]
        , tpf
        , FunD 'fromPersistValues fpv
        , toFieldNames
        , utv
        , puk
        , DataInstD
            []
            ''EntityField
            [ genericDataType mps entName $ VarT $ mkName "backend"
            , VarT $ mkName "typ"
            ]
            (map fst fields)
            []
        , FunD 'persistFieldDef (map snd fields)
        , TySynInstD
            ''PersistEntityBackend
#if MIN_VERSION_template_haskell(2,9,0)
            (TySynEqn
               [genericDataType mps entName $ VarT $ mkName "backend"]
               (backendDataType mps))
#else
            [genericDataType mps entName $ VarT $ mkName "backend"]
            (backendDataType mps)
#endif
        , FunD 'persistIdField [Clause [] (NormalB $ ConE $ mkName $ unpack $ unHaskellName entName ++ "Id") []]
        , FunD 'fieldLens lensClauses
        ]
      ] `mappend` lenses)

mkLenses :: MkPersistSettings -> EntityDef -> Q [Dec]
mkLenses mps _ | not (mpsGenerateLenses mps) = return []
mkLenses _ ent | entitySum ent = return []
mkLenses mps ent = fmap mconcat $ forM (entityFields ent) $ \field -> do
    let lensName' = recNameNoUnderscore mps (unHaskellName $ entityHaskell ent) (unHaskellName $ fieldHaskell field)
        lensName = mkName $ unpack lensName'
        fieldName = mkName $ unpack $ "_" ++ lensName'
    needleN <- newName "needle"
    setterN <- newName "setter"
    fN <- newName "f"
    aN <- newName "a"
    yN <- newName "y"
    let needle = VarE needleN
        setter = VarE setterN
        f = VarE fN
        a = VarE aN
        y = VarE yN
        fT = mkName "f"
        -- FIXME if we want to get really fancy, then: if this field is the
        -- *only* Id field present, then set backend1 and backend2 to different
        -- values
        backend1 = mkName "backend"
        backend2 = mkName "backend"
        aT = pairToType mps backend1 (field, nullable $ fieldAttrs field)
        bT = pairToType mps backend2 (field, nullable $ fieldAttrs field)
        mkST backend = genericDataType mps (entityHaskell ent) (VarT backend)
        sT = mkST backend1
        tT = mkST backend2
        t1 `arrow` t2 = ArrowT `AppT` t1 `AppT` t2
        vars = PlainTV fT
             : (if mpsGeneric mps then [PlainTV backend1{-, PlainTV backend2-}] else [])
    return
        [ SigD lensName $ ForallT vars [ClassP ''Functor [VarT fT]] $
            (aT `arrow` (VarT fT `AppT` bT)) `arrow`
            (sT `arrow` (VarT fT `AppT` tT))
        , FunD lensName $ return $ Clause
            [VarP fN, VarP aN]
            (NormalB $ VarE 'fmap
                `AppE` setter
                `AppE` (f `AppE` needle))
            [ FunD needleN [Clause [] (NormalB $ VarE fieldName `AppE` a) []]
            , FunD setterN $ return $ Clause
                [VarP yN]
                (NormalB $ RecUpdE a
                    [ (fieldName, y)
                    ])
                []
            ]
        ]

mkForeignKeysComposite :: MkPersistSettings -> EntityDef -> ForeignDef -> Q [Dec]
mkForeignKeysComposite mps t fdef = do
   let fieldName f = mkName $ unpack $ recName mps (unHaskellName $ entityHaskell t) (unHaskellName f)
   let fname=fieldName $ foreignConstraintNameHaskell fdef
   let reftablename=mkName $ unpack $ unHaskellName $ foreignRefTableHaskell fdef 
   let tablename=mkName $ unpack $ unHaskellName $ entityHaskell t
   entName <- newName "entname"
   
   let flds = map (\(a,_,_,_) -> VarE (fieldName a)) $ foreignFields fdef
   let xs = ListE $ map (\a -> AppE (VarE 'toPersistValue) ((AppE a (VarE entName)))) flds
   let fn = FunD fname [Clause [VarP entName] (NormalB (AppE (ConE 'Key) (AppE (ConE 'PersistList) xs))) []]
   
   let t2 = ConT ''KeyBackend `AppT` ConT ''SqlBackend `AppT` ConT reftablename
   let sig = SigD fname $ (ArrowT `AppT` (ConT tablename)) `AppT` t2
   return [sig, fn]


-- | produce code similar to the following:
--
-- @
--   instance PersistEntity e => PersistField e where
--      toPersistValue = PersistMap $ zip columNames (map toPersistValue . toPersistFields)
--      fromPersistValue (PersistMap o) = 
--          let columns = HM.fromList x
--          in fromPersistValues $ map (\name ->
--            case HM.lookup name o of
--              Just v ->
--                case fromPersistValue v of
--                  Left e -> error e
--                  Right r -> r
--              Nothing -> error $ "Missing field: " `mappend` unpack name) columnNames 
--      fromPersistValue x = Left $ "Expected PersistMap, received: " ++ show x
--      sqlType _ = SqlString
-- @
persistFieldFromEntity :: MkPersistSettings -> EntityDef -> Q [Dec]
persistFieldFromEntity mps e = do
    ss <- [|SqlString|]
    obj <- [|\ent -> PersistMap $ zip (map pack columnNames) (map toPersistValue $ toPersistFields ent)|]
    fpv <- [|\x -> let columns = HM.fromList x
                    in fromPersistValues $ map
                         (\(nulled, name) ->
                            case HM.lookup name columns of
                                Just v -> case fromPersistValue v of
                                    Left e' -> error $ unpack e'
                                    Right r -> r
                                Nothing -> if nulled then PersistNull
                                    else error $ "Missing field: " `mappend` unpack name)
                         (zip maybeColumns $ map pack columnNames)
          |]

    compose <- [|(<=<)|]
    getPersistMap' <- [|getPersistMap|]
    return
        [ persistFieldInstanceD typ
            [ FunD 'toPersistValue [ Clause [] (NormalB obj) [] ]
            , FunD 'fromPersistValue
                [ Clause [] (NormalB $ InfixE (Just fpv) compose $ Just getPersistMap') []
                ]
            ]
        , persistFieldSqlInstanceD typ
            [ sqlTypeFunD ss
            ]
        ]
    where
      typ = genericDataType mps (entityHaskell e) $ VarT $ mkName "backend"
      entFields = entityFields e
      columnNames  = map (unpack . unHaskellName . fieldHaskell) entFields
      maybeColumns = map ((== Nullable ByMaybeAttr) . nullable . fieldAttrs) entFields

-- | Apply the given list of functions to the same @EntityDef@s.
--
-- This function is useful for cases such as:
--
-- >>> share [mkSave "myDefs", mkPersist sqlSettings] [persistLowerCase|...|]
share :: [[EntityDef] -> Q [Dec]] -> [EntityDef] -> Q [Dec]
share fs x = fmap mconcat $ mapM ($ x) fs

-- | Save the @EntityDef@s passed in under the given name.
mkSave :: String -> [EntityDef] -> Q [Dec]
mkSave name' defs' = do
    let name = mkName name'
    defs <- lift defs'
    return [ SigD name $ ListT `AppT` (ConT ''EntityDef `AppT` ConT ''SqlType)
           , FunD name [Clause [] (NormalB defs) []]
           ]

data Dep = Dep
    { depTarget :: HaskellName
    , depSourceTable :: HaskellName
    , depSourceField :: HaskellName
    , depSourceNull  :: IsNullable
    }

-- | Generate a 'DeleteCascade' instance for the given @EntityDef@s.
mkDeleteCascade :: MkPersistSettings -> [EntityDef] -> Q [Dec]
mkDeleteCascade mps defs = do
    let deps = concatMap getDeps defs
    mapM (go deps) defs
  where
    getDeps :: EntityDef -> [Dep]
    getDeps def =
        concatMap getDeps' $ entityFields $ fixEntityDef def
      where
        getDeps' :: FieldDef -> [Dep]
        getDeps' field@FieldDef {..} =
            case foreignReference field of
                Just f ->
                     return Dep
                        { depTarget = f
                        , depSourceTable = entityHaskell def
                        , depSourceField = fieldHaskell
                        , depSourceNull  = nullable fieldAttrs
                        }
                Nothing -> []
    go :: [Dep] -> EntityDef -> Q Dec
    go allDeps EntityDef{entityHaskell = name} = do
        let deps = filter (\x -> depTarget x == name) allDeps
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
                    [ filt `AppE` ConE filtName
                           `AppE` (left `AppE` val (depSourceNull dep))
                           `AppE` eq
                    ]
              where
                filtName = filterConName' mps (depSourceTable dep) (depSourceField dep)
                val (Nullable ByMaybeAttr) = just `AppE` VarE key
                val _                      =             VarE key



        let stmts :: [Stmt]
            stmts = map mkStmt deps `mappend`
                    [NoBindS $ del `AppE` VarE key]

        let entityT = genericDataType mps name $ VarT $ mkName "backend"

        return $
            InstanceD
            [ ClassP ''PersistQuery [VarT $ mkName "backend"]
            , EqualP (ConT ''PersistEntityBackend `AppT` entityT) (VarT $ mkName "backend")
            ]
            (ConT ''DeleteCascade `AppT` entityT `AppT` VarT (mkName "backend"))
            [ FunD 'deleteCascade
                [Clause [VarP key] (NormalB $ DoE stmts) []]
            ]

mkUniqueKeys :: EntityDef -> Q Dec
mkUniqueKeys def | entitySum def =
    return $ FunD 'persistUniqueKeys [Clause [WildP] (NormalB $ ListE []) []]
mkUniqueKeys def = do
    c <- clause
    return $ FunD 'persistUniqueKeys [c]
  where
    clause = do
        xs <- forM (entityFields def) $ \fd -> do
            let x = fieldHaskell fd
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
sqlTypeFunD st = FunD 'sqlType
                [ Clause [WildP] (NormalB st) [] ]

persistFieldInstanceD :: Type -> [Dec] -> Dec
persistFieldInstanceD typ =
   InstanceD [] (ConT ''PersistField `AppT` typ)

persistFieldSqlInstanceD :: Type -> [Dec] -> Dec
persistFieldSqlInstanceD typ =
   InstanceD [] (ConT ''PersistFieldSql `AppT` typ)

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
                            [] -> Left $ pack "Invalid " ++ pack dt ++ pack ": " ++ s'|]
    return
        [ persistFieldInstanceD (ConT $ mkName s)
            [ FunD 'toPersistValue
                [ Clause [] (NormalB tpv) []
                ]
            , FunD 'fromPersistValue
                [ Clause [] (NormalB $ fpv `AppE` LitE (StringL s)) []
                ]
            ]
        , persistFieldSqlInstanceD (ConT $ mkName s)
            [ sqlTypeFunD ss
            ]
        ]

-- | Automatically creates a valid 'PersistField' instance for any datatype
-- that has valid 'ToJSON' and 'FromJSON' instances. For a datatype @T@ it
-- generates instances similar to these:
-- 
-- @
--    instance PersistField T where
--        toPersistValue = PersistByteString . L.toStrict . encode
--        fromPersistValue = (left T.pack) . eitherDecodeStrict' <=< fromPersistValue
--    instance PersistFieldSql T where
--        sqlType _ = SqlString
-- @
derivePersistFieldJSON :: String -> Q [Dec]
derivePersistFieldJSON s = do
    ss <- [|SqlString|]
    tpv <- [|PersistByteString . B.concat . BL.toChunks . encode|]
    fpv <- [|\dt v -> do
                bs' <- fromPersistValue v
                case eitherDecodeStrict' bs' of
                    Left e -> Left $ pack "JSON decoding error for " ++ pack dt ++ pack ": " ++ pack e ++ pack ". On Input: " ++ decodeUtf8 bs'
                    Right x -> Right x|]
    return
        [ persistFieldInstanceD (ConT $ mkName s)
            [ FunD 'toPersistValue
                [ Clause [] (NormalB tpv) []
                ]
            , FunD 'fromPersistValue
                [ Clause [] (NormalB $ fpv `AppE` LitE (StringL s)) []
                ]
            ]
        , persistFieldSqlInstanceD (ConT $ mkName s)
            [ sqlTypeFunD ss
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
    typ = ConT ''Migration
    body :: Q Exp
    body =
        case defs of
            [] -> [|return ()|]
            _  -> do
              defsName <- newName "defs"
              defsStmt <- do
                defs' <- mapM lift defs
                let defsExp = ListE defs'
                return $ LetS [ValD (VarP defsName) (NormalB defsExp) []]
              stmts <- mapM (toStmt $ VarE defsName) defs
              return (DoE $ defsStmt : stmts)
    toStmt :: Exp -> EntityDef -> Q Stmt
    toStmt defsExp ed = do
        u <- lift ed
        m <- [|migrate|]
        return $ NoBindS $ m `AppE` defsExp `AppE` u

instance Lift EntityDef where
    lift (EntityDef a b c d e f g h i j k) =
        [|EntityDef
            $(lift a)
            $(lift b)
            $(lift c)
            $(liftTs d)
            $(lift e)
            $(lift f)
            $(lift g)
            $(lift h)
            $(liftTs i)
            $(liftMap j)
            $(lift k)
            |]
instance Lift FieldDef where
    lift (FieldDef a b c d e f g) = [|FieldDef a b c $(lift' d) $(liftTs e) f g|]
instance Lift UniqueDef where
    lift (UniqueDef a b c d) = [|UniqueDef $(lift a) $(lift b) $(lift c) $(liftTs d)|]
instance Lift PrimaryDef where
    lift (PrimaryDef a b) = [|PrimaryDef $(lift a) $(liftTs b)|]
instance Lift ForeignDef where
    lift (ForeignDef a b c d e f) = [|ForeignDef $(lift a) $(lift b) $(lift c) $(lift d) $(lift e) $(liftTs f)|]

-- | A hack to avoid orphans.
class Lift' a where
    lift' :: a -> Q Exp
instance Lift' SqlType where
    lift' = lift
instance Lift' a => Lift' (Maybe a) where
    lift' Nothing = [|Nothing|]
    lift' (Just a) = [|Just $(lift' a)|]
instance Lift' EntityDef where
    lift' = lift
instance Lift' () where
    lift' () = [|()|]
instance Lift' SqlTypeExp where
    lift' = lift

packPTH :: String -> Text
packPTH = pack
#if !MIN_VERSION_text(0, 11, 2)
{-# NOINLINE packPTH #-}
#endif

liftT :: Text -> Q Exp
liftT t = [|packPTH $(lift (unpack t))|]

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

instance Lift SqlType where
    lift SqlString = [|SqlString|]
    lift SqlInt32 = [|SqlInt32|]
    lift SqlInt64 = [|SqlInt64|]
    lift SqlReal = [|SqlReal|]
    lift (SqlNumeric x y) =
        [|SqlNumeric (fromInteger x') (fromInteger y')|]
      where
        x' = fromIntegral x :: Integer
        y' = fromIntegral y :: Integer
    lift SqlBool = [|SqlBool|]
    lift SqlDay = [|SqlDay|]
    lift SqlTime = [|SqlTime|]
    lift SqlDayTime = [|SqlDayTime|]
    lift SqlBlob = [|SqlBlob|]
    lift (SqlOther a) = [|SqlOther $(liftT a)|]

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
    name = filterConName mps et cd
    maybeTyp =
        if nullable (fieldAttrs cd) == Nullable ByMaybeAttr
            then ConT ''Maybe `AppT` typ
            else typ
    typ =
        case foreignReference cd of
            Just ft ->
                 ConT ''KeyBackend
                    `AppT` (if mpsGeneric mps
                                then VarT $ mkName "backend"
                                else mpsBackend mps)
                    `AppT` genericDataType mps ft (VarT $ mkName "backend")
            Nothing -> ftToType $ fieldType cd

filterConName :: MkPersistSettings
              -> EntityDef
              -> FieldDef
              -> Name
filterConName mps entity field = filterConName' mps (entityHaskell entity) (fieldHaskell field)

filterConName' :: MkPersistSettings
               -> HaskellName -- ^ table
               -> HaskellName -- ^ field
               -> Name
filterConName' mps entity field = mkName $ unpack $ concat
    [ if mpsPrefixFields mps || field == HaskellName "Id"
        then unHaskellName entity
        else ""
    , upperFirst $ unHaskellName field
    ]

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
        typ = genericDataType mps (entityHaskell def) $ VarT $ mkName "backend"
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
            (if nullable (fieldAttrs f) == Nullable ByMaybeAttr then dotColonQE else dotColonE)
            (Just $ AppE packE $ LitE $ StringL $ unpack $ unHaskellName $ fieldHaskell f)
    case mpsEntityJSON mps of
        Nothing -> return [toJSONI, fromJSONI]
        Just entityJSON -> do
            entityJSONIs <- [d|
                instance ToJSON (Entity $(pure typ)) where
                    toJSON = $(varE (entityToJSON entityJSON))
                instance FromJSON (Entity $(pure typ)) where
                    parseJSON = $(varE (entityFromJSON entityJSON))
                |]
            return $ toJSONI : fromJSONI : entityJSONIs
