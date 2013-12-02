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
    , EntityJSON, entityToJSON, entityFromJSON
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
      -- * Internal
    , packPTH
    , lensPTH
    ) where

import Prelude hiding ((++), take, concat, splitAt)
import qualified Prelude as P 
import Database.Persist
import Database.Persist.Sql (Migration, SqlPersistT, migrate, SqlBackend, PersistFieldSql)
import Database.Persist.Quasi
import Language.Haskell.TH.Lib (varE)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (mkName)
import qualified Language.Haskell.TH.Syntax
import Data.Char (toLower, toUpper)
import Control.Monad (forM, (<=<), mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import qualified System.IO as SIO
import Data.Text (pack, Text, append, unpack, concat, uncons, cons)
import qualified Data.Text.IO as TIO
import Data.List (foldl', find)
import Data.Maybe (isJust)
import Data.Monoid (mappend, mconcat)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Aeson
    ( ToJSON (toJSON), FromJSON (parseJSON), (.=), object
    , Value (Object), (.:), (.:?)
    )
import Control.Applicative (pure, (<*>))
import Control.Monad.Logger (MonadLogger)
import Database.Persist.Sql (sqlType)

{-
readMay :: Read a => String -> Maybe a
readMay s =
    case reads s of
        (x, _):_ -> Just x
        [] -> Nothing
-}

mkName :: Text -> Name
mkName = Language.Haskell.TH.Syntax.mkName . unpack

-- | Converts a quasi-quoted syntax into a list of entity definitions, to be
-- used as input to the template haskell generation code (mkPersist).
persistWith :: PersistSettings -> QuasiQuoter
persistWith ps = QuasiQuoter
    { quoteExp = parseSqlType ps . pack
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
    parseSqlType ps s

parseSqlType :: PersistSettings -> Text -> Q Exp
parseSqlType ps s =
    lift $ map (sqlEntityType defsOrig) defsOrig
  where
    defsOrig = parse ps s

sqlEntityType :: [EntityDef ()] -> EntityDef () -> EntityDef SqlTypeExp
sqlEntityType allEntities ent = ent
    { entityFields = map sqlFieldDef $ entityFields ent }
  where
    sqlFieldDef :: FieldDef () -> FieldDef SqlTypeExp
    sqlFieldDef field = do
        field
            { fieldSqlType = final
            , fieldEmbedded = mEmbedded (fieldType field)
            }
      where
        -- In the case of embedding, there won't be any datatype created yet.
        -- We just use SqlString, as the data will be serialized to JSON.
        final
            | isJust (mEmbedded (fieldType field)) = SqlString'
            | isReference = SqlInt64'
            | otherwise =
                case fieldType field of
                    -- In the case of lists, we always serialize to a string
                    -- value (via JSON).
                    --
                    -- Normally, this would be determined automatically by
                    -- SqlTypeExp. However, there's one corner case: if there's
                    -- a list of entity IDs, the datatype for the ID has not
                    -- yet been created, so the compiler will fail. This extra
                    -- clause works around this limitation.
                    FTList _ -> SqlString'
                    _ -> SqlTypeExp st

        mEmbedded (FTTypeCon Just{} _) = Nothing
        mEmbedded (FTTypeCon Nothing n) = let name = HaskellName n in
            find ((name ==) . entityHaskell) allEntities
        mEmbedded (FTList x) = mEmbedded x
        mEmbedded (FTApp x y) = maybe (mEmbedded y) Just (mEmbedded x)

        isReference =
            case stripId $ fieldType field of
                Just{} -> True
                Nothing -> False

        typ = ftToType $ fieldType field
        mtyp = (ConT ''Maybe `AppT` typ)
        typedNothing = SigE (ConE 'Nothing) mtyp
        st = VarE 'sqlType `AppE` typedNothing

data SqlTypeExp = SqlTypeExp Exp
                | SqlString'
                | SqlInt64'
instance Lift SqlTypeExp where
    lift (SqlTypeExp e) = return e
    lift SqlString'     = [|SqlString|]
    lift SqlInt64'      = [|SqlInt64|]

-- | Create data types and appropriate 'PersistEntity' instances for the given
-- 'EntityDef's. Works well with the persist quasi-quoter.
mkPersist :: MkPersistSettings -> [EntityDef SqlType] -> Q [Dec]
mkPersist mps ents' = fmap mconcat $ flip mapM ents $ \ent -> do
    x <- persistFieldFromEntity mps ent
    y <- mkEntity mps ent
    z <- mkJSON mps ent
    return $ mconcat [x, y, z]
  where
    ents = map fixEntityDef ents'

-- | Implement special preprocessing on EntityDef as necessary for 'mkPersist'.
-- For example, strip out any fields marked as MigrationOnly.
fixEntityDef :: EntityDef a -> EntityDef a
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
    , mpsGeneric = True -- FIXME switch default to False in the future
    , mpsPrefixFields = True
    , mpsEntityJSON = Just EntityJSON
        { entityToJSON = 'keyValueEntityToJSON
        , entityFromJSON = 'keyValueEntityFromJSON
        }
    }

-- | Use the 'SqlPersist' backend.
sqlSettings :: MkPersistSettings
sqlSettings = mkPersistSettings $ ConT ''SqlBackend

-- | Same as 'sqlSettings', but set 'mpsGeneric' to @False@.
--
-- Since 1.1.1
sqlOnlySettings :: MkPersistSettings
sqlOnlySettings = sqlSettings { mpsGeneric = False }

recName :: MkPersistSettings -> Text -> Text -> Text
recName mps dt f
  | mpsPrefixFields mps = lowerFirst dt ++ upperFirst f
  | otherwise           = lowerFirst f

mapFirst :: (Char -> Char) -> Text -> Text
mapFirst f t =
    case uncons t of
        Just (a, b) -> cons (f a) b
        Nothing -> t

lowerFirst :: Text -> Text
lowerFirst = mapFirst toLower

upperFirst :: Text -> Text
upperFirst = mapFirst toUpper

dataTypeDec :: MkPersistSettings -> EntityDef a -> Dec
dataTypeDec mps t =
    DataD [] nameFinal paramsFinal constrs
    $ map mkName $ entityDerives t
  where
    mkCol x FieldDef {..} =
        (mkName $ recName mps x $ unHaskellName fieldHaskell,
         if fieldStrict then IsStrict else NotStrict,
         pairToType mps (fieldType, nullable fieldAttrs)
        )
    (nameFinal, paramsFinal)
        | mpsGeneric mps = (genericTypeName t, [PlainTV backendName])
        | otherwise = (name, [])
    name = entNameName t
    cols = map (mkCol $ entName t) $ entityFields t

    constrs
        | entitySum t = map sumCon $ entityFields t
        | otherwise = [RecC name cols]

    sumCon fd = NormalC
        (sumConstrName mps t fd)
        [(NotStrict, pairToType mps (fieldType fd, NotNullable))]

sumConstrName :: MkPersistSettings -> EntityDef a -> FieldDef b -> Name
sumConstrName mps t FieldDef {..} = mkName $ concat
    [ if mpsPrefixFields mps then entName t else ""
    , upperFirst $ unHaskellName fieldHaskell
    , "Sum"
    ]

entityUpdates :: EntityDef a -> [(HaskellName, FieldType, IsNullable, PersistUpdate)]
entityUpdates =
    concatMap go . entityFields
  where
    go FieldDef {..} = map (\a -> (fieldHaskell, fieldType, nullable fieldAttrs, a)) [minBound..maxBound]

uniqueTypeDec :: MkPersistSettings -> EntityDef a -> Dec
uniqueTypeDec mps t =
    DataInstD [] ''Unique
        [entityType mps t]
            (map (mkUnique mps t) $ entityUniques t)
            []

mkUnique :: MkPersistSettings -> EntityDef a -> UniqueDef -> Con
mkUnique mps t (UniqueDef (HaskellName constr) _ fields attrs) =
    NormalC (mkName constr) types
  where
    types = map (go . flip lookup3 (entityFields t))
          $ map (unHaskellName . fst) fields

    force = "!force" `elem` attrs

    go :: (FieldType, IsNullable) -> (Strict, Type)
    go (_, Nullable _) | not force = error nullErrMsg
    go (ft, y) = (NotStrict, pairToType mps (ft, y))

    lookup3 :: Text -> [FieldDef a] -> (FieldType, IsNullable)
    lookup3 s [] =
        error $ unpack $ "Column not found: " ++ s ++ " in unique " ++ constr
    lookup3 x (FieldDef {..}:rest)
        | x == unHaskellName fieldHaskell = (fieldType, nullable fieldAttrs)
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
           -> (FieldType, IsNullable)
           -> Type
pairToType mps (s, Nullable ByMaybeAttr) =
  ConT ''Maybe `AppT` idType mps s
pairToType mps (s, _) = idType mps s

backendName :: Name
backendName = mkName "backend"

backendType :: Type
backendType = VarT backendName

backendDataType :: MkPersistSettings -> Type
backendDataType mps
    | mpsGeneric mps = backendType
    | otherwise = mpsBackend mps

entityTypeBackend :: MkPersistSettings
            -> EntityDef a
            -> Type -- ^ backend
            -> Type
entityTypeBackend mps t = entityTypeFromName mps (entName t)

entityType :: MkPersistSettings -> EntityDef a -> Type
entityType mps t = entityTypeBackend mps t backendType

entityTypeFromName :: MkPersistSettings
                -> Text -- ^ entity name
                -> Type -- ^ backend
                -> Type
entityTypeFromName mps eName backend
    | mpsGeneric mps = genericTypeFromName backend eName
    | otherwise = ConT $ mkName eName

genericTypeName :: EntityDef a -> Name
genericTypeName t = genericTypeNameFromName $ entName t

genericTypeFromName :: Type -> Text -> Type
genericTypeFromName backend eName = ConT (genericTypeNameFromName eName) `AppT` backend

genericTypeNameFromName :: Text -> Name
genericTypeNameFromName eName = mkName $ eName `mappend` "Generic"

{-
genericType :: EntityDef a -> Type
genericType t = genericTypeFromName $ entName t
-}



idType :: MkPersistSettings -> FieldType -> Type
idType mps typ =
    case stripId typ of
        Just typ' ->
            ConT ''KeyBackend `AppT` backend'
            `AppT` entityTypeFromName mps typ' backendType
        Nothing -> ftToType typ
  where
    backend'
        | mpsGeneric mps = backendType
        | otherwise = mpsBackend mps

degen :: [Clause] -> [Clause]
degen [] =
    let err = VarE 'error `AppE` LitE (StringL
                "Degenerate case, should never happen")
     in [Clause [WildP] (NormalB err) []]
degen x = x

mkToPersistFields :: MkPersistSettings -> EntityDef a -> Q Dec
mkToPersistFields mps t@EntityDef { entitySum = isSum, entityFields = fields } = do
    clauses <-
        if isSum
            then sequence $ zipWith goSum fields [1..]
            else fmap return go
    return $ FunD 'toPersistFields clauses
  where
    go :: Q Clause
    go = do
        xs <- sequence $ replicate fieldCount $ newName "x"
        let pat = ConP (entNameName t) $ map VarP xs
        sp <- [|SomePersistField|]
        let bod = ListE $ map (AppE sp . VarE) xs
        return $ Clause [pat] (NormalB bod) []

    fieldCount = length fields

    goSum :: FieldDef a -> Int -> Q Clause
    goSum fd idx = do
        let name = sumConstrName mps t fd
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
                [RecP (mkName $ unHaskellName constr) []]
                (NormalB names')
                []

mkUniqueToValues :: [UniqueDef] -> Q Dec
mkUniqueToValues pairs = do
    pairs' <- mapM go pairs
    return $ FunD 'persistUniqueToValues $ degen pairs'
  where
    go :: UniqueDef -> Q Clause
    go (UniqueDef constr _ names _) = do
        xs <- mapM (const $ newName "x") names
        let pat = ConP (mkName $ unHaskellName constr) $ map VarP xs
        tpv <- [|toPersistValue|]
        let bod = ListE $ map (AppE tpv . VarE) xs
        return $ Clause [pat] (NormalB bod) []

{-
mkToUpdate :: String -> [(String, PersistUpdate)] -> Q Dec
mkToUpdate name pairs = do
    pairs' <- mapM go pairs
    return $ FunD (Language.Haskell.TH.Syntax.mkName name) $ degen pairs'
  where
    go (constr, pu) = do
        pu' <- lift pu
        return $ Clause [RecP (Language.Haskell.TH.Syntax.mkName constr) []] (NormalB pu') []

mkToFieldName :: String -> [(String, String)] -> Dec
mkToFieldName func pairs =
        FunD (Language.Haskell.TH.Syntax.mkName func) $ degen $ map go pairs
  where
    go (constr, name) =
        Clause [RecP (Language.Haskell.TH.Syntax.mkName constr) []] (NormalB $ LitE $ StringL name) []

mkToValue :: String -> [String] -> Dec
mkToValue func = FunD (Language.Haskell.TH.Syntax.mkName func) . degen . map go
  where
    go constr =
        let x = mkName "x"
         in Clause [ConP (Language.Haskell.TH.Syntax.mkName constr) [VarP x]]
                   (NormalB $ VarE 'toPersistValue `AppE` VarE x)
                   []
-}

isNotNull :: PersistValue -> Bool
isNotNull PersistNull = False
isNotNull _ = True

--
-- data KeyBackend backend (ContactGeneric backend) = ContactKey !Int64
mkAssociatedKey :: MkPersistSettings -> EntityDef a -> Type -> Q [Dec]
mkAssociatedKey mps t backendKeyType = do
  let keyName    = mkName $ entName t `mappend` "Key"
  let recordType = entityType mps t
  insideKeyName <- newName "x"
  fpv <- [| \x -> case fromPersistValue x of
              Left e' -> error $ unpack e'
              Right r -> $(return $ ConE keyName) r
        |]
  tpv <- [| toPersistValue $(return $ VarE insideKeyName) |]
  return
        [
          DataInstD [] ''Key [ recordType ]
            [ NormalC keyName [ (IsStrict, backendKeyType) ] ] []
        ,  FunD 'persistValueToPersistKey [ Clause [] (NormalB fpv) []]
        ,  FunD 'persistKeyToPersistValue [ Clause
             [ConP keyName [VarP insideKeyName]]
             (NormalB tpv)
             []
           ]
        ]

entId :: EntityDef a -> Text
entId = flip mappend "Id" . entName

entName :: EntityDef a -> Text
entName = unHaskellName . entityHaskell

entNameName :: EntityDef a -> Name
entNameName = mkName . entName

mkFromPersistValues :: MkPersistSettings -> EntityDef a -> Q [Clause]
mkFromPersistValues mps t@(EntityDef { entitySum = False }) = do
    nothing <- [|Left $(liftT $ "Invalid fromPersistValues input. Entity: " `mappend` entName t)|]
    let cons' = ConE $ entNameName t
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
    go ap' x y = InfixE (Just x) ap' (Just y)
    mkPersistValue fieldName = [|\persistValue ->
            case fromPersistValue persistValue of
                   Right r  -> Right r
                   Left err -> Left $
                     "field " `mappend` $(liftT fieldName) `mappend` ": " `mappend` err
          |]

mkFromPersistValues mps t@(EntityDef { entitySum = True }) = do
    nothing <- [|Left $(liftT $ "Invalid fromPersistValues input: sum type with all nulls. Entity: " `mappend` entName t)|]
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

mkLensClauses :: MkPersistSettings -> EntityDef a -> Q [Clause]
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
            [ConP (mkName $ entId t) []]
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
        fieldName = mkName $ recName mps (entName t) (unHaskellName $ fieldHaskell f)
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

mkEntity :: MkPersistSettings -> EntityDef SqlType -> Q [Dec]
mkEntity mps t = do
    t' <- lift t
    tpf <- mkToPersistFields mps t
    fpv <- mkFromPersistValues mps t

    let backendKeyType = ConT ''BackendKey `AppT` mpsBackend mps
            -- if mpsGeneric mps then backendType else mpsBackend mps
    key <- mkAssociatedKey mps t backendKeyType
    utv <- mkUniqueToValues $ entityUniques t
    puk <- mkUniqueKeys t
    fkc <- mapM (mkForeignKeysComposite mps t) $ entityForeigns t
    
    fields <- mapM (mkField mps t) $ FieldDef
        { fieldHaskell = HaskellName "Id"
        , fieldDB = entityID t
        , fieldType = FTTypeCon Nothing (entId t)
        , fieldSqlType = SqlInt64
        , fieldEmbedded = Nothing
        , fieldAttrs = []
        , fieldStrict = True
        }
        : entityFields t
    toFieldNames <- mkToFieldNames $ entityUniques t

    let addSyn -- FIXME maybe remove this
            | mpsGeneric mps = (:) $
                TySynD (entNameName t) [] $
                    entityTypeBackend mps t $ mpsBackend mps
            | otherwise = id

    lensClauses <- mkLensClauses mps t

    return $ addSyn $
       dataTypeDec mps t : mconcat fkc `mappend`
      ([ TySynD idName [] $
            ConT ''Key `AppT` ConT (entNameName t)
      , InstanceD [] clazz $
        [ uniqueTypeDec mps t
        , FunD 'entityDef [Clause [WildP] (NormalB t') []]
        , tpf
        ]
        `mappend` key
        `mappend`
        [ FunD 'fromPersistValues fpv
        , toFieldNames
        , utv
        , puk
        , DataInstD
            []
            ''EntityField
            [ genericDataType
            , VarT $ mkName "typ"
            ]
            (map fst fields)
            []
        , FunD 'persistFieldDef (map snd fields)
        , TySynInstD
            ''EntityBackend
#if MIN_VERSION_template_haskell(2,9,0)
            (TySynEqn
               [genericDataType]
               (backendDataType mps))
#else
            [genericDataType]
            (backendDataType mps)
#endif
        , FunD 'persistIdField [Clause [] (NormalB $ ConE idName) []]
        , FunD 'fieldLens lensClauses
        ]
      ])
  where
    genericDataType = entityType mps t
    idName          = mkName $ entId t
    clazz           = ConT ''PersistEntity `AppT` genericDataType

mkForeignKeysComposite :: MkPersistSettings -> EntityDef a -> ForeignDef -> Q [Dec]
mkForeignKeysComposite mps t fdef = do
   let fieldName f = mkName $ recName mps (entName t) (unHaskellName f)
   let fname        = fieldName $ foreignConstraintNameHaskell fdef
   let reftablename = mkName $ unHaskellName $ foreignRefTableHaskell fdef
   let tablename    = mkName $ entName t
   eName <- newName "entname"
   
   let flds = map (\(a,_,_,_) -> VarE (fieldName a)) $ foreignFields fdef
   let xs = ListE $ map (\a -> AppE (VarE 'toPersistValue) ((AppE a (VarE eName)))) flds
   let fn = FunD fname [Clause [VarP eName] (NormalB (AppE (VarE 'persistValueToPersistKey) (AppE (ConE 'PersistList) xs))) []]
   
   let keybackend = ConT ''Key `AppT` ConT reftablename
   let sig = SigD fname $ (ArrowT `AppT` (ConT tablename)) `AppT` keybackend
   return [sig, fn]


-- | produce code similar to the following:
--
-- instance PersistEntity e => PersistField e where
--    toPersistValue = PersistMap $ zip columNames (map toPersistValue . toPersistFields)
--    fromPersistValue (PersistMap o) = 
--        let columns = HM.fromList x
--        in fromPersistValues $ map (\name ->
--          case HM.lookup name o of
--            Just v ->
--              case fromPersistValue v of
--                Left e -> error e
--                Right r -> r
--            Nothing -> error $ "Missing field: " `mappend` unpack name) columnNames 
--    fromPersistValue x = Left $ "Expected PersistMap, received: " ++ show x
--    sqlType _ = SqlString
persistFieldFromEntity :: MkPersistSettings -> EntityDef a -> Q [Dec]
persistFieldFromEntity mps e = do
    ss <- [|SqlString|]
    let columnNames = map (unpack . unHaskellName . fieldHaskell) (entityFields e)
    obj <- [|\ent -> PersistMap $ zip (map pack columnNames) (map toPersistValue $ toPersistFields ent)|]
    fpv <- [|\x -> let columns = HM.fromList x
                   in fromPersistValues $ map (\name -> 
                          case HM.lookup name columns of
                              Just v -> 
                                  case fromPersistValue v of
                                      Left e' -> error $ unpack e'
                                      Right r -> r
                              Nothing -> error $ "Missing field: " `mappend` unpack name) (map pack columnNames)|]
    let typ = entityType mps e

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

-- | Apply the given list of functions to the same @EntityDef@s.
--
-- This function is useful for cases such as:
--
-- >>> share [mkSave "myDefs", mkPersist sqlSettings] [persistLowerCase|...|]
share :: [[EntityDef a] -> Q [Dec]] -> [EntityDef a] -> Q [Dec]
share fs x = fmap mconcat $ mapM ($ x) fs

-- | Save the @EntityDef@s passed in under the given name.
mkSave :: Text -> [EntityDef SqlType] -> Q [Dec]
mkSave name' defs' = do
    let name = mkName name'
    defs <- lift defs'
    return [ SigD name $ ListT `AppT` (ConT ''EntityDef `AppT` ConT ''SqlType)
           , FunD name [Clause [] (NormalB defs) []]
           ]

data Dep = Dep
    { depTarget :: Text
    , depSourceTable :: HaskellName
    , depSourceField :: HaskellName
    , depSourceNull  :: IsNullable
    }

-- | Generate a 'DeleteCascade' instance for the given @EntityDef@s.
mkDeleteCascade :: MkPersistSettings -> [EntityDef a] -> Q [Dec]
mkDeleteCascade mps defs = do
    let deps = concatMap getDeps defs
    mapM (go deps) defs
  where
    getDeps :: EntityDef a -> [Dep]
    getDeps def =
        concatMap getDeps' $ entityFields $ fixEntityDef def
      where
        getDeps' :: FieldDef a -> [Dep]
        getDeps' FieldDef {..} =
            case stripId fieldType of
                Just f ->
                     return Dep
                        { depTarget = f
                        , depSourceTable = entityHaskell def
                        , depSourceField = fieldHaskell
                        , depSourceNull  = nullable fieldAttrs
                        }
                Nothing -> []
    go :: [Dep] -> EntityDef a -> Q Dec
    go allDeps t@EntityDef{} = do
        let deps = filter (\x -> depTarget x == entName t) allDeps
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

        let entityT = entityType mps t
        let monad = VarT $ mkName "m"

        return $
            InstanceD
            [ ClassP ''PersistQuery [monad]
            , EqualP (ConT ''EntityBackend `AppT` entityT) (ConT ''MonadBackend `AppT` monad)
            ]
            (ConT ''DeleteCascade `AppT` entityT `AppT` monad)
            [ FunD 'deleteCascade
                [Clause [VarP key] (NormalB $ DoE stmts) []]
            ]

mkUniqueKeys :: EntityDef a -> Q Dec
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
                (mkName $ entName def)
                (map (VarP . snd) xs)
        return $ Clause [pat] (NormalB $ ListE pcs) []

    go :: [(HaskellName, Name)] -> UniqueDef -> Exp
    go xs (UniqueDef name _ cols _) =
        foldl' (go' xs) (ConE (mkName $ unHaskellName name)) (map fst cols)

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
    let fieldName = Language.Haskell.TH.Syntax.mkName s
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
        [ persistFieldInstanceD (ConT fieldName)
            [ FunD 'toPersistValue
                [ Clause [] (NormalB tpv) []
                ]
            , FunD 'fromPersistValue
                [ Clause [] (NormalB $ fpv `AppE` LitE (StringL s)) []
                ]
            ]
        , persistFieldSqlInstanceD (ConT fieldName)
            [ sqlTypeFunD ss
            ]
        ]

-- | Creates a single function to perform all migrations for the entities
-- defined here. One thing to be aware of is dependencies: if you have entities
-- with foreign references, make sure to place those definitions after the
-- entities they reference.
mkMigrate :: Lift' a => Text -> [EntityDef a] -> Q [Dec]
mkMigrate fun allDefs = do
    body' <- body
    return
        [ SigD (mkName fun) typ
        , FunD (mkName fun) [Clause [] (NormalB body') []]
        ]
  where
    defs = filter isMigrated allDefs
    isMigrated def = not $ "no-migrate" `elem` entityAttrs def
    monadName = mkName "m"
    monad = VarT monadName
    typ = ForallT [PlainTV monadName]
            [ ClassP ''MonadBaseControl [ConT ''IO, monad]
            , ClassP ''MonadIO [monad]
            , ClassP ''MonadLogger [monad]
            ]
            $ ConT ''Migration `AppT` (ConT ''SqlPersistT `AppT` monad)
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
    toStmt :: Lift' a => Exp -> EntityDef a -> Q Stmt
    toStmt defsExp ed = do
        u <- lift ed
        m <- [|migrate|]
        return $ NoBindS $ m `AppE` defsExp `AppE` u

instance Lift' a => Lift (EntityDef a) where
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
instance Lift' a => Lift (FieldDef a) where
    lift (FieldDef a b c d e f g) = [|FieldDef a b c $(lift' d) $(liftTs e) f $(lift' g)|]
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
instance Lift' a => Lift' (EntityDef a) where
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
    lift SqlDayTimeZoned = [|SqlDayTimeZoned|]
    lift SqlBlob = [|SqlBlob|]
    lift (SqlOther a) = [|SqlOther $(liftT a)|]

-- Ent
--   fieldName FieldType
--
-- forall . typ ~ FieldType => EntFieldName
--
-- EntFieldName = FieldDef ....
mkField :: MkPersistSettings -> EntityDef a -> FieldDef SqlType -> Q (Con, Clause)
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
        case stripId $ fieldType cd of
            Just ft ->
                 ConT ''Key
                    `AppT` entityTypeFromName mps ft backendType
            Nothing -> ftToType $ fieldType cd

filterConName :: MkPersistSettings
              -> EntityDef sqlType1
              -> FieldDef sqlType2
              -> Name
filterConName mps entity field = filterConName' mps (entityHaskell entity) (fieldHaskell field)

filterConName' :: MkPersistSettings
               -> HaskellName -- ^ table
               -> HaskellName -- ^ field
               -> Name
filterConName' mps entity field = mkName $ concat
    [ if mpsPrefixFields mps || field == HaskellName "Id"
        then unHaskellName entity
        else ""
    , upperFirst $ unHaskellName field
    ]

ftToType :: FieldType -> Type
ftToType (FTTypeCon Nothing t) = ConT $ mkName t
ftToType (FTTypeCon (Just m) t) = ConT $ mkName $ concat [m, ".", t]
ftToType (FTApp x y) = ftToType x `AppT` ftToType y
ftToType (FTList x) = ListT `AppT` ftToType x

infixr 5 ++
(++) :: Text -> Text -> Text
(++) = append

mkJSON :: MkPersistSettings -> EntityDef a -> Q [Dec]
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

    let conName = mkName $ entName def
        typ = entityType mps def
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
            let pureTyp = pure typ
            entityJSONIs <- [d|
                instance ToJSON (Entity $pureTyp) where
                    toJSON = $(varE (entityToJSON entityJSON))
                instance FromJSON (Entity $pureTyp) where
                    parseJSON = $(varE (entityFromJSON entityJSON))
                |]
            return $ toJSONI : fromJSONI : entityJSONIs
