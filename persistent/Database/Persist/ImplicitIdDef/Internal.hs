{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

-- | WARNING: This is an @Internal@ module. As such, breaking changes to the API
-- of this module will not have a corresponding major version bump.
--
-- Please depend on "Database.Persist.ImplicitIdDef" instead. If you can't use
-- that module, please file an issue on GitHub with your desired use case.
--
-- @since 2.13.0.0
module Database.Persist.ImplicitIdDef.Internal where

import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH (Type)
import LiftType
import Type.Reflection
import Data.Typeable (eqT)
import Data.Foldable (asum)

import Database.Persist.Class.PersistField (PersistField)
import Database.Persist.Names
import Database.Persist.Sql.Class
import Database.Persist.Types

-- | A specification for how the implied ID columns are created.
--
-- By default, @persistent@ will give each table a default column named @id@
-- (customizable by 'PersistSettings'), and the column type will be whatever
-- you'd expect from @'BackendKey' yourBackendType@. For The 'SqlBackend' type,
-- this is an auto incrementing integer primary key.
--
-- You might want to give a different example. A common use case in postgresql
-- is to use the UUID type, and automatically generate them using a SQL
-- function.
--
-- Previously, you'd need to add a custom @Id@ annotation for each model.
--
-- > User
-- >     Id   UUID default="uuid_generate_v1mc()"
-- >     name Text
-- >
-- > Dog
-- >     Id   UUID default="uuid_generate_v1mc()"
-- >     name Text
-- >     user UserId
--
-- Now, you can simply create an 'ImplicitIdDef' that corresponds to this
-- declaration.
--
-- @
-- newtype UUID = UUID 'ByteString'
--
-- instance 'PersistField' UUID where
--     'toPersistValue' (UUID bs) =
--         'PersistLiteral_' 'Escaped' bs
--     'fromPersistValue' pv =
--         case pv of
--             PersistLiteral_ Escaped bs ->
--                 Right (UUID bs)
--             _ ->
--                 Left "nope"
--
-- instance 'PersistFieldSql' UUID where
--     'sqlType' _ = 'SqlOther' "UUID"
-- @
--
-- With this instance at the ready, we can now create our implicit definition:
--
-- @
-- uuidDef :: ImplicitIdDef
-- uuidDef = mkImplicitIdDef \@UUID "uuid_generate_v1mc()"
-- @
--
-- And we can use 'setImplicitIdDef' to use this with the 'MkPersistSettings'
-- for our block.
--
-- @
-- mkPersist (setImplicitIdDef uuidDef sqlSettings) [persistLowerCase| ... |]
-- @
--
-- TODO: either explain interaction with mkMigrate or fix it. see issue #1249
-- for more details.
--
-- @since 2.13.0.0
data ImplicitIdDef = ImplicitIdDef
    { iidFieldType :: EntityNameHS -> FieldType
    -- ^ The field type. Accepts the 'EntityNameHS' if you want to refer to it.
    -- By default, @Id@ is appended to the end of the Haskell name.
    --
    -- @since 2.13.0.0
    , iidFieldSqlType :: SqlType
    -- ^ The 'SqlType' for the default column. By default, this is 'SqlInt64' to
    -- correspond with an autoincrementing integer primary key.
    --
    -- @since 2.13.0.0
    , iidType :: Bool -> Type -> Type
    -- ^ The Bool argument is whether or not the 'MkPersistBackend' type has the
    -- 'mpsGeneric' field set.
    --
    -- The 'Type' is the 'mpsBackend' value.
    --
    -- The default uses @'BackendKey' 'SqlBackend'@ (or a generic equivalent).
    --
    -- @since 2.13.0.0
    , iidDefault :: Maybe Text
    -- ^ The default expression for the field. Note that setting this to
    -- 'Nothing' is unsafe. see
    -- https://github.com/yesodweb/persistent/issues/1247 for more information.
    --
    -- With some cases - like the Postgresql @SERIAL@ type - this is safe, since
    -- there's an implied default.
    --
    -- @since 2.13.0.0
    , iidMaxLen :: Maybe Integer
    -- ^ Specify the maximum length for a key column. This is necessary for
    -- @VARCHAR@ columns, like @UUID@ in MySQL. MySQL will throw a runtime error
    -- if a text or binary column is used in an index without a length
    -- specification.
    --
    -- @since 2.13.0.0
    }

-- | Create an 'ImplicitIdDef' based on the 'Typeable' and 'PersistFieldSql'
-- constraints in scope.
--
-- This function uses the @TypeApplications@ syntax.  Let's look at an example
-- that works with Postgres UUIDs.
--
-- > newtype UUID = UUID Text
-- >     deriving newtype PersistField
-- >
-- > instance PersistFieldSql UUID where
-- >     sqlType _ = SqlOther "UUID"
-- >
-- > idDef :: ImplicitIdDef
-- > idDef = mkImplicitIdDefTypeable @UUID "uuid_generate_v1mc()"
--
-- This 'ImplicitIdDef' will generate default UUID columns, and the database
-- will call the @uuid_generate_v1mc()@  function to generate the value for new
-- rows being inserted.
--
-- If the type @t@ is 'Text' or 'String' then a @max_len@ attribute of 200 is
-- set. To customize this, use 'setImplicitIdDefMaxLen'.
--
-- @since 2.13.0.0
mkImplicitIdDef
    :: forall t. (Typeable t, PersistFieldSql t)
    => Text
    -- ^ The default expression to use for columns. Should be valid SQL in the
    -- language you're using.
    -> ImplicitIdDef
mkImplicitIdDef def =
    ImplicitIdDef
        { iidFieldType = \_ ->
            fieldTypeFromTypeable @t
        , iidFieldSqlType =
            sqlType (Proxy @t)
        , iidType =
            \_ _ -> liftType @t
        , iidDefault =
            Just def
        , iidMaxLen =
            -- this follows a special casing behavior that @persistent@ has done
            -- for a while now. this keeps folks code from breaking and probably
            -- is mostly what people want.
            asum
                [ 200 <$ eqT @t @Text
                , 200 <$ eqT @t @String
                ]
        }

-- | Set the maximum length of the implied ID column. This is required for
-- any type where the associated 'SqlType' is a @TEXT@ or @VARCHAR@ sort of
-- thing.
--
-- @since 2.13.0.0
setImplicitIdDefMaxLen
    :: Integer
    -> ImplicitIdDef
    -> ImplicitIdDef
setImplicitIdDefMaxLen i iid = iid { iidMaxLen = Just i }

-- |  This function converts a 'Typeable' type into a @persistent@
-- representation of the type of a field - 'FieldTyp'.
--
-- @since 2.13.0.0
fieldTypeFromTypeable :: forall t. (PersistField t, Typeable t) => FieldType
fieldTypeFromTypeable = go (typeRep @t)
  where
    go :: forall k (a :: k). TypeRep a -> FieldType
    go tr =
        case tr of
            Con tyCon ->
                FTTypeCon Nothing $ Text.pack $ tyConName tyCon
            App trA trB ->
                FTApp (go trA) (go trB)
            Fun _ _ ->
                error "No functions in field defs."

-- | Remove the default attribute of the 'ImplicitIdDef' column. This will
-- require you to provide an ID for the model with every insert, using
-- 'insertKey' instead of 'insert', unless the type has some means of getting
-- around that in the migrations.
--
-- As an example, the Postgresql @SERIAL@ type expands to an autoincrementing
-- integer. Postgres will implicitly create the relevant series and set the
-- default to be @NEXTVAL('series_name')@. A default is therefore unnecessary to
-- use for this type.
--
-- However, for a @UUID@, postgres *does not* have an implicit default. You must
-- either specify a default UUID generation function, or insert them yourself
-- (again, using 'insertKey').
--
-- This function will be deprecated in the future when omiting the default
-- implicit ID column is more fully supported.
--
-- @since 2.13.0.0
unsafeClearDefaultImplicitId :: ImplicitIdDef -> ImplicitIdDef
unsafeClearDefaultImplicitId iid = iid { iidDefault = Nothing }
