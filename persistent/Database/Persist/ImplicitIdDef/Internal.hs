{-# LANGUAGE RankNTypes, AllowAmbiguousTypes, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Persist.ImplicitIdDef.Internal where

import Data.Proxy
import Data.Text (Text)
import Language.Haskell.TH (Type)
import LiftType
import Type.Reflection
import qualified Data.Text as T

import Database.Persist.Names
import Database.Persist.Types
import Database.Persist.Sql.Class

-- |
--
-- @since 2.13.0.0
data ImplicitIdDef = ImplicitIdDef
    { iidFieldType :: EntityNameHS -> FieldType
    , iidFieldSqlType :: SqlType
    , iidType :: Bool -> Type -> Type
    -- ^ The Bool argument is whether or not the 'MkPersistBackend' type has the
    -- 'mpsGeneric' field set.
    --
    -- The 'Type' is the 'mpsBackend' value.
    , iidDefault :: Maybe Text
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
        }

fieldTypeFromTypeable :: forall (t :: *). Typeable t => FieldType
fieldTypeFromTypeable = go (typeRep @t)
  where
    go :: forall k (a :: k). TypeRep a -> FieldType
    go tr =
        case tr of
            Con tyCon ->
                let
                    tyName = T.pack $ tyConName tyCon
                    modName = T.pack $ tyConModule tyCon
                in
                    FTTypeCon Nothing tyName
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

