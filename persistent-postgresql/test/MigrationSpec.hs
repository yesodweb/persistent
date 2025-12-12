{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module MigrationSpec where

import PgInit

import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import Database.Persist.Postgresql.Internal.Migration

getStmtGetter
    :: (Monad m) => SqlPersistT m (Text -> IO Statement)
getStmtGetter = do
    backend <- ask
    pure (getStmtConn backend)

-- NB: we do not perform these migrations in main.hs
share
    [mkPersist persistSettings{mpsGeneric = False}]
    [persistLowerCase|
User sql=users
    name Text
    title Text Maybe
    deriving Show Eq

UserFriendship sql=user_friendships
    user1Id UserId Maybe
    user2Id UserId Maybe
    deriving Show Eq

Password sql=passwords
    passwordHash Text
    userId UserId Maybe
    UniqueUserId userId !force

Password2 sql=passwords_2
    passwordHash Text
    userId UserId Maybe OnDeleteCascade OnUpdateSetNull
    UniqueUserId2 userId !force

AdminUser sql=admin_users
    userId UserId
    Primary userId

    promotedByUserId UserId
    UniquePromotedByUserId promotedByUserId

FKParent sql=migration_fk_parent

FKChildV1 sql=migration_fk_child

-- Simulate creating a new FK field on an existing table
FKChildV2 sql=migration_fk_child
    parentId FKParentId

ExplicitPrimaryKey sql=explicit_primary_key
    Id Text
|]

userEntityDef :: EntityDef
userEntityDef = entityDef (Proxy :: Proxy User)

userFriendshipEntityDef :: EntityDef
userFriendshipEntityDef = entityDef (Proxy :: Proxy UserFriendship)

passwordEntityDef :: EntityDef
passwordEntityDef = entityDef (Proxy :: Proxy Password)

password2EntityDef :: EntityDef
password2EntityDef = entityDef (Proxy :: Proxy Password2)

adminUserEntityDef :: EntityDef
adminUserEntityDef = entityDef (Proxy :: Proxy AdminUser)

fkParentEntityDef :: EntityDef
fkParentEntityDef = entityDef (Proxy :: Proxy FKParent)

fkChildV1EntityDef :: EntityDef
fkChildV1EntityDef = entityDef (Proxy :: Proxy FKChildV1)

fkChildV2EntityDef :: EntityDef
fkChildV2EntityDef = entityDef (Proxy :: Proxy FKChildV2)

explicitPrimaryKeyEntityDef :: EntityDef
explicitPrimaryKeyEntityDef = entityDef (Proxy :: Proxy ExplicitPrimaryKey)

-- Note that FKChild is deliberately omitted here because we have two
-- versions of it
allEntityDefs :: [EntityDef]
allEntityDefs =
    [ userEntityDef
    , userFriendshipEntityDef
    , passwordEntityDef
    , password2EntityDef
    , adminUserEntityDef
    , fkParentEntityDef
    , explicitPrimaryKeyEntityDef
    ]

-- Note that this function migrates to the schema expected by FKChildV1
migrateManually :: (HasCallStack, MonadIO m) => SqlPersistT m ()
migrateManually = do
    cleanDB
    let
        rawEx sql = rawExecute sql []
    rawEx
        "CREATE TABLE users(id int8 primary key, name text not null, title text);"
    rawEx $
        T.concat
            [ "CREATE TABLE user_friendships("
            , "  id int8 primary key,"
            , "  user1_id int8 references users(id) on delete restrict on update restrict,"
            , "  user2_id int8 references users(id) on delete restrict on update restrict"
            , ");"
            ]
    rawEx $
        T.concat
            [ "CREATE TABLE passwords("
            , "  id int8 primary key,"
            , "  password_hash text not null,"
            , "  user_id int8 references users(id) on delete restrict on update restrict"
            , ");"
            ]
    rawEx $
        T.concat
            [ "ALTER TABLE passwords"
            , "  ADD CONSTRAINT unique_user_id"
            , "  UNIQUE(user_id);"
            ]
    rawEx $
        T.concat
            [ "CREATE TABLE passwords_2("
            , "  id int8 primary key,"
            , "  password_hash text not null,"
            , "  user_id int8 references users(id) on delete cascade on update set null"
            , ");"
            ]
    rawEx $
        T.concat
            [ "ALTER TABLE passwords_2"
            , "  ADD CONSTRAINT unique_user_id2"
            , "  UNIQUE(user_id);"
            ]
    -- Add an extra redundant FK constraint on passwords_2.user_id, so that we
    -- can test that the migrator ignores it
    rawEx $
        T.concat
            [ "ALTER TABLE passwords_2"
            , "  ADD CONSTRAINT duplicate_passwords_2_user_id_fkey"
            , "  FOREIGN KEY (user_id) REFERENCES users(id);"
            ]
    rawEx $
        T.concat
            [ "CREATE TABLE admin_users("
            , "  user_id int8 not null references users(id) on delete restrict on update restrict primary key,"
            , "  promoted_by_user_id int8 not null references users(id) on delete restrict on update restrict"
            , ");"
            ]
    rawEx $
        T.concat
            [ "ALTER TABLE admin_users"
            , "  ADD CONSTRAINT unique_promoted_by_user_id"
            , "  UNIQUE(promoted_by_user_id);"
            ]
    rawEx "CREATE TABLE migration_fk_parent(id int8 primary key);"
    rawEx "CREATE TABLE migration_fk_child(id int8 primary key);"
    rawEx "CREATE TABLE explicit_primary_key(id text primary key);"
    rawEx "CREATE TABLE ignored(id int8 primary key);"

cleanDB :: (HasCallStack, MonadIO m) => SqlPersistT m ()
cleanDB = do
    let
        rawEx sql = rawExecute sql []
    rawEx "DROP TABLE IF EXISTS user_friendships;"
    rawEx "DROP TABLE IF EXISTS passwords;"
    rawEx "DROP TABLE IF EXISTS passwords_2;"
    rawEx "DROP TABLE IF EXISTS ignored;"
    rawEx "DROP TABLE IF EXISTS admin_users;"
    rawEx "DROP TABLE IF EXISTS users;"
    rawEx "DROP TABLE IF EXISTS migration_fk_child;"
    rawEx "DROP TABLE IF EXISTS migration_fk_parent;"
    rawEx "DROP TABLE IF EXISTS explicit_primary_key;"

spec :: Spec
spec = describe "MigrationSpec" $ do
    it "gathers schema state" $ runConnAssert $ do
        migrateManually

        getter <- getStmtGetter
        actual <-
            liftIO $
                collectSchemaState getter $
                    map
                        EntityNameDB
                        [ "users"
                        , "admin_users"
                        , "user_friendships"
                        , "passwords"
                        , "passwords_2"
                        , "nonexistent"
                        ]

        cleanDB

        let
            expected =
                SchemaState
                    ( Map.fromList
                        [
                            ( EntityNameDB{unEntityNameDB = "admin_users"}
                            , EntityExists
                                ( ExistingEntitySchemaState
                                    { essColumns =
                                        Map.fromList
                                            [
                                                ( FieldNameDB{unFieldNameDB = "promoted_by_user_id"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "promoted_by_user_id"}
                                                        , cNull = False
                                                        , cSqlType = SqlInt64
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList
                                                        [ ColumnReference
                                                            { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                            , crConstraintName =
                                                                ConstraintNameDB{unConstraintNameDB = "admin_users_promoted_by_user_id_fkey"}
                                                            , crFieldCascade =
                                                                FieldCascade{fcOnUpdate = Just Restrict, fcOnDelete = Just Restrict}
                                                            }
                                                        ]
                                                    )
                                                )
                                            ,
                                                ( FieldNameDB{unFieldNameDB = "user_id"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "user_id"}
                                                        , cNull = False
                                                        , cSqlType = SqlInt64
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList
                                                        [ ColumnReference
                                                            { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                            , crConstraintName =
                                                                ConstraintNameDB{unConstraintNameDB = "admin_users_user_id_fkey"}
                                                            , crFieldCascade =
                                                                FieldCascade{fcOnUpdate = Just Restrict, fcOnDelete = Just Restrict}
                                                            }
                                                        ]
                                                    )
                                                )
                                            ]
                                    , essUniqueConstraints =
                                        Map.fromList
                                            [
                                                ( ConstraintNameDB{unConstraintNameDB = "unique_promoted_by_user_id"}
                                                , [FieldNameDB{unFieldNameDB = "promoted_by_user_id"}]
                                                )
                                            ]
                                    }
                                )
                            )
                        , (EntityNameDB{unEntityNameDB = "nonexistent"}, EntityDoesNotExist)
                        ,
                            ( EntityNameDB{unEntityNameDB = "passwords"}
                            , EntityExists
                                ( ExistingEntitySchemaState
                                    { essColumns =
                                        Map.fromList
                                            [
                                                ( FieldNameDB{unFieldNameDB = "id"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "id"}
                                                        , cNull = False
                                                        , cSqlType = SqlInt64
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList []
                                                    )
                                                )
                                            ,
                                                ( FieldNameDB{unFieldNameDB = "password_hash"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "password_hash"}
                                                        , cNull = False
                                                        , cSqlType = SqlString
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList []
                                                    )
                                                )
                                            ,
                                                ( FieldNameDB{unFieldNameDB = "user_id"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "user_id"}
                                                        , cNull = True
                                                        , cSqlType = SqlInt64
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList
                                                        [ ColumnReference
                                                            { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                            , crConstraintName =
                                                                ConstraintNameDB{unConstraintNameDB = "passwords_user_id_fkey"}
                                                            , crFieldCascade =
                                                                FieldCascade{fcOnUpdate = Just Restrict, fcOnDelete = Just Restrict}
                                                            }
                                                        ]
                                                    )
                                                )
                                            ]
                                    , essUniqueConstraints =
                                        Map.fromList
                                            [
                                                ( ConstraintNameDB{unConstraintNameDB = "unique_user_id"}
                                                , [FieldNameDB{unFieldNameDB = "user_id"}]
                                                )
                                            ]
                                    }
                                )
                            )
                        ,
                            ( EntityNameDB{unEntityNameDB = "passwords_2"}
                            , EntityExists
                                ( ExistingEntitySchemaState
                                    { essColumns =
                                        Map.fromList
                                            [
                                                ( FieldNameDB{unFieldNameDB = "id"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "id"}
                                                        , cNull = False
                                                        , cSqlType = SqlInt64
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList []
                                                    )
                                                )
                                            ,
                                                ( FieldNameDB{unFieldNameDB = "password_hash"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "password_hash"}
                                                        , cNull = False
                                                        , cSqlType = SqlString
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList []
                                                    )
                                                )
                                            ,
                                                ( FieldNameDB{unFieldNameDB = "user_id"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "user_id"}
                                                        , cNull = True
                                                        , cSqlType = SqlInt64
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList
                                                        [ ColumnReference
                                                            { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                            , crConstraintName =
                                                                ConstraintNameDB{unConstraintNameDB = "duplicate_passwords_2_user_id_fkey"}
                                                            , crFieldCascade =
                                                                FieldCascade{fcOnUpdate = Just NoAction, fcOnDelete = Just NoAction}
                                                            }
                                                        , ColumnReference
                                                            { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                            , crConstraintName =
                                                                ConstraintNameDB{unConstraintNameDB = "passwords_2_user_id_fkey"}
                                                            , crFieldCascade =
                                                                FieldCascade{fcOnUpdate = Just SetNull, fcOnDelete = Just Cascade}
                                                            }
                                                        ]
                                                    )
                                                )
                                            ]
                                    , essUniqueConstraints =
                                        Map.fromList
                                            [
                                                ( ConstraintNameDB{unConstraintNameDB = "unique_user_id2"}
                                                , [FieldNameDB{unFieldNameDB = "user_id"}]
                                                )
                                            ]
                                    }
                                )
                            )
                        ,
                            ( EntityNameDB{unEntityNameDB = "user_friendships"}
                            , EntityExists
                                ( ExistingEntitySchemaState
                                    { essColumns =
                                        Map.fromList
                                            [
                                                ( FieldNameDB{unFieldNameDB = "id"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "id"}
                                                        , cNull = False
                                                        , cSqlType = SqlInt64
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList []
                                                    )
                                                )
                                            ,
                                                ( FieldNameDB{unFieldNameDB = "user1_id"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "user1_id"}
                                                        , cNull = True
                                                        , cSqlType = SqlInt64
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList
                                                        [ ColumnReference
                                                            { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                            , crConstraintName =
                                                                ConstraintNameDB{unConstraintNameDB = "user_friendships_user1_id_fkey"}
                                                            , crFieldCascade =
                                                                FieldCascade{fcOnUpdate = Just Restrict, fcOnDelete = Just Restrict}
                                                            }
                                                        ]
                                                    )
                                                )
                                            ,
                                                ( FieldNameDB{unFieldNameDB = "user2_id"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "user2_id"}
                                                        , cNull = True
                                                        , cSqlType = SqlInt64
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList
                                                        [ ColumnReference
                                                            { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                            , crConstraintName =
                                                                ConstraintNameDB{unConstraintNameDB = "user_friendships_user2_id_fkey"}
                                                            , crFieldCascade =
                                                                FieldCascade{fcOnUpdate = Just Restrict, fcOnDelete = Just Restrict}
                                                            }
                                                        ]
                                                    )
                                                )
                                            ]
                                    , essUniqueConstraints = Map.fromList []
                                    }
                                )
                            )
                        ,
                            ( EntityNameDB{unEntityNameDB = "users"}
                            , EntityExists
                                ( ExistingEntitySchemaState
                                    { essColumns =
                                        Map.fromList
                                            [
                                                ( FieldNameDB{unFieldNameDB = "id"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "id"}
                                                        , cNull = False
                                                        , cSqlType = SqlInt64
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList []
                                                    )
                                                )
                                            ,
                                                ( FieldNameDB{unFieldNameDB = "name"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "name"}
                                                        , cNull = False
                                                        , cSqlType = SqlString
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList []
                                                    )
                                                )
                                            ,
                                                ( FieldNameDB{unFieldNameDB = "title"}
                                                ,
                                                    ( Column
                                                        { cName = FieldNameDB{unFieldNameDB = "title"}
                                                        , cNull = True
                                                        , cSqlType = SqlString
                                                        , cDefault = Nothing
                                                        , cGenerated = Nothing
                                                        , cDefaultConstraintName = Nothing
                                                        , cMaxLen = Nothing
                                                        , cReference = Nothing
                                                        }
                                                    , Set.fromList []
                                                    )
                                                )
                                            ]
                                    , essUniqueConstraints = Map.fromList []
                                    }
                                )
                            )
                        ]
                    )

        actual `shouldBe` Right expected

    it "no-ops on a migrated DB" $ runConnAssert $ do
        migrateManually

        getter <- getStmtGetter
        result <-
            liftIO $ migrateEntitiesStructured getter allEntityDefs allEntityDefs

        cleanDB

        case result of
            Right [] ->
                pure ()
            Left err ->
                expectationFailure $ show err
            Right alters ->
                map (snd . showAlterDb) alters `shouldBe` []

    it "migrates a clean DB" $ runConnAssert $ do
        cleanDB

        getter <- getStmtGetter
        result <-
            liftIO $ migrateEntitiesStructured getter allEntityDefs allEntityDefs

        cleanDB

        case result of
            Right [] ->
                pure ()
            Left err ->
                expectationFailure $ show err
            Right alters -> do
                traverse_ (flip rawExecute [] . snd . showAlterDb) alters
                result2 <-
                    liftIO $ migrateEntitiesStructured getter allEntityDefs allEntityDefs
                result2 `shouldBe` Right []

    it "suggests FK constraints for new fields first time" $ runConnAssert $ do
        migrateManually

        getter <- getStmtGetter
        result <-
            liftIO $
                migrateEntitiesStructured
                    getter
                    (fkChildV2EntityDef : allEntityDefs)
                    [fkChildV2EntityDef]

        cleanDB

        case result of
            Right [] ->
                pure ()
            Left err ->
                expectationFailure $ show err
            Right alters ->
                map (snd . showAlterDb) alters
                    `shouldBe` [ "ALTER TABLE \"migration_fk_child\" ADD COLUMN \"parent_id\" INT8 NOT NULL"
                               , "ALTER TABLE \"migration_fk_child\" ADD CONSTRAINT \"migration_fk_child_parent_id_fkey\" FOREIGN KEY(\"parent_id\") REFERENCES \"migration_fk_parent\"(\"id\") ON DELETE RESTRICT  ON UPDATE RESTRICT"
                               ]
