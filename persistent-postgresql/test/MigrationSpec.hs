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
{-# LANGUAGE UndecidableInstances #-}

module MigrationSpec where

import PgInit

import qualified Data.Map as Map
import Data.Proxy
import qualified Data.Text as T
import Database.Persist.Postgresql.Internal.Migration
import qualified Database.Persist.SqlBackend.Internal as SqlBackend

getConnPrepare
    :: (Monad m) => SqlPersistT m (Text -> IO Statement)
getConnPrepare = do
    backend <- ask
    pure (SqlBackend.connPrepare backend)

-- NB: we do not perform these migrations in main.hs
share
    [mkPersist persistSettings{mpsGeneric = False}, mkMigrate "migrate"]
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
|]

userEntityDef :: EntityDef
userEntityDef = entityDef (Proxy :: Proxy User)

userFriendshipEntityDef :: EntityDef
userFriendshipEntityDef = entityDef (Proxy :: Proxy UserFriendship)

passwordEntityDef :: EntityDef
passwordEntityDef = entityDef (Proxy :: Proxy Password)

password2EntityDef :: EntityDef
password2EntityDef = entityDef (Proxy :: Proxy Password2)

allEntityDefs :: [EntityDef]
allEntityDefs =
    [ userEntityDef
    , userFriendshipEntityDef
    , passwordEntityDef
    , password2EntityDef
    ]

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
    rawEx "CREATE TABLE ignored(id int8 primary key);"

cleanDB :: (HasCallStack, MonadIO m) => SqlPersistT m ()
cleanDB = do
    let
        rawEx sql = rawExecute sql []
    rawEx "DROP TABLE IF EXISTS user_friendships;"
    rawEx "DROP TABLE IF EXISTS passwords;"
    rawEx "DROP TABLE IF EXISTS passwords_2;"
    rawEx "DROP TABLE IF EXISTS ignored;"
    rawEx "DROP TABLE IF EXISTS users;"

spec :: Spec
spec = describe "MigrationSpec" $ do
    it "gathers schema state" $ runConnAssert $ do
        migrateManually

        connPrepare <- getConnPrepare
        actual <-
            liftIO $
                collectSchemaState connPrepare $
                    map
                        EntityNameDB
                        [ "users"
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
                        [ (EntityNameDB{unEntityNameDB = "nonexistent"}, EntityDoesNotExist)
                        ,
                            ( EntityNameDB{unEntityNameDB = "passwords"}
                            , EntityExists
                                ( ExistingEntitySchemaState
                                    { essColumns =
                                        [ Column
                                            { cName = FieldNameDB{unFieldNameDB = "user_id"}
                                            , cNull = True
                                            , cSqlType = SqlInt64
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference =
                                                Just
                                                    ( ColumnReference
                                                        { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                        , crConstraintName =
                                                            ConstraintNameDB{unConstraintNameDB = "passwords_user_id_fkey"}
                                                        , crFieldCascade =
                                                            FieldCascade{fcOnUpdate = Just Restrict, fcOnDelete = Just Restrict}
                                                        }
                                                    )
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "password_hash"}
                                            , cNull = False
                                            , cSqlType = SqlString
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference = Nothing
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "id"}
                                            , cNull = False
                                            , cSqlType = SqlInt64
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference = Nothing
                                            }
                                        ]
                                    , essConstraints =
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
                                        [ Column
                                            { cName = FieldNameDB{unFieldNameDB = "user_id"}
                                            , cNull = True
                                            , cSqlType = SqlInt64
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference =
                                                Just
                                                    ( ColumnReference
                                                        { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                        , crConstraintName =
                                                            ConstraintNameDB{unConstraintNameDB = "passwords_2_user_id_fkey"}
                                                        , crFieldCascade =
                                                            FieldCascade{fcOnUpdate = Just SetNull, fcOnDelete = Just Cascade}
                                                        }
                                                    )
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "password_hash"}
                                            , cNull = False
                                            , cSqlType = SqlString
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference = Nothing
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "id"}
                                            , cNull = False
                                            , cSqlType = SqlInt64
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference = Nothing
                                            }
                                        ]
                                    , essConstraints =
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
                                        [ Column
                                            { cName = FieldNameDB{unFieldNameDB = "user2_id"}
                                            , cNull = True
                                            , cSqlType = SqlInt64
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference =
                                                Just
                                                    ( ColumnReference
                                                        { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                        , crConstraintName =
                                                            ConstraintNameDB{unConstraintNameDB = "user_friendships_user2_id_fkey"}
                                                        , crFieldCascade =
                                                            FieldCascade{fcOnUpdate = Just Restrict, fcOnDelete = Just Restrict}
                                                        }
                                                    )
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "user1_id"}
                                            , cNull = True
                                            , cSqlType = SqlInt64
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference =
                                                Just
                                                    ( ColumnReference
                                                        { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                        , crConstraintName =
                                                            ConstraintNameDB{unConstraintNameDB = "user_friendships_user1_id_fkey"}
                                                        , crFieldCascade =
                                                            FieldCascade{fcOnUpdate = Just Restrict, fcOnDelete = Just Restrict}
                                                        }
                                                    )
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "id"}
                                            , cNull = False
                                            , cSqlType = SqlInt64
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference = Nothing
                                            }
                                        ]
                                    , essConstraints = Map.fromList []
                                    }
                                )
                            )
                        ,
                            ( EntityNameDB{unEntityNameDB = "users"}
                            , EntityExists
                                ( ExistingEntitySchemaState
                                    { essColumns =
                                        [ Column
                                            { cName = FieldNameDB{unFieldNameDB = "title"}
                                            , cNull = True
                                            , cSqlType = SqlString
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference = Nothing
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "name"}
                                            , cNull = False
                                            , cSqlType = SqlString
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference = Nothing
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "id"}
                                            , cNull = False
                                            , cSqlType = SqlInt64
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference = Nothing
                                            }
                                        ]
                                    , essConstraints = Map.fromList []
                                    }
                                )
                            )
                        ]
                    )

        actual `shouldBe` Right expected

    it "no-ops on a migrated DB" $ runConnAssert $ do
        migrateManually

        connPrepare <- getConnPrepare
        result <-
            liftIO $ migrateEntitiesStructured connPrepare allEntityDefs allEntityDefs

        cleanDB

        case result of
            Right [] ->
                pure ()
            Left err ->
                expectationFailure $ show err
            Right alters ->
                map (snd . showAlterDb) alters `shouldBe` []
