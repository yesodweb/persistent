{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module MigrationSpec where

import PgInit

import qualified Data.Map as Map
import Database.Persist.Postgresql.Internal.Migration
import qualified Database.Persist.SqlBackend.Internal as SqlBackend

runConnPrepare
    :: (MonadIO m) => ((Text -> IO Statement) -> a -> IO b) -> a -> SqlPersistT m b
runConnPrepare inner arg = do
    backend <- ask
    liftIO $ inner (SqlBackend.connPrepare backend) arg

spec :: Spec
spec = describe "MigrationSpec" $ do
    it "works" $ runConnAssert $ do
        let
            rawEx sql = rawExecute sql []
        rawEx
            "CREATE TABLE users(id serial primary key, name text not null, title text);"
        rawEx
            "CREATE TABLE user_friendships(id serial primary key, user_1_id int references users(id), user_2_id int references users(id));"
        rawEx
            "CREATE TABLE passwords(id serial primary key, password_hash text, user_id int unique references users(id));"
        rawEx
            "CREATE TABLE passwords_2(id serial primary key, password_hash text, user_id int unique references users(id));"
        rawEx "CREATE TABLE ignored(id serial primary key);"

        actual <-
            runConnPrepare collectSchemaState $
                map
                    EntityNameDB
                    [ "users"
                    , "user_friendships"
                    , "passwords"
                    , "passwords_2"
                    , "nonexistent"
                    ]

        let
            expected =
                SchemaState $
                    Map.fromList
                        [ (EntityNameDB{unEntityNameDB = "nonexistent"}, EntityDoesNotExist)
                        ,
                            ( EntityNameDB{unEntityNameDB = "passwords"}
                            , EntityExists
                                ( ExistingEntitySchemaState
                                    { essColumns =
                                        [ Column
                                            { cName = FieldNameDB{unFieldNameDB = "user_id"}
                                            , cNull = True
                                            , cSqlType = SqlInt32
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
                                                            FieldCascade{fcOnUpdate = Just NoAction, fcOnDelete = Just NoAction}
                                                        }
                                                    )
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "password_hash"}
                                            , cNull = True
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
                                            , cSqlType = SqlInt32
                                            , cDefault = Just "nextval('passwords_id_seq'::regclass)"
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference = Nothing
                                            }
                                        ]
                                    , essConstraints =
                                        Map.fromList
                                            [
                                                ( ConstraintNameDB{unConstraintNameDB = "passwords_user_id_key"}
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
                                            , cSqlType = SqlInt32
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
                                                            FieldCascade{fcOnUpdate = Just NoAction, fcOnDelete = Just NoAction}
                                                        }
                                                    )
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "password_hash"}
                                            , cNull = True
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
                                            , cSqlType = SqlInt32
                                            , cDefault = Just "nextval('passwords_2_id_seq'::regclass)"
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference = Nothing
                                            }
                                        ]
                                    , essConstraints =
                                        Map.fromList
                                            [
                                                ( ConstraintNameDB{unConstraintNameDB = "passwords_2_user_id_key"}
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
                                            { cName = FieldNameDB{unFieldNameDB = "user_2_id"}
                                            , cNull = True
                                            , cSqlType = SqlInt32
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference =
                                                Just
                                                    ( ColumnReference
                                                        { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                        , crConstraintName =
                                                            ConstraintNameDB{unConstraintNameDB = "user_friendships_user_2_id_fkey"}
                                                        , crFieldCascade =
                                                            FieldCascade{fcOnUpdate = Just NoAction, fcOnDelete = Just NoAction}
                                                        }
                                                    )
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "user_1_id"}
                                            , cNull = True
                                            , cSqlType = SqlInt32
                                            , cDefault = Nothing
                                            , cGenerated = Nothing
                                            , cDefaultConstraintName = Nothing
                                            , cMaxLen = Nothing
                                            , cReference =
                                                Just
                                                    ( ColumnReference
                                                        { crTableName = EntityNameDB{unEntityNameDB = "users"}
                                                        , crConstraintName =
                                                            ConstraintNameDB{unConstraintNameDB = "user_friendships_user_1_id_fkey"}
                                                        , crFieldCascade =
                                                            FieldCascade{fcOnUpdate = Just NoAction, fcOnDelete = Just NoAction}
                                                        }
                                                    )
                                            }
                                        , Column
                                            { cName = FieldNameDB{unFieldNameDB = "id"}
                                            , cNull = False
                                            , cSqlType = SqlInt32
                                            , cDefault = Just "nextval('user_friendships_id_seq'::regclass)"
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
                                            , cSqlType = SqlInt32
                                            , cDefault = Just "nextval('users_id_seq'::regclass)"
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

        actual `shouldBe` Right expected
