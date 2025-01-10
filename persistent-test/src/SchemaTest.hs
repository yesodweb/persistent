{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module SchemaTest (specsWith, migration, cleanDB) where

import Database.Persist.Sql
import Database.Persist.TH

import Init

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migration"] [persistLowerCase|
SchemaEntity schema=foo
    bar Int
    Primary bar

DefaultSchemaEntity sql=schema_entity
    bar Int
    Primary bar
|]

cleanDB
    ::
    ( PersistQueryWrite backend
    , MonadIO m
    , PersistStoreWrite (BaseBackend backend)
    )
    => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter (SchemaEntityGeneric backend)])
  deleteWhere ([] :: [Filter (DefaultSchemaEntityGeneric backend)])

specsWith
    :: Runner SqlBackend m
    => RunDb SqlBackend m
    -> Spec
specsWith runConn = describe "entity with non-null schema" $ do
    it "inserts and selects work as expected" $ asIO $ runConn $ do
        -- Ensure we can write to the database
        x <- insert $
            SchemaEntity
                { schemaEntityBar = 42
                }
        Just schemaEntity <- get x
        rawBar  <- rawSql "SELECT bar FROM foo.schema_entity" []
        liftIO $ rawBar @?= [Single (42 :: Int)]
        liftIO $ schemaEntityBar schemaEntity @== 42
        return ()
    it "is not ambiguous when both tables exist" $ asIO $ runConn $ do
        _ <- insert $
            SchemaEntity
                { schemaEntityBar = 42
                }
        _ <- insert $
            DefaultSchemaEntity
                { defaultSchemaEntityBar = 43
                }
        rawBar  <- rawSql "SELECT bar FROM schema_entity" []
        liftIO $ rawBar @?= [Single (43 :: Int)]
        return ()
