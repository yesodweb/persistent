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
|]

cleanDB
    ::
    ( PersistQueryWrite backend
    , MonadIO m
    , PersistStoreWrite (BaseBackend backend)
    )
    => ReaderT backend m ()
cleanDB = deleteWhere ([] :: [Filter (SchemaEntityGeneric backend)])

specsWith
    :: Runner SqlBackend m
    => RunDb SqlBackend m
    -> Spec
specsWith runConn = describe "entity with non-null schema" $
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
