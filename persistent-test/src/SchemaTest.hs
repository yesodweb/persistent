{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module SchemaTest (specsWith, migration, cleanDB) where

import Database.Persist.Sql
import Database.Persist.TH

import Init

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migration"] [persistLowerCase|
SchemaEntity schema=foo
    foo Int
    Primary foo
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
                { schemaEntityFoo = 42
                }
        Just schemaEntity <- get x
        rawFoo  <- rawSql "SELECT foo FROM foo.schema_entity" []
        liftIO $ rawFoo @?= [Single (42 :: Int)]
        return ()
