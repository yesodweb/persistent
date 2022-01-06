{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module EmptyEntityTest (specsWith, migration, cleanDB) where

import Database.Persist.Sql
import Database.Persist.TH

import Init

-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "migration"] [persistLowerCase|
EmptyEntity
|]

cleanDB
    ::
    ( PersistQueryWrite SqlBackend
    , MonadIO m
    , PersistStoreWrite (BaseBackend SqlBackend)
    )
    => ReaderT SqlBackend m ()
cleanDB = deleteWhere ([] :: [Filter EmptyEntity])

specsWith
    :: Runner SqlBackend m
    => RunDb SqlBackend m
    -> Maybe (ReaderT SqlBackend m a)
    -> Spec
specsWith runConn mmigrate = describe "empty entity" $
    it "inserts" $ asIO $ runConn $ do
        _ <- sequence_ mmigrate
        -- Ensure reading the data from the database works...
        _ <- sequence_ mmigrate
        x <- insert EmptyEntity
        Just EmptyEntity <- get x
        return ()
