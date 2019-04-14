{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module EmptyEntityTest (specsWith, migration, cleanDB) where

import Database.Persist.Sql
import Database.Persist.TH

import Init

-- Test lower case names
share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migration"] [persistLowerCase|
EmptyEntity
|]

cleanDB
    ::
    ( PersistQueryWrite backend
    , MonadIO m
    , PersistStoreWrite (BaseBackend backend)
    )
    => ReaderT backend m ()
cleanDB = deleteWhere ([] :: [Filter (EmptyEntityGeneric backend)])

specsWith
    :: Runner backend m
    => RunDb backend m
    -> Maybe (ReaderT backend m a)
    -> Spec
specsWith runConn mmigrate = describe "empty entity" $
    it "inserts" $ asIO $ runConn $ do
        _ <- sequence_ mmigrate
        -- Ensure reading the data from the database works...
        _ <- sequence_ mmigrate
        x <- insert EmptyEntity
        Just EmptyEntity <- get x
        return ()
