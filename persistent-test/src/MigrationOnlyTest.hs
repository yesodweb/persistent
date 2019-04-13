{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module MigrationOnlyTest (specsWith, migrateAll1, migrateAll2) where

import qualified Data.Text as T

import Database.Persist.TH
import Init

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll1"] [persistLowerCase|
TwoField1 sql=two_field
    field1 Int
    field2 T.Text
    field3 Bool Maybe
    deriving Eq Show
|]

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migrateAll2", mkDeleteCascade sqlSettings] [persistLowerCase|
TwoField
    field1 Int
    field2 T.Text
    field3 Bool Maybe MigrationOnly
    deriving Eq Show

Referencing
    field1 Int
    field2 TwoFieldId MigrationOnly
|]

specsWith
    :: (MonadIO m, PersistQueryWrite backend, PersistStoreWrite backend, PersistQueryWrite (BaseBackend backend))
    => RunDb backend m
    -> Maybe (ReaderT backend m a)
    -> Spec
specsWith runDb mmigrate = describe "MigrationOnly field" $ do
    it "doesn't have the field in the Haskell entity" $ asIO $ runDb $ do
        sequence_ mmigrate
        sequence_ mmigrate
        let tf = TwoField 5 "hello"
        tid <- insert tf
        mtf <- get tid
        liftIO $ mtf @?= Just tf
        deleteWhere ([] :: [Filter (TwoFieldGeneric backend)])
