{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module RenameTest
    ( renameSpecs
    ) where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit hiding (Test)
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist.GenericSql.Raw
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
#if MIN_VERSION_monad_control(0, 3, 0)
import qualified Control.Monad.Trans.Control
#else
import qualified Control.Monad.IO.Control
#endif
import Control.Monad.IO.Class (MonadIO)

-- Test lower case names
share [mkPersist sqlMkSettings, mkMigrate "lowerCase"] [persistLowerCase|
LowerCaseTable id=my_id
    fullName String
RefTable
    someVal Int sql=something_else
    lct LowerCaseTableId
    UniqueRefTable someVal
|]

runConn ::
#if MIN_VERSION_monad_control(0, 3, 0)
    (Control.Monad.Trans.Control.MonadBaseControl IO m, MonadIO m)
#else
    Control.Monad.IO.Control.MonadControlIO m
#endif
    => SqlPersist m t -> m ()
runConn f = do
    _ <- withSqlitePool ":memory:" 1 $ runSqlPool f
#if WITH_POSTGRESQL
    _ <- withPostgresqlPool "user=test password=test host=localhost port=5432 dbname=test" 1 $ runSqlPool f
#endif
    return ()

renameSpecs :: Specs
renameSpecs = describe "rename specs" $ do
    it "handles lower casing" $ asIO $ do
        runConn $ do
            runMigrationSilent lowerCase
            withStmt "SELECT full_name from lower_case_table WHERE my_id=5" [] $ const $ return ()
            withStmt "SELECT something_else from ref_table WHERE id=4" [] $ const $ return ()

asIO :: IO a -> IO a
asIO = id
