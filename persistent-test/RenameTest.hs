{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module RenameTest where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist.GenericSql.Raw
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

-- Test lower case names
share [mkPersist sqlMkSettings, mkMigrate "lowerCase"] [persistLowerCase|
LowerCaseTable id=my_id
    fullName String
RefTable
    someVal Int sql=something_else
    lct LowerCaseTableId
    UniqueRefTable someVal
|]

runConn2 :: C.ResourceIO m => SqlPersist m t -> m ()
runConn2 f = do
    _ <- withSqlitePool ":memory:" 1 $ runSqlPool f
#if WITH_POSTGRESQL
    _ <- withPostgresqlPool (ConnectInfo "localhost" 5432 "test" "test" "test") 1 $ runSqlPool f
#endif
    return ()

renameSpecs :: Specs
renameSpecs = describe "rename specs" $ do
    it "handles lower casing" $ asIO $ do
        runConn2 $ do
            _ <- runMigrationSilent lowerCase
            C.runResourceT $ withStmt "SELECT full_name from lower_case_table WHERE my_id=5" [] C.$$ CL.sinkNull
            C.runResourceT $ withStmt "SELECT something_else from ref_table WHERE id=4" [] C.$$ CL.sinkNull

asIO :: IO a -> IO a
asIO = id
