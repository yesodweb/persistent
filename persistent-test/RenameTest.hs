{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module RenameTest where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist.EntityDef
import Database.Persist.GenericSql.Raw
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import qualified Data.Text as T

-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "lowerCase"] [persistLowerCase|
LowerCaseTable id=my_id
    fullName String
    ExtraBlock
        foo bar
        baz
        bin
    ExtraBlock2
        something
RefTable
    someVal Int sql=something_else
    lct LowerCaseTableId
    UniqueRefTable someVal
|]

runConn2 :: C.ResourceIO m => SqlPersist m t -> m ()
runConn2 f = do
    _ <- withSqlitePool ":memory:" 1 $ runSqlPool f
#if WITH_POSTGRESQL
    _ <- withPostgresqlPool "host=localhost port=5432 user=test dbname=test password=test" 1 $ runSqlPool f
#endif
    return ()

renameSpecs :: Specs
renameSpecs = describe "rename specs" $ do
    it "handles lower casing" $ asIO $ do
        runConn2 $ do
            _ <- runMigrationSilent lowerCase
            C.runResourceT $ withStmt "SELECT full_name from lower_case_table WHERE my_id=5" [] C.$$ CL.sinkNull
            C.runResourceT $ withStmt "SELECT something_else from ref_table WHERE id=4" [] C.$$ CL.sinkNull
    it "extra blocks" $ do
        entityExtra (entityDef (undefined :: LowerCaseTable)) @?=
            Map.fromList
                [ ("ExtraBlock", map T.words ["foo bar", "baz", "bin"])
                , ("ExtraBlock2", map T.words ["something"])
                ]

asIO :: IO a -> IO a
asIO = id
