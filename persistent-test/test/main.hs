{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified PersistentTest
import qualified RenameTest
import qualified DataTypeTest
import qualified EmptyEntityTest
import qualified HtmlTest
import qualified EmbedTest
import qualified EmbedOrderTest
import qualified LargeNumberTest
import qualified MaxLenTest
import qualified Recursive
import qualified SumTypeTest
import qualified UniqueTest
import qualified MigrationTest
import qualified MigrationOnlyTest
import qualified PersistUniqueTest
import qualified CompositeTest
import qualified PrimaryTest
import Test.Hspec (hspec)
import Test.Hspec.Runner
import Init
import System.Exit
import Control.Monad (unless, when)
import Filesystem (isFile, removeFile)
import Filesystem.Path.CurrentOS (fromText)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Exception (handle, IOException)


#ifdef WITH_NOSQL
#else
import Database.Persist.Sql (printMigration, runMigrationUnsafe)

setup migration = do
  printMigration migration
  runMigrationUnsafe migration
#endif

toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1

main :: IO ()
main = do
#ifndef WITH_NOSQL
  handle (\(_ :: IOException) -> return ())
    $ removeFile $ fromText sqlite_database

  runConn $ do
    mapM_ setup
      [ PersistentTest.testMigrate
      , PersistentTest.noPrefixMigrate
      , EmbedTest.embedMigrate
      , EmbedOrderTest.embedOrderMigrate
      , LargeNumberTest.numberMigrate
      , UniqueTest.uniqueMigrate
      , MaxLenTest.maxlenMigrate
      , Recursive.recursiveMigrate
      , CompositeTest.compositeMigrate
      , MigrationTest.migrationMigrate
      , PersistUniqueTest.migration
      , PrimaryTest.migration
      ]
    PersistentTest.cleanDB
#endif

  hspec $ do
    RenameTest.specs
    DataTypeTest.specs
    HtmlTest.specs
    EmbedTest.specs
    EmbedOrderTest.specs
    LargeNumberTest.specs
    UniqueTest.specs
    MaxLenTest.specs
    Recursive.specs
    SumTypeTest.specs
    MigrationOnlyTest.specs
    PersistentTest.specs
    EmptyEntityTest.specs
    CompositeTest.specs
    PersistUniqueTest.specs
    PrimaryTest.specs

#ifndef WITH_NOSQL
    MigrationTest.specs
    PersistentTest.specs
#endif
