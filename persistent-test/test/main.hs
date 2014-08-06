{-# LANGUAGE CPP #-}

import qualified PersistentTest
import qualified RenameTest
import qualified DataTypeTest
import qualified EmptyEntityTest
import qualified HtmlTest
import qualified EmbedTest
import qualified EmbedOrderTest
import qualified LargeNumberTest
import qualified MaxLenTest
import qualified SumTypeTest
import qualified UniqueTest
import qualified MigrationTest
import qualified MigrationOnlyTest
import qualified CompositeTest
import Test.Hspec (hspec)
import Test.Hspec.Runner
import Init
import System.Exit
import Control.Monad (unless, when)
import Filesystem (isFile, removeFile)
import Filesystem.Path.CurrentOS (fromText)
import Control.Monad.Trans.Resource (runResourceT)


#ifdef MongoDB
setup = setupMongo
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
#ifndef WITH_MONGODB
  sqExists <- isFile $ fromText sqlite_database
  when sqExists $ removeFile $ fromText sqlite_database
  runConn (setup PersistentTest.testMigrate)
  runConn (setup PersistentTest.noPrefixMigrate)
#endif
  runConn PersistentTest.cleanDB

#ifndef WITH_MONGODB
  runConn (setup EmbedTest.embedMigrate)
  runConn (setup EmbedOrderTest.embedOrderMigrate)
  runConn (setup LargeNumberTest.numberMigrate)
  runConn (setup UniqueTest.uniqueMigrate)
  runConn (setup MaxLenTest.maxlenMigrate)
  runConn (setup CompositeTest.compositeMigrate)
  runConn (setup MigrationTest.migrationMigrate)
#endif

  hspec $ do
    PersistentTest.specs
    RenameTest.specs
#ifndef WITH_POSTGRESQL
    DataTypeTest.specs
#endif
    HtmlTest.specs
    EmbedTest.specs
    EmbedOrderTest.specs
    LargeNumberTest.specs
    UniqueTest.specs
    MaxLenTest.specs
    SumTypeTest.specs
    MigrationOnlyTest.specs
    PersistentTest.specs
#ifndef WITH_MONGODB
    CompositeTest.specs
    MigrationTest.specs
#endif
    EmptyEntityTest.specs
