{-# LANGUAGE CPP #-}

import qualified PersistentTest
import qualified RenameTest
import qualified DataTypeTest
import qualified HtmlTest
import qualified EmbedTest
import qualified LargeNumberTest
import qualified MaxLenTest
import qualified SumTypeTest
import qualified UniqueTest
import qualified MigrationOnlyTest
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
#endif
  summary <- hspecWith defaultConfig $ PersistentTest.specs
  runResourceT $ runConn PersistentTest.cleanDB
  unless (summaryFailures summary == 0) $ exitWith (toExitCode False)

#ifndef WITH_MONGODB
  runConn (setup EmbedTest.embedMigrate)
  runConn (setup LargeNumberTest.numberMigrate)
  runConn (setup UniqueTest.uniqueMigrate)
  runConn (setup MaxLenTest.maxlenMigrate)
#endif

  hspec $ do
    RenameTest.specs
    DataTypeTest.specs
    HtmlTest.specs
    EmbedTest.specs
    LargeNumberTest.specs
    UniqueTest.specs
    MaxLenTest.specs
    SumTypeTest.specs
    MigrationOnlyTest.specs
