{-# LANGUAGE CPP #-}

import qualified PersistentTest
import qualified RenameTest
import qualified DataTypeTest
import qualified HtmlTest
import qualified JoinTest
import qualified EmbedTest
import qualified LargeNumberTest
import qualified MaxLenTest
import Test.Hspec.Monadic (hspecX)
import Init
import System.Exit
import Control.Monad (unless)


#ifdef MongoDB
setup = setupMongo
#else
import Database.Persist.GenericSql (printMigration, runMigrationUnsafe)

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
  runConn (setup PersistentTest.testMigrate)
#endif
  r <- hspecB $ PersistentTest.specs
  (liftIO $ runConn PersistentTest.cleanDB)
  unless r $ exitWith (toExitCode r)

#ifndef WITH_MONGODB
  runConn (setup EmbedTest.embedMigrate)
  runConn (setup LargeNumberTest.numberMigrate)
  runConn (setup JoinTest.joinMigrate)
  runConn (setup MaxLenTest.maxlenMigrate)
#endif

  hspecX $
    RenameTest.specs >>
    DataTypeTest.specs >>
    HtmlTest.specs >>
    JoinTest.specs >>
    EmbedTest.specs >>
    LargeNumberTest.specs >>
    MaxLenTest.specs
