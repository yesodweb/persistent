{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified CompositeTest
import qualified CustomPersistFieldTest
import qualified CustomPrimaryKeyReferenceTest
import qualified DataTypeTest
import qualified EmbedOrderTest
import qualified EmbedTest
import qualified EmptyEntityTest
import qualified HtmlTest
import Init
import qualified LargeNumberTest
import qualified MaxLenTest
import qualified MigrationOnlyTest
import qualified PersistentTest
import qualified PersistUniqueTest
import qualified PrimaryTest
import qualified Recursive
import qualified RenameTest
import qualified SumTypeTest
import qualified InsertDuplicateUpdate
import qualified UniqueTest

#ifndef WITH_NOSQL
#  ifdef WITH_SQLITE
import Control.Exception (handle, IOException)
import Filesystem (removeFile)
import Filesystem.Path.CurrentOS (fromText)
import qualified MigrationTest
#  endif
#endif


#ifdef WITH_NOSQL
#else

setup :: MonadIO m => Migration -> ReaderT SqlBackend m ()
setup migration = do
  printMigration migration
  runMigrationUnsafe migration
#endif

main :: IO ()
main = do
#ifndef WITH_NOSQL
#  ifdef WITH_SQLITE
  handle (\(_ :: IOException) -> return ())
    $ removeFile $ fromText sqlite_database_file
#  endif

  runConn $ do
    mapM_ setup
      [ PersistentTest.testMigrate
      , PersistentTest.noPrefixMigrate
      , EmbedTest.embedMigrate
      , InsertDuplicateUpdate.duplicateMigrate
      , EmbedOrderTest.embedOrderMigrate
      , LargeNumberTest.numberMigrate
      , UniqueTest.uniqueMigrate
      , MaxLenTest.maxlenMigrate
      , Recursive.recursiveMigrate
      , CompositeTest.compositeMigrate
#  ifdef WITH_SQLITE
      , MigrationTest.migrationMigrate
#  endif
      , PersistUniqueTest.migration
      , RenameTest.migration
      , CustomPersistFieldTest.customFieldMigrate
#  ifndef WITH_MYSQL
      , PrimaryTest.migration
#  endif
#  ifdef WITH_MYSQL
      , InsertDuplicateUpdate.duplicateMigrate
#  endif
      , CustomPrimaryKeyReferenceTest.migration
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
    CustomPersistFieldTest.specs
    CustomPrimaryKeyReferenceTest.specs
    InsertDuplicateUpdate.specs

#ifdef WITH_SQLITE
    MigrationTest.specs
#endif
