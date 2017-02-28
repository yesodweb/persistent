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
import qualified MigrationOnlyTest
import qualified PersistUniqueTest
import qualified CompositeTest
import qualified PrimaryTest
import qualified CustomPersistFieldTest
import qualified CustomPrimaryKeyReferenceTest
import Init

#ifndef WITH_NOSQL
import qualified MigrationTest
import Filesystem (removeFile)
import Filesystem.Path.CurrentOS (fromText)
import Control.Exception (handle, IOException)
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
      , RenameTest.migration
      , CustomPersistFieldTest.customFieldMigrate
#  ifndef WITH_MYSQL
      , PrimaryTest.migration
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

#ifndef WITH_NOSQL
    MigrationTest.specs
#endif
