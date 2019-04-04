{-# LANGUAGE CPP #-}

{-# LANGUAGE ScopedTypeVariables #-}

import InitMongo

-- Modules that were commented out were effectively noops for MongoDB.
import qualified CompositeTest
import qualified CustomPersistFieldTest
import qualified CustomPrimaryKeyReferenceTest
import qualified DataTypeTestMongo as DataTypeTest
import qualified EmbedOrderTestMongo
import qualified EmbedTestMongo
import qualified EmptyEntityTest
import qualified HtmlTestMongo
import qualified LargeNumberTestMongo
import qualified MaxLenTestMongo
import qualified MigrationOnlyTest
import qualified PersistentTest
import qualified PersistUniqueTest
import qualified PrimaryTest
import qualified RecursiveMongo
import qualified RenameTest
import qualified SumTypeTestMongo
-- import qualified UniqueTestMongo
import qualified MigrationColumnLengthTest
import qualified TransactionLevelTest

main :: IO ()
main = do

  hspec $ do
    RenameTest.specs
    DataTypeTest.specs
    HtmlTestMongo.specs
    EmbedTestMongo.specs
    EmbedOrderTestMongo.specs
    LargeNumberTestMongo.specs
    -- UniqueTestMongo.specs
    MaxLenTestMongo.specs
    RecursiveMongo.specs
    SumTypeTestMongo.specs
    MigrationOnlyTest.specs
    PersistentTest.specs
    EmptyEntityTest.specs
    CompositeTest.specs
    PersistUniqueTest.specs
    PrimaryTest.specs
    CustomPersistFieldTest.specs
    CustomPrimaryKeyReferenceTest.specs
