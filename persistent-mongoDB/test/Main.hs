{-# LANGUAGE CPP #-}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified CompositeTest
import qualified CustomPersistFieldTest
import qualified CustomPrimaryKeyReferenceTest
import qualified DataTypeTestMongo as DataTypeTest
import qualified EmbedOrderTestMongo
import qualified EmbedTestMongo
import qualified EmptyEntityTest
import qualified HtmlTestMongo
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
import qualified UniqueTest
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
