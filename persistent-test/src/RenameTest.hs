{-# LANGUAGE TypeApplications, UndecidableInstances #-}

module RenameTest where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (getCurrentTime, Day, UTCTime(..))

import Init

-- persistent used to not allow types with an "Id" suffix
-- this verifies that the issue is fixed
type TextId = Text

-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "migration"] [persistLowerCase|
-- This just tests that a field can be named "key"
KeyTable
    key Text
    deriving Eq Show

IdTable
    -- this used to have a default=CURRENT_DATE, but the test that uses it
    -- specifies that there is no default on this column. the default is
    -- failing MySQL and sqlite tests since they don't have shared overlap on
    -- an appropriate default for a date.
    Id   Day
    name Text
    -- This was added to test the ability to break a cycle
    -- getting rid of the Maybe should be a compilation failure
    keyTableEmbed IdTable Maybe
    deriving Eq Show

LowerCaseTable
    Id            sql=my_id
    fullName Text
    ExtraBlock
        foo bar
        baz
        bin
    ExtraBlock2
        something

RefTable
    someVal Int sql=something_else
    lct LowerCaseTableId
    text TextId
    UniqueRefTable someVal

-- Test a reference to a non-int Id
ForeignIdTable
    idId IdTableId
|]

cleanDB :: SqlPersistT IO ()
cleanDB = do
  deleteWhere ([] :: [Filter IdTable])
  deleteWhere ([] :: [Filter LowerCaseTable])
  deleteWhere ([] :: [Filter RefTable])

specsWith :: ( MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "rename specs" $ do
    describe "LowerCaseTable" $ do
        it "LowerCaseTable has the right sql name" $ do
            fmap fieldDB (getEntityIdField (entityDef (Proxy @LowerCaseTable)))
                `shouldBe`
                    Just (FieldNameDB "my_id")

    it "user specified id, insertKey, no default=" $ runDb $ do
        let rec2 = IdTable "Foo2" Nothing
        let rec1 = IdTable "Foo1" $ Just rec2
        let rec  = IdTable "Foo" $ Just rec1
        now <- liftIO getCurrentTime
        let key = IdTableKey $ utctDay now
        insertKey key rec
        Just rec' <- get key
        rec' @== rec
        (Entity key' _):_ <- selectList ([] :: [Filter IdTable]) []
        key' @== key

    it "extra blocks" $
        getEntityExtra (entityDef (Nothing :: Maybe LowerCaseTable)) @?=
            Map.fromList
                [ ("ExtraBlock", map T.words ["foo bar", "baz", "bin"])
                , ("ExtraBlock2", map T.words ["something"])
                ]
