{-# LANGUAGE UndecidableInstances #-}
module RenameTest where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (getCurrentTime, Day, UTCTime(..))

import Init

-- persistent used to not allow types with an "Id" suffix
-- this verifies that the issue is fixed
type TextId = Text

-- Test lower case names
share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migration"] [persistLowerCase|
-- This just tests that a field can be named "key"
KeyTable
    key Text
    deriving Eq Show

IdTable
    Id   Day default=CURRENT_DATE
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

cleanDB
    :: forall backend.
    ( BaseBackend backend ~ backend
    , PersistQueryWrite backend
    )
    => ReaderT backend IO ()
cleanDB = do
  deleteWhere ([] :: [Filter (IdTableGeneric backend)])
  deleteWhere ([] :: [Filter (LowerCaseTableGeneric backend)])
  deleteWhere ([] :: [Filter (RefTableGeneric backend)])

specsWith
    ::
    ( PersistStoreWrite backend, PersistQueryRead backend
    , backend ~ BaseBackend backend
    , MonadIO m, MonadFail m
    , Eq (BackendKey backend)
    )
    => RunDb backend m
    -> Spec
specsWith runDb = describe "rename specs" $ do
    it "user specified id, insertKey, no default=" $ runDb $ do
      let rec2 = IdTable "Foo2" Nothing
      let rec1 = IdTable "Foo1" $ Just rec2
      let rec  = IdTable "Foo" $ Just rec1
      now <- liftIO getCurrentTime
      let key = IdTableKey $ utctDay now
      insertKey key rec
      Just rec' <- get key
      rec' @== rec
      (Entity key' _):_ <- selectList ([] :: [Filter (IdTableGeneric backend)]) []
      key' @== key

    it "extra blocks" $
        entityExtra (entityDef (Nothing :: Maybe LowerCaseTable)) @?=
            Map.fromList
                [ ("ExtraBlock", map T.words ["foo bar", "baz", "bin"])
                , ("ExtraBlock2", map T.words ["something"])
                ]
