{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module RenameTest where

#ifndef WITH_NOSQL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource (runResourceT)
#endif
import Data.Time (getCurrentTime, Day, UTCTime(..))
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Aeson.Compat
import Init

-- persistent used to not allow types with an "Id" suffix
-- this verifies that the issue is fixed
type TextId = Text

-- Test lower case names
#if WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings, mkMigrate "migration"] [persistLowerCase|
#endif
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
|]
#if WITH_NOSQL
cleanDB :: ReaderT Context IO ()
cleanDB = do
  deleteWhere ([] :: [Filter IdTable])
  deleteWhere ([] :: [Filter LowerCaseTable])
  deleteWhere ([] :: [Filter RefTable])
db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = describe "rename specs" $ do
#ifndef WITH_NOSQL
    it "handles lower casing" $ asIO $
        runConn $ do
            runResourceT $ rawQuery "SELECT full_name from lower_case_table WHERE my_id=5" [] C.$$ CL.sinkNull
            runResourceT $ rawQuery "SELECT something_else from ref_table WHERE id=4" [] C.$$ CL.sinkNull
#endif

    it "user specified id, insertKey, no default=" $ db $ do
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

#ifndef WITH_MYSQL
#  ifndef WITH_NOSQL
    -- this uses default=
    it "user specified id, default=" $ db $ do
      let rec = IdTable "Foo" Nothing
      k <- insert rec
      Just rec' <- get k
      rec' @== rec
#  endif
#endif

    it "extra blocks" $
        entityExtra (entityDef (Nothing :: Maybe LowerCaseTable)) @?=
            Map.fromList
                [ ("ExtraBlock", map T.words ["foo bar", "baz", "bin"])
                , ("ExtraBlock2", map T.words ["something"])
                ]

asIO :: IO a -> IO a
asIO = id
