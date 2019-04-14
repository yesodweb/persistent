{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -O0 #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module EmbedTest (specsWith, cleanDB, embedMigrate) where

import Control.Exception (Exception, throw)
import Data.List.NonEmpty hiding (insert, length)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Set as S

import EntityEmbedTest
import Init

data TestException = TestException
    deriving (Show, Typeable, Eq)
instance Exception TestException

instance PersistFieldSql a => PersistFieldSql (NonEmpty a) where
    sqlType _ = SqlString

instance PersistField a => PersistField (NonEmpty a) where
    toPersistValue = toPersistValue . toList
    fromPersistValue pv = do
        xs <- fromPersistValue pv
        case xs of
            [] -> Left "PersistField: NonEmpty found unexpected Empty List"
            (l:ls) -> Right (l:|ls)


share [mkPersist sqlSettings { mpsGeneric = True },  mkMigrate "embedMigrate"] [persistUpperCase|

  OnlyName
    name Text
    deriving Show Eq Read Ord

  HasEmbed
    name Text
    embed OnlyName
    deriving Show Eq Read Ord

  HasEmbeds
    name Text
    embed OnlyName
    double HasEmbed
    deriving Show Eq Read Ord

  HasListEmbed
    name Text
    list [HasEmbed]
    deriving Show Eq Read Ord

  HasSetEmbed
    name Text
    set (S.Set HasEmbed)
    deriving Show Eq Read Ord

  HasMap
    name Text
    map (M.Map T.Text T.Text)
    deriving Show Eq Read Ord

  HasList
    list [HasListId]
    deriving Show Eq Read Ord

  EmbedsHasMap
    name Text Maybe
    embed HasMap
    deriving Show Eq Read Ord

  InList
    one Int
    two Int
    deriving Show Eq

  ListEmbed
    nested [InList]
    one Int
    two Int
    deriving Show Eq

  User
    ident Text
    password Text Maybe
    profile Profile
    deriving Show Eq Read Ord

  Profile
    firstName Text
    lastName Text
    contact Contact Maybe
    deriving Show Eq Read Ord

  Contact
    phone Int
    email T.Text
    deriving Show Eq Read Ord

  Account
    userIds       (NonEmpty (Key User))
    name          Text Maybe
    customDomains [Text]             -- we may want to allow multiple cust domains.  use [] instead of maybe

    deriving Show Eq Read Ord

  HasNestedList
    list [IntList]
    deriving Show Eq

  IntList
    ints [Int]
    deriving Show Eq

  -- We would like to be able to use OnlyNameId
  -- But (Key OnlyName) works
  MapIdValue
    map (M.Map T.Text (Key OnlyName))
    deriving Show Eq Read Ord

  HasArrayWithEntities
    hasEntity (Entity ARecord)
    arrayWithEntities [AnEntity]
    deriving Show Eq Read Ord

  -- Self refrences are only allowed as a nullable type:
  -- a Maybe or a List
  SelfList
    reference [SelfList]

  SelfMaybe
    reference SelfMaybe Maybe

  -- This failes
  -- SelfDirect
  --  reference SelfDirect
|]
cleanDB :: (PersistQuery backend, PersistEntityBackend HasMap ~ backend, MonadIO m) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter (HasEmbedGeneric backend)])
  deleteWhere ([] :: [Filter (HasEmbedsGeneric backend)])
  deleteWhere ([] :: [Filter (HasListEmbedGeneric backend)])
  deleteWhere ([] :: [Filter (HasSetEmbedGeneric backend)])
  deleteWhere ([] :: [Filter (UserGeneric backend)])
  deleteWhere ([] :: [Filter (HasMapGeneric backend)])
  deleteWhere ([] :: [Filter (HasListGeneric backend)])
  deleteWhere ([] :: [Filter (EmbedsHasMapGeneric backend)])
  deleteWhere ([] :: [Filter (ListEmbedGeneric backend)])
  deleteWhere ([] :: [Filter (ARecordGeneric backend)])
  deleteWhere ([] :: [Filter (AccountGeneric backend)])
  deleteWhere ([] :: [Filter (HasNestedListGeneric backend)])

_unlessM :: MonadIO m => IO Bool -> m () -> m ()
_unlessM predicate body = do
    b <- liftIO predicate
    unless b body

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDb = describe "embedded entities" $ do

  it "simple entities" $ runDb $ do
      let container = HasEmbeds "container" (OnlyName "2")
            (HasEmbed "embed" (OnlyName "1"))
      contK <- insert container
      Just res <- selectFirst [HasEmbedsName ==. "container"] []
      res @== Entity contK container

  it "query for equality of embeded entity" $ runDb $ do
      let container = HasEmbed "container" (OnlyName "2")
      contK <- insert container
      Just res <- selectFirst [HasEmbedEmbed ==. OnlyName "2"] []
      res @== Entity contK container

  it "Set" $ runDb $ do
      let container = HasSetEmbed "set" $ S.fromList
            [ HasEmbed "embed" (OnlyName "1")
            , HasEmbed "embed" (OnlyName "2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasSetEmbedName ==. "set"] []
      res @== Entity contK container

  it "Set empty" $ runDb $ do
      let container = HasSetEmbed "set empty" $ S.fromList []
      contK <- insert container
      Just res <- selectFirst [HasSetEmbedName ==. "set empty"] []
      res @== Entity contK container

  it "exception" $ flip shouldThrow (== TestException) $ runDb $ do
      let container = HasSetEmbed "set" $ S.fromList
            [ HasEmbed "embed" (OnlyName "1")
            , HasEmbed "embed" (OnlyName "2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasSetEmbedName ==. throw TestException] []
      res @== Entity contK container

  it "ListEmbed" $ runDb $ do
      let container = HasListEmbed "list"
            [ HasEmbed "embed" (OnlyName "1")
            , HasEmbed "embed" (OnlyName "2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasListEmbedName ==. "list"] []
      res @== Entity contK container

  it "ListEmbed empty" $ runDb $ do
      let container = HasListEmbed "list empty" []
      contK <- insert container
      Just res <- selectFirst [HasListEmbedName ==. "list empty"] []
      res @== Entity contK container

  it "List empty" $ runDb $ do
      let container = HasList []
      contK <- insert container
      Just res <- selectFirst [] []
      res @== Entity contK container

  it "NonEmpty List wrapper" $ runDb $ do
      let con = Contact 123456 "foo@bar.com"
      let prof = Profile "fstN" "lstN" (Just con)
      uid <- insert $ User "foo" (Just "pswd") prof
      let container = Account (uid:|[]) (Just "Account") []
      contK <- insert container
      Just res <- selectFirst [AccountUserIds ==. (uid:|[])] []
      res @== Entity contK container

  it "Map" $ runDb $ do
      let container = HasMap "2 items" $ M.fromList [
              ("k1","v1")
            , ("k2","v2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasMapName ==. "2 items"] []
      res @== Entity contK container

  it "Map empty" $ runDb $ do
      let container = HasMap "empty" $ M.fromList []
      contK <- insert container
      Just res <- selectFirst [HasMapName ==. "empty"] []
      res @== Entity contK container

  it "Embeds a Map" $ runDb $ do
      let container = EmbedsHasMap (Just "non-empty map") $ HasMap "2 items" $ M.fromList [
              ("k1","v1")
            , ("k2","v2")
            ]
      contK <- insert container
      Just res <- selectFirst [EmbedsHasMapName ==. Just "non-empty map"] []
      res @== Entity contK container

  it "Embeds a Map empty" $ runDb $ do
      let container = EmbedsHasMap (Just "empty map") $ HasMap "empty" $ M.fromList []
      contK <- insert container
      Just res <- selectFirst [EmbedsHasMapName ==. Just "empty map"] []
      res @== Entity contK container

  it "Embeds a Map with ids as values" $ runDb $ do
      onId <- insert $ OnlyName "nombre"
      onId2 <- insert $ OnlyName "nombre2"
      let midValue = MapIdValue $ M.fromList [("foo", onId),("bar",onId2)]
      mK <- insert midValue
      Just mv <- get mK
      mv @== midValue
