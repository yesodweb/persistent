{-# LANGUAGE UndecidableInstances, DeriveGeneric #-} -- FIXME

module DerivePersistTest where

import Database.Persist.TH (mkDeleteCascade, derivePersist, persistLowerCase, stripEntityNamePrefix, DeriveEntityDef(..), DeriveFieldDef(..), DeriveForeignKey(..), mkDeriveEntityDef, mkDeriveFieldDef)
import Database.Persist.Quasi (lowerCaseSettings)
import Data.Maybe (isJust)

import Init

data DerivePrimary = DerivePrimary {
  derivePrimaryName :: String,
  derivePrimaryAge :: Int
} deriving (Eq, Show)


derivePersist persistSettings {mpsGeneric=False, mpsRecordFieldToHaskellName = stripEntityNamePrefix} lowerCaseSettings [
  (mkDeriveEntityDef 'DerivePrimary) {
    deriveFields = [(mkDeriveFieldDef 'derivePrimaryName) {
      sqlNameOverride="sql_name"
      }]
  }]

data DeriveSimpleRef = DeriveSimpleRef {
  plainPrimaryRefName :: String,
  plainPrimaryRefRef :: DerivePrimaryId
} deriving (Eq, Show)
derivePersist persistSettings {mpsGeneric=False, mpsRecordFieldToHaskellName = stripEntityNamePrefix} lowerCaseSettings [
  mkDeriveEntityDef 'DeriveSimpleRef
  ]

newtype SubType2 = SubType2 {
  menuObject :: [MenuObject2]
} deriving (Eq, Show)

newtype MenuObject2 = MenuObject2 {
  sub :: Maybe SubType2
} deriving (Eq, Show)

derivePersist persistSettings {mpsGeneric=False, mpsRecordFieldToHaskellName = stripEntityNamePrefix} lowerCaseSettings [
  mkDeriveEntityDef 'SubType2
  ]

derivePersist persistSettings {mpsGeneric=False, mpsRecordFieldToHaskellName = stripEntityNamePrefix} lowerCaseSettings [
  mkDeriveEntityDef 'MenuObject2
  ]

share [mkPersist sqlSettings { mpsGeneric = True }] [persistLowerCase|

SubType3
  object3 [MenuObject3]
  deriving Show Eq

MenuObject3
  sub3 SubType3 Maybe
  deriving Show Eq

|]

data TestParent2 = TestParent2 {
  testParentName ::  String
  , testParentName2 :: String
  , testParentAge :: Int
  , testParentExtra44 :: String
  } deriving (Eq, Show)

data TestChild2 = TestChild2 {
  testChildName :: String
  , testChildName2 :: String
  , testChildAge :: Int
  , testChildExtra4 :: String
} deriving (Eq, Show)

derivePersist persistSettings {mpsGeneric=False, mpsRecordFieldToHaskellName = stripEntityNamePrefix} lowerCaseSettings [
    (mkDeriveEntityDef 'TestParent2) {
      primaryId = Just $ Right ['testParentName, 'testParentName2, 'testParentAge]
    },
    (mkDeriveEntityDef 'TestChild2) {
      foreignKeys = [DeriveForeignKey 'TestParent2 "fkparent" ['testChildName, 'testChildName2, 'testChildAge] Nothing]
    }
  ]

migration :: Migration
migration = do
  migrate [] (entityDef (Proxy :: Proxy DerivePrimary))
  migrate [] (entityDef (Proxy :: Proxy DeriveSimpleRef))
  migrate [] (entityDef (Proxy :: Proxy SubType2))
  migrate [] (entityDef (Proxy :: Proxy MenuObject2))
  migrate [] (entityDef (Proxy :: Proxy TestParent2))
  migrate [] (entityDef (Proxy :: Proxy TestChild2))

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "derivePersist" $ do
  it "Supports basic operations" $ runDb $ do
    let p1 = DerivePrimary "a" 1
    k <- insert p1
    p1' <- get k
    Just p1 @== p1'
  
  it "Creates fields" $ runDb $ do
    let p1 = DerivePrimary "a" 1
    k <- insert p1
    p1' <- selectFirst [DerivePrimaryName ==. "a"] []
    Just (Entity k p1) @== p1'

  it "Supports primary key" $ runDb $ do
    let p1 = DerivePrimary "a" 1
    k <- insert p1
    let p = DeriveSimpleRef "a" k
    k' <- insert p
    p' <- get k'
    Just p @== p'

  it "Supports recursive" $ runDb $ do
    let m1 = MenuObject2 $ Just $ SubType2 []
    let m2 = MenuObject2 $ Just $ SubType2 [m1]
    let m3 = MenuObject2 $ Just $ SubType2 [m2]
    k3 <- insert m3
    m3' <- get k3
    m3' @== Just m3
    res <- rawSql "SELECT * FROM menu_object" []
    liftIO $ print (res :: [(Single Text, Single Text)])
  
  it "Supports composite keys" $ runDb $ do
    -- copy from CompositeTest
    let p1 = TestParent2 "a1" "b1" 11 "p1"
    let p2 = TestParent2 "a2" "b2" 22 "p2"
    let c1 = TestChild2 "a1" "b1" 11 "c1"
    kp1 <- insert p1
    insert_ p2
    kc1 <- insert c1
    mc <- get kc1
    isJust mc @== True
    let Just c11 = mc
    c1 @== c11
    testChild2Fkparent c11 @== kp1
