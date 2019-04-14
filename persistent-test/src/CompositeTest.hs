{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module CompositeTest where

import qualified Data.Map as Map
import Data.Maybe (isJust)

import Database.Persist.TH (mkDeleteCascade)
import Init


-- mpsGeneric = False is due to a bug or at least lack of a feature in mkKeyTypeDec TH.hs
share [mkPersist persistSettings { mpsGeneric = False }, mkMigrate "compositeMigrate", mkDeleteCascade persistSettings { mpsGeneric = False }] [persistLowerCase|
  TestParent
      name  String maxlen=20
      name2 String maxlen=20
      age Int
      extra44 String
      Primary name name2 age
      deriving Show Eq
  TestChild
      name  String maxlen=20
      name2 String maxlen=20
      age Int
      extra4 String
      Foreign TestParent fkparent name name2 age
      deriving Show Eq

  Citizen
    name String
    age Int Maybe
    deriving Eq Show
  Address
    address String
    country String
    deriving Eq Show
  CitizenAddress
    citizen CitizenId
    address AddressId
    Primary citizen address
    deriving Eq Show

  PrimaryCompositeWithOtherNullableFields
    foo String       maxlen=20
    bar String       maxlen=20
    baz String Maybe
    Primary foo bar
    deriving Eq Show
|]

cleanDB :: (PersistQuery backend, PersistEntityBackend TestChild ~ backend, MonadIO m) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter TestChild])
  deleteWhere ([] :: [Filter TestParent])
  deleteWhere ([] :: [Filter CitizenAddress])
  deleteWhere ([] :: [Filter Citizen])
  deleteWhere ([] :: [Filter Address])

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "composite" $
  describe "primary keys" $ do

    let p1 = TestParent "a1" "b1" 11 "p1"
    let p2 = TestParent "a2" "b2" 22 "p2"
    let p3 = TestParent "a3" "b3" 33 "p3"
    let p1' = TestParent "a1" "b1" 11 "p1'"
    let c1 = TestChild "a1" "b1" 11 "c1"
    let c1' = TestChild "a1" "b1" 11 "c1'"

    it "insertWithKey" $ runDb $ do
      kp1 <- insert p1
      delete kp1
      insertKey kp1 p2

    it "repsert" $ runDb $ do
      kp1 <- insert p1
      repsert kp1 p2

    it "Insert" $ runDb $ do
      kp1 <- insert p1
      matchParentK kp1 @== Right ("a1","b1",11)
      mp <- get kp1
      isJust mp @== True
      let Just p11 = mp
      p1 @== p11
      xs <- selectList [TestParentId ==. kp1] []
      length xs @== 1
      let [Entity newkp1 newp1] = xs
      matchParentK kp1 @== matchParentK newkp1
      p1 @== newp1

    it "Id field" $ runDb $ do
      kp1 <- insert p1
      kp2 <- insert p2
      xs <- selectList [TestParentId <-. [kp1,kp2]] []
      length xs @== 2
      [(Entity newkp1 newp1),(Entity newkp2 newp2)] <- pure xs
      matchParentK kp1 @== matchParentK newkp1
      matchParentK kp2 @== matchParentK newkp2
      p1 @== newp1
      p2 @== newp2

    it "Filter by Id with 'not equal'" $ runDb $ do
      kp1 <- insert p1
      kp2 <- insert p2
      xs <- selectList [TestParentId !=. kp1] []
      length xs @== 1
      let [Entity newkp2 _newp2] = xs
      matchParentK kp2 @== matchParentK newkp2

    it "Filter by Id with 'in'" $ runDb $ do
      kp1 <- insert p1
      kp2 <- insert p2
      xs <- selectList [TestParentId <-. [kp1,kp2]] []
      length xs @== 2
      let [Entity newkp1 _newp1,Entity newkp2 _newp2] = xs
      matchParentK kp1 @== matchParentK newkp1
      matchParentK kp2 @== matchParentK newkp2

    it "Filter by Id with 'not in'" $ runDb $ do
      kp1 <- insert p1
      kp2 <- insert p2
      xs <- selectList [TestParentId /<-. [kp1]] []
      length xs @== 1
      let [Entity newkp2 _newp2] = xs
      matchParentK kp2 @== matchParentK newkp2

    it "Filter by Id with 'not in' with no data" $ runDb $ do
      kp1 <- insert p1
      kp2 <- insert p2
      xs <- selectList [TestParentId /<-. [kp1,kp2]] []
      length xs @== 0

    it "Extract Parent Foreign Key from Child value" $ runDb $ do
      kp1 <- insert p1
      _ <- insert p2
      kc1 <- insert c1
      mc <- get kc1
      isJust mc @== True
      let Just c11 = mc
      c1 @== c11
      testChildFkparent c11 @== kp1

    it "Validate Key contents" $ runDb $ do
      _ <- insert p1
      _ <- insert p2
      _ <- insert p3
      xs <- selectKeysList [] [Asc TestParentName]
      length xs @== 3
      let [kps1,kps2,kps3] = xs
      matchParentK kps1 @== Right ("a1","b1",11)
      matchParentK kps2 @== Right ("a2","b2",22)
      matchParentK kps3 @== Right ("a3","b3",33)

    it "Delete" $ runDb $ do
      kp1 <- insert p1
      kp2 <- insert p2

      _ <- delete kp1
      r <- get kp1
      r @== Nothing
      r1 <- get kp2
      isJust r1 @== True

    it "Update" $ runDb $ do
      kp1 <- insert p1
      _ <- update kp1 [TestParentExtra44 =. "q1"]
      newkps1 <- get kp1
      newkps1 @== Just (TestParent "a1" "b1" 11 "q1")

    it "Replace Parent" $ runDb $ do
      kp1 <- insert p1
      _ <- replace kp1 p1'
      newp1 <- get kp1
      newp1 @== Just p1'

    it "Replace Child" $ runDb $ do
      -- c1 FKs p1
      _ <- insert p1
      kc1 <- insert c1
      _ <- replace kc1 c1'
      newc1 <- get kc1
      newc1 @== Just c1'

    it "Insert Many to Many" $ runDb $ do
      let z1 = Citizen "mk" (Just 11)
      let a1 = Address "abc" "usa"
      let z2 = Citizen "gb" (Just 22)
      let a2 = Address "def" "den"

      kc1 <- insert z1
      ka1 <- insert a1
      let ca1 = CitizenAddress kc1 ka1
      kca1 <- insert ca1
      matchCitizenAddressK kca1 @== matchK2 kc1 ka1

      mca <- get kca1
      isJust mca @== True
      let Just newca1 = mca
      ca1 @== newca1

      kc2 <- insert z2
      ka2 <- insert a2
      let ca2 = CitizenAddress kc2 ka2
      kca2 <- insert ca2
      matchCitizenAddressK kca2 @== matchK2 kc2 ka2

      xs <- selectList [CitizenAddressId ==. kca1] []
      length xs @== 1
      let [Entity newkca1 newca2] = xs
      matchCitizenAddressK kca1 @== matchCitizenAddressK newkca1
      ca1 @== newca2
    it "insertMany" $ runDb $ do
      [kp1, kp2] <- insertMany [p1, p2]
      rs <- getMany [kp1, kp2]
      rs @== Map.fromList [(kp1, p1), (kp2, p2)]
    it "RawSql Key instance" $ runDb $ do
      key <- insert p1
      keyFromRaw <- rawSql "SELECT name, name2, age FROM test_parent LIMIT 1" []
      [key] @== keyFromRaw

-- TODO: push into persistent-qq test suite
--     it "RawSql Key instance with sqlQQ" $ runDb $ do
--       key <- insert p1
--       keyFromRaw' <- [sqlQQ|
--           SELECT @{TestParentName}, @{TestParentName2}, @{TestParentAge}
--             FROM ^{TestParent}
--             LIMIT 1
--       |]
--       [key] @== keyFromRaw'

    it "RawSql Entity instance" $ runDb $ do
      key <- insert p1
      newp1 <- rawSql "SELECT ?? FROM test_parent LIMIT 1" []
      [Entity key p1] @== newp1

-- TODO: put into persistent-qq test suite
--     it "RawSql Entity instance with sqlQQ" $ runDb $ do
--       key <- insert p1
--       newp1' <- [sqlQQ| SELECT ?? FROM ^{TestParent} |]
--       [Entity key p1] @== newp1'

matchK :: (PersistField a, PersistEntity record) => Key record -> Either Text a
matchK = (\(pv:[]) -> fromPersistValue pv) . keyToValues

matchK2 :: (PersistField a1, PersistField a, PersistEntity record, PersistEntity record2)
        => Key record -> Key record2
        -> Either Text (a1, a)
matchK2 k1 k2 = (,) <$> matchK k1 <*> matchK k2

matchParentK :: Key TestParent -> Either Text (String, String, Int64)
matchParentK = (\(a:b:c:[]) -> (,,) <$> fromPersistValue a <*> fromPersistValue b <*> fromPersistValue c)
             . keyToValues

matchCitizenAddressK :: Key CitizenAddress -> Either Text (Int64, Int64)
matchCitizenAddressK = (\(a:b:[]) -> (,) <$> fromPersistValue a <*> fromPersistValue b)
                     . keyToValues
