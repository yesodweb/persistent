{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module CompositeTest where

import Test.Hspec.Expectations ()


#  if MIN_VERSION_monad_control(0, 3, 0)
import qualified Control.Monad.Trans.Control
#  else
import qualified Control.Monad.IO.Control
#  endif

import Database.Persist.TH (mkDeleteCascade)
#  if MIN_VERSION_monad_control(0, 3, 0)
import qualified Control.Exception as E
#    define CATCH catch'
#  else
import qualified Control.Exception.Control as Control
#    define CATCH Control.catch
#  endif

import Init
#ifndef WITH_MONGODB
import Database.Persist
import Data.Maybe (isJust)

import Control.Applicative ((<$>),(<*>))
#endif

share [mkPersist sqlSettings,  mkMigrate "compositeMigrate", mkDeleteCascade sqlSettings] [persistLowerCase|
  TestChild
      name1 String maxlen=20
      name2 String maxlen=20
      age3 Int
      extra4 String
      Foreign TestParent fkparent name1 name2 age3
      deriving Show Eq
  TestParent
      name11 String maxlen=20
      name22 String maxlen=20
      age33 Int
      extra44 String
      Primary name11 name22 age33
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
|]


#ifdef WITH_MONGODB
#else
cleanDB :: (PersistQuery m, PersistEntityBackend TestChild ~ PersistMonadBackend m) => m ()
cleanDB = do
  deleteWhere ([] :: [Filter TestChild])
  deleteWhere ([] :: [Filter TestParent])
  deleteWhere ([] :: [Filter CitizenAddress])
  deleteWhere ([] :: [Filter Citizen])
  deleteWhere ([] :: [Filter Address])

db :: Action IO () -> Assertion
db = db' cleanDB

specs :: Spec
specs = describe "composite" $
  describe "primary keys" $ do

    let p1 = TestParent "a1" "b1" 11 "p1"
    let p2 = TestParent "a2" "b2" 22 "p2"
    let p3 = TestParent "a3" "b3" 33 "p3"
    let c1 = TestChild "a1" "b1" 11 "c1"

    it "Insert" $ db $ do
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
    
    it "Id field" $ db $ do
      kp1 <- insert p1
      kp2 <- insert p2
      xs <- selectList [TestParentId <-. [kp1,kp2]] []
      length xs @== 2
      let [e1@(Entity newkp1 newp1),e2@(Entity newkp2 newp2)] = xs 
      matchParentK kp1 @== matchParentK newkp1
      matchParentK kp2 @== matchParentK newkp2
      p1 @== newp1
      p2 @== newp2

    it "Filter by Id with 'not equal'" $ db $ do
      kp1 <- insert p1
      kp2 <- insert p2
      xs <- selectList [TestParentId !=. kp1] []
      length xs @== 1
      let [Entity newkp2 newp2] = xs 
      matchParentK kp2 @== matchParentK newkp2
  
    it "Filter by Id with 'in'" $ db $ do
      kp1 <- insert p1
      kp2 <- insert p2
      xs <- selectList [TestParentId <-. [kp1,kp2]] []
      length xs @== 2
      let [Entity newkp1 newp1,Entity newkp2 newp2] = xs 
      matchParentK kp1 @== matchParentK newkp1
      matchParentK kp2 @== matchParentK newkp2

    it "Filter by Id with 'not in'" $ db $ do
      kp1 <- insert p1
      kp2 <- insert p2
      xs <- selectList [TestParentId /<-. [kp1]] []
      length xs @== 1
      let [Entity newkp2 newp2] = xs 
      matchParentK kp2 @== matchParentK newkp2
  
    it "Filter by Id with 'not in' with no data" $ db $ do
      kp1 <- insert p1
      kp2 <- insert p2
      xs <- selectList [TestParentId /<-. [kp1,kp2]] []
      length xs @== 0

    it "Extract Parent Foreign Key from Child value" $ db $ do
      kp1 <- insert p1
      _ <- insert p2
      kc1 <- insert c1
      mc <- get kc1
      isJust mc @== True
      let Just c11 = mc
      c1 @== c11
      testChildFkparent c11 @== kp1
        
    it "Validate Key contents" $ db $ do
      _ <- insert p1
      _ <- insert p2
      _ <- insert p3
      xs <- selectKeysList [] [Asc TestParentName11] 
      length xs @== 3
      let [kps1,kps2,kps3] = xs
      matchParentK kps1 @== Right ("a1","b1",11)
      matchParentK kps2 @== Right ("a2","b2",22)
      matchParentK kps3 @== Right ("a3","b3",33)

    it "Delete" $ db $ do
      kp1 <- insert p1
      kp2 <- insert p2

      _ <- delete kp1
      r <- get kp1
      r @== Nothing
      r1 <- get kp2
      isJust r1 @== True

    it "Update" $ db $ do
      kp1 <- insert p1
      _ <- update kp1 [TestParentExtra44 =. "q1"] 
      newkps1 <- get kp1
      newkps1 @== Just (TestParent "a1" "b1" 11 "q1")
    
    it "Insert Many to Many" $ db $ do
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
      let [Entity newkca1 newca1] = xs
      matchCitizenAddressK kca1 @== matchCitizenAddressK newkca1
      ca1 @== newca1

matchK :: PersistField a => KeyBackend backend entity -> Either Text a
matchK = fromPersistValue . unKey

matchK2 :: (PersistField a1, PersistField a) =>
                 KeyBackend backend entity
                 -> KeyBackend backend1 entity1 -> Either Text (a1, a)
matchK2 k1 k2 = (,) <$> matchK k1 <*> matchK k2

matchParentK :: Key TestParent -> Either Text (String, String, Int64)
matchParentK (Key (PersistList [a, b, c]))  = (,,) <$> fromPersistValue a <*> fromPersistValue b <*> fromPersistValue c
matchParentK k = error $ "matchParentK: unexpected value for key " ++ show k

matchCitizenAddressK :: Key CitizenAddress -> Either Text (Int64, Int64)
matchCitizenAddressK (Key (PersistList [a, b]))  = (,) <$> fromPersistValue a <*> fromPersistValue b
matchCitizenAddressK k = error $ "matchCitizenAddressK: unexpected value for key " ++ show k
#endif

#if MIN_VERSION_monad_control(0, 3, 0)
catch' :: (Control.Monad.Trans.Control.MonadBaseControl IO m, E.Exception e)
       => m a       -- ^ The computation to run
       -> (e -> m a) -- ^ Handler to invoke if an exception is raised
       -> m a
catch' a handler = Control.Monad.Trans.Control.control $ \runInIO ->
                    E.catch (runInIO a)
                            (\e -> runInIO $ handler e)
#endif


