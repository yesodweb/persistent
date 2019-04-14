{-# LANGUAGE OverloadedStrings #-}
module RawMongoHelpers where

import qualified Database.MongoDB as MongoDB
import Database.Persist.MongoDB (toInsertDoc, docToEntityThrow, collectionName, recordToDocument)

import MongoInit
import PersistentTest (cleanDB)
import PersistentTestModels


db :: ReaderT MongoDB.MongoContext IO () -> IO ()
db = db' cleanDB

specs :: Spec
specs = do
  describe "raw MongoDB helpers" $ do
    it "collectionName" $ do
        collectionName (Person "Duder" 0 Nothing) @?= "Person"

    it "toInsertFields, entityFields, & docToEntityThrow" $ db $ do
        let p1 = Person "Duder" 0 Nothing
        let doc = toInsertDoc p1
        MongoDB.ObjId _id <- MongoDB.insert "Person" $ doc
        let idSelector = "_id" MongoDB.=: _id
        Entity _ ent1 <- docToEntityThrow $ idSelector:doc
        liftIO $ p1 @?= ent1

        let p2 = p1 {personColor = Just "blue"}
        let doc2 = idSelector:recordToDocument p2
        MongoDB.save "Person" doc2
        Entity _ ent2 <- docToEntityThrow doc2
        liftIO $ p2 @?= ent2
