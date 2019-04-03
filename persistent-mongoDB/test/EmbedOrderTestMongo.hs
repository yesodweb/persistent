{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls, MultiParamTypeClasses #-}
module EmbedOrderTestMongo (specs) where

import InitMongo
import Data.Map hiding (insert)

import Debug.Trace (trace)
debug :: Show s => s -> s
debug x = trace (show x) x

mkPersist persistSettings [persistUpperCase|
Foo sql=foo_embed_order
    bars [Bar]
    deriving Eq Show
Bar sql=bar_embed_order
    b String
    u String
    g String
    deriving Eq Show
|]

cleanDB :: (PersistQuery backend, PersistEntityBackend Foo ~ backend, MonadIO m) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Foo])
  deleteWhere ([] :: [Filter Bar])

db :: Action IO () -> Assertion
db = db' cleanDB

specs :: Spec
specs = describe "embedded entities" $ do
  it "preserves ordering" $ db $ do
    let foo = Foo [Bar "b" "u" "g"]
    fooId <- insert foo
    Just otherFoo <- get fooId
    foo @== otherFoo

  it "PersistMap PersistValue serializaion" $ db $ do
    let record = fromList [("b","b"),("u","u"),("g","g")] :: Map Text Text
    record @== (fromRight . fromPersistValue . toPersistValue) record

fromRight :: Show a => Either a b -> b
fromRight (Left e) = error $ "expected Right, got Left " ++ show e
fromRight (Right x) = x
