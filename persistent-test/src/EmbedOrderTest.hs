{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE CPP, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls, MultiParamTypeClasses #-}
module EmbedOrderTest (specs,
#ifndef WITH_NOSQL
embedOrderMigrate
#endif
) where

import Init
import Data.Map hiding (insert)
import Control.Monad.Trans.Control (MonadBaseControl)
import Debug.Trace (trace)

debug :: Show s => s -> s
debug x = trace (show x) x

#if WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings, mkMigrate "embedOrderMigrate"] [persistUpperCase|
#endif
Foo sql=foo_embed_order
    bars [Bar]
    deriving Eq Show
Bar sql=bar_embed_order
    b String
    u String
    g String
    deriving Eq Show
|]

#ifdef WITH_NOSQL
cleanDB :: (PersistQuery backend, MonadBaseControl IO m, PersistEntityBackend Foo ~ backend, MonadIO m) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Foo])
  deleteWhere ([] :: [Filter Bar])

db :: Action IO () -> Assertion
db = db' cleanDB
#endif

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

    -- this demonstrates a change in ordering
    -- that won't be a problem if the keys are properly tracked
    {-
    let precord = PersistMap [("b",PersistText "b"),("u",PersistText "u"),("g",PersistText "g")]
    precord ==@ (debug . toPersistValue . debug . (fromRight . fromPersistValue :: PersistValue -> Map Text Text)) precord

    let precord = PersistMap [("b",PersistText "b"),("u",PersistText "u"),("g",PersistText "g")]
    precord ==@ (fromSuccess . fromJSON . debug . (toJSON :: PersistValue -> Value)) precord


fromSuccess :: Result a -> a
fromSuccess (Success s) = s
fromSuccess (Error e) = error $ "expected Success, got Error " ++ e
    -}

fromRight :: Show a => Either a b -> b
fromRight (Left e) = error $ "expected Right, got Left " ++ show e
fromRight (Right x) = x
