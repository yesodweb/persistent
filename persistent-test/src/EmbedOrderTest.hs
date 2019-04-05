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
import qualified Data.Map as Map

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
cleanDB :: (PersistQuery backend, PersistEntityBackend Foo ~ backend, MonadIO m) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Foo])
  deleteWhere ([] :: [Filter Bar])

db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = specsWith db Foo Bar

specsWith
    ::
    ( PersistEntityBackend foo ~ BaseBackend backend
    , PersistEntityBackend bar ~ BaseBackend backend
    , PersistStoreRead backend, PersistStoreWrite backend
    , Show foo, Eq foo
    , MonadFail m
    , MonadIO m
    , Show bar, Eq bar
    , PersistEntity foo, PersistEntity bar
    )
    => (ReaderT backend m () -> IO ())
    -> ([bar] -> foo)
    -> (String -> String -> String -> bar)
    -> Spec
specsWith db mkFoo mkBar = describe "embedded entities" $ do
    it "preserves ordering" $ db $ do
        let foo = mkFoo [mkBar "b" "u" "g"]
        fooId <- insert foo
        Just otherFoo <- get fooId
        foo @== otherFoo

    it "PersistMap PersistValue serializaion" $ db $ do
        let record = Map.fromList [("b" :: Text,"b" :: Text),("u","u"),("g","g")]
        record @== (fromRight . fromPersistValue . toPersistValue) record

fromRight :: Show a => Either a b -> b
fromRight (Left e) = error $ "expected Right, got Left " ++ show e
fromRight (Right x) = x
