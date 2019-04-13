{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module EmbedOrderTest (specsWith, embedOrderMigrate, cleanDB) where

import qualified Data.Map as Map
import Debug.Trace (trace)

import Init

debug :: Show s => s -> s
debug x = trace (show x) x

share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "embedOrderMigrate"] [persistUpperCase|
Foo sql=foo_embed_order
    bars [Bar]
    deriving Eq Show
Bar sql=bar_embed_order
    b String
    u String
    g String
    deriving Eq Show
|]

cleanDB :: Runner backend m => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter (FooGeneric backend)])
  deleteWhere ([] :: [Filter (BarGeneric backend)])

specsWith :: Runner backend m => RunDb backend m -> Spec
specsWith db = describe "embedded entities" $ do
    it "preserves ordering" $ db $ do
        let foo = Foo [Bar "b" "u" "g"]
        fooId <- insert foo
        Just otherFoo <- get fooId
        foo @== otherFoo

    it "PersistMap PersistValue serializaion" $ db $ do
        let record = Map.fromList [("b" :: Text,"b" :: Text),("u","u"),("g","g")]
        record @== (fromRight . fromPersistValue . toPersistValue) record

fromRight :: Show a => Either a b -> b
fromRight (Left e) = error $ "expected Right, got Left " ++ show e
fromRight (Right x) = x
