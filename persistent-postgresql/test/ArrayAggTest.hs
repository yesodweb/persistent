{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ArrayAggTest where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.List (sort)
import qualified Data.Text as T
import Test.Hspec.Expectations ()

import PersistentTestModels
import PgInit

share [mkPersist persistSettings,  mkMigrate "jsonTestMigrate"] [persistLowerCase|
  TestValue
    json Value
|]

cleanDB :: (BaseBackend backend ~ SqlBackend, PersistQueryWrite backend, MonadIO m) => ReaderT backend m ()
cleanDB = deleteWhere ([] :: [Filter TestValue])

emptyArr :: Value
emptyArr = toJSON ([] :: [Value])

specs :: Spec
specs = do
  describe "rawSql/array_agg" $ do
    let runArrayAggTest :: (PersistField [a], Ord a, Show a) => Text -> [a] -> Assertion
        runArrayAggTest dbField expected = runConnAssert $ do
          void $ insertMany
            [ UserPT "a" $ Just "b"
            , UserPT "c" $ Just "d"
            , UserPT "e"   Nothing
            , UserPT "g" $ Just "h" ]
          escape <- ((. DBName) . connEscapeName) `fmap` ask
          let query = T.concat [ "SELECT array_agg(", escape dbField, ") "
                               , "FROM ", escape "UserPT"
                               ]
          [Single xs] <- rawSql query []
          liftIO $ sort xs @?= expected

    it "works for [Text]"       $ do
        runArrayAggTest "ident"    ["a", "c", "e", "g" :: Text]
    it "works for [Maybe Text]" $ do
        runArrayAggTest "password" [Nothing, Just "b", Just "d", Just "h" :: Maybe Text]
