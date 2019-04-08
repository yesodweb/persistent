{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ArrayAggTest where

import qualified Data.Text as T
import Data.List (sort)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import qualified Data.Vector as V (fromList)
import Test.Hspec.Expectations ()

import PersistentTestModels

import Database.Persist
import Database.Persist.Postgresql.JSON

import PgInit

share [mkPersist persistSettings,  mkMigrate "jsonTestMigrate"] [persistLowerCase|
  TestValue
    json Value
|]

cleanDB :: (BaseBackend backend ~ SqlBackend, PersistQueryWrite backend, MonadIO m) => ReaderT backend m ()
cleanDB = deleteWhere ([] :: [Filter TestValue])

emptyArr :: Value
emptyArr = toJSON ([] :: [Value])

specs :: RunDb SqlBackend IO -> Spec
specs runDb = do
  describe "rawSql/array_agg" $ do
    let runArrayAggTest :: (PersistField [a], Ord a, Show a) => Text -> [a] -> Assertion
        runArrayAggTest dbField expected = runDb $ do
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
