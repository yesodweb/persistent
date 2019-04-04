{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module LargeNumberTestMongo where

import InitMongo
import Data.Word

mkPersist persistSettings [persistUpperCase|
  Number
    intx Int
    int32 Int32
    word32 Word32
    int64 Int64
    word64 Word64
    deriving Show Eq
|]

cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Number ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Number])
db :: Action IO () -> Assertion
db = db' cleanDB

specs :: Spec
specs = describe "persistent" $ do
  it "large numbers" $ db $ do
      let go x = do
              xid <- insert x
              x' <- get xid
              liftIO $ x' @?= Just x

      go $ Number maxBound 0 0 0 0
      go $ Number 0 maxBound 0 0 0
      go $ Number 0 0 maxBound 0 0
      go $ Number 0 0 0 maxBound 0
      go $ Number 0 0 0 0 maxBound

      go $ Number minBound 0 0 0 0
      go $ Number 0 minBound 0 0 0
      go $ Number 0 0 minBound 0 0
      go $ Number 0 0 0 minBound 0
      go $ Number 0 0 0 0 minBound
