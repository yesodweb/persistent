{-# LANGUAGE CPP #-}
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
module LargeNumberTest where

import Init
import Data.Word

#ifdef WITH_NOSQL
mkPersist persistSettings { mpsGeneric = True } [persistUpperCase|
#else
share [mkPersist sqlSettings { mpsGeneric = True },  mkMigrate "numberMigrate"] [persistLowerCase|
#endif
  Number
    intx Int
    int32 Int32
    word32 Word32
    int64 Int64
    word64 Word64
    deriving Show Eq
|]

#ifdef WITH_NOSQL
cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Number ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Number])
db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = specsWith db

specsWith
    ::
    ( PersistStoreWrite backend
    , PersistStoreWrite (BaseBackend backend)
    , MonadIO m
    )
    => RunDb backend m
    -> Spec
specsWith runDb = describe "Large Numbers" $ do
  it "preserves their values in the database" $ runDb $ do
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

